/*
 * Copyright (C) 2002-2021 ProcessOne, SARL. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include <stdio.h>
#include <string.h>
#include <erl_nif.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdint.h>
#include <ctype.h>
#include "options.h"
#include "uthash.h"
#include "ioqueue.h"

#define BUF_SIZE 1024

#define enif_alloc malloc
#define enif_free free
#define enif_realloc realloc

typedef struct {
    BIO *bio_read;
    BIO *bio_write;
    SSL *ssl;
    int handshakes;
    ErlNifMutex *mtx;
    int valid;
    ioqueue *to_send_queue;
    char *cert_file;
    char *ciphers;
    char *dh_file;
    char *ca_file;
    long options;
    char *sni_error;
    long command;
} state_t;

static int ssl_index;

#ifdef _WIN32
typedef unsigned __int32 uint32_t;
#endif

#ifndef SSL_OP_NO_TICKET
#define SSL_OP_NO_TICKET 0
#endif

#if OPENSSL_VERSION_NUMBER < 0x10100000L || defined LIBRESSL_VERSION_NUMBER
#define DH_set0_pqg(dh, dh_p, param, dh_g) (dh)->p = dh_p; (dh)->g = dh_g
#endif

#if OPENSSL_VERSION_NUMBER < 0x10002000L
#define SSL_is_server(s) (s)->server
#endif

void __free(void *ptr, size_t size) {
    enif_free(ptr);
}

#undef uthash_malloc
#undef uthash_free
#define uthash_malloc enif_alloc
#define uthash_free __free

#if OPENSSL_VERSION_NUMBER >= 0x10100000L || OPENSSL_VERSION_NUMBER < 0x10002000
#undef SSL_CTX_set_ecdh_auto
#define SSL_CTX_set_ecdh_auto(A, B) do {} while(0)
#endif

#define CIPHERS "HIGH:!aNULL:!eNULL:!3DES:@STRENGTH"
#define PROTOCOL_OPTIONS "no_sslv3|cipher_server_preference|no_compression"

static ErlNifResourceType *tls_state_t = NULL;
static ErlNifMutex **mtx_buf = NULL;

/**
 * Prepare the SSL options flag.
 **/
static int set_option_flag(const unsigned char *opt, size_t len, long *flag) {
    ssl_option_t *p;
    for (p = ssl_options; p->name; p++) {
        if (!memcmp(opt, p->name, len) && p->name[len] == '\0') {
            *flag |= p->code;
            return 1;
        }
    }
    return 0;
}

typedef struct {
    char *key;
    char *file;
    SSL_CTX *ssl_ctx;
    UT_hash_handle hh;
} cert_info_t;

static cert_info_t *certs_map = NULL;
static cert_info_t *certfiles_map = NULL;

static ErlNifRWLock *certs_map_lock = NULL;
static ErlNifRWLock *certfiles_map_lock = NULL;

static void free_cert_info(cert_info_t *info) {
    if (info) {
        enif_free(info->key);
        enif_free(info->file);
        if (info->ssl_ctx)
            SSL_CTX_free(info->ssl_ctx);
        enif_free(info);
    }
}

static void clear_certs_map() {
    cert_info_t *info = NULL;
    cert_info_t *tmp = NULL;

    enif_rwlock_rwlock(certs_map_lock);
    HASH_ITER(hh, certs_map, info, tmp) {
        HASH_DEL(certs_map, info);
        free_cert_info(info);
    }
    enif_rwlock_rwunlock(certs_map_lock);
}

static void clear_certfiles_map() {
    cert_info_t *info = NULL;
    cert_info_t *tmp = NULL;

    enif_rwlock_rwlock(certfiles_map_lock);
    HASH_ITER(hh, certfiles_map, info, tmp) {
        HASH_DEL(certfiles_map, info);
        free_cert_info(info);
    }
    enif_rwlock_rwunlock(certfiles_map_lock);
}

static state_t *init_tls_state() {
    state_t *state = enif_alloc_resource(tls_state_t, sizeof(state_t));
    if (!state) return NULL;
    memset(state, 0, sizeof(state_t));
    state->mtx = enif_mutex_create("");
    if (!state->mtx) {
        enif_release_resource(state);
        return NULL;
    }
    state->to_send_queue = ioqueue_create();
    if (!state->to_send_queue) {
        enif_release_resource(state);
        enif_mutex_destroy(state->mtx);
        return NULL;
    }
    state->valid = 1;
    return state;
}

static void destroy_tls_state(ErlNifEnv *env, void *data) {
    state_t *state = (state_t *) data;
    if (state) {
        if (state->ssl)
            SSL_free(state->ssl);
        if (state->mtx)
            enif_mutex_destroy(state->mtx);
        if (state->cert_file)
            enif_free(state->cert_file);
        if (state->to_send_queue)
            ioqueue_free(state->to_send_queue);
        memset(state, 0, sizeof(state_t));
    }
}

#if OPENSSL_VERSION_NUMBER < 0x10100000L || defined LIBRESSL_VERSION_NUMBER

static void locking_callback(int mode, int n, const char *file, int line) {
    if (mode & CRYPTO_LOCK)
        enif_mutex_lock(mtx_buf[n]);
    else
        enif_mutex_unlock(mtx_buf[n]);
}

static void thread_id_callback(CRYPTO_THREADID *id) {
    CRYPTO_THREADID_set_pointer(id, enif_thread_self());
}

static int atomic_add_callback(int *pointer, int amount, int type, const char *file, int line) {
    return __sync_add_and_fetch(pointer, amount);
}

#endif

static int load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info) {
    int i;

    OpenSSL_add_ssl_algorithms();
    SSL_load_error_strings();

    mtx_buf = enif_alloc(CRYPTO_num_locks() * sizeof(ErlNifMutex *));
    for (i = 0; i < CRYPTO_num_locks(); i++)
        mtx_buf[i] = enif_mutex_create("");

    CRYPTO_set_add_lock_callback(atomic_add_callback);
    CRYPTO_set_locking_callback(locking_callback);
    CRYPTO_THREADID_set_callback(thread_id_callback);

    certs_map_lock = enif_rwlock_create("certs_map_lock");
    certfiles_map_lock = enif_rwlock_create("certfiles_map_lock");

    ssl_index = SSL_get_ex_new_index(0, "ssl index", NULL, NULL, NULL);
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    tls_state_t = enif_open_resource_type(env, NULL, "tls_state_t",
                                          destroy_tls_state,
                                          flags, NULL);
    return 0;
}

static void unload(ErlNifEnv *env, void *priv) {
    int i;

    clear_certs_map();
    clear_certfiles_map();
    enif_rwlock_destroy(certs_map_lock);
    enif_rwlock_destroy(certfiles_map_lock);
    certs_map = NULL;
    certs_map_lock = NULL;
    certfiles_map = NULL;
    certfiles_map_lock = NULL;
    for (i = 0; i < CRYPTO_num_locks(); i++)
        enif_mutex_destroy(mtx_buf[i]);
    enif_free(mtx_buf);
    mtx_buf = NULL;
}

static int verify_callback(int preverify_ok, X509_STORE_CTX *ctx) {
    return 1;
}

/*
 * ECDHE is enabled only on OpenSSL 1.0.0e and later.
 * See http://www.openssl.org/news/secadv_20110906.txt
 * for details.
 */
#ifndef OPENSSL_NO_ECDH

static void setup_ecdh(SSL_CTX *ctx) {
#if OPENSSL_VERSION_NUMBER < 0x10002000
    EC_KEY *ecdh;

    if (SSLeay() < 0x1000005fL) {
        return;
    }

    ecdh = EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
    SSL_CTX_set_options(ctx, SSL_OP_SINGLE_ECDH_USE);
    SSL_CTX_set_tmp_ecdh(ctx, ecdh);

    EC_KEY_free(ecdh);
#else
    SSL_CTX_set_ecdh_auto(ctx, 1);
#endif
}

#endif

#ifndef OPENSSL_NO_DH
/*
  2048-bit MODP Group with 256-bit Prime Order Subgroup (RFC5114)
*/
static unsigned char dh2048_p[] = {
        0x87, 0xA8, 0xE6, 0x1D, 0xB4, 0xB6, 0x66, 0x3C,
        0xFF, 0xBB, 0xD1, 0x9C, 0x65, 0x19, 0x59, 0x99,
        0x8C, 0xEE, 0xF6, 0x08, 0x66, 0x0D, 0xD0, 0xF2,
        0x5D, 0x2C, 0xEE, 0xD4, 0x43, 0x5E, 0x3B, 0x00,
        0xE0, 0x0D, 0xF8, 0xF1, 0xD6, 0x19, 0x57, 0xD4,
        0xFA, 0xF7, 0xDF, 0x45, 0x61, 0xB2, 0xAA, 0x30,
        0x16, 0xC3, 0xD9, 0x11, 0x34, 0x09, 0x6F, 0xAA,
        0x3B, 0xF4, 0x29, 0x6D, 0x83, 0x0E, 0x9A, 0x7C,
        0x20, 0x9E, 0x0C, 0x64, 0x97, 0x51, 0x7A, 0xBD,
        0x5A, 0x8A, 0x9D, 0x30, 0x6B, 0xCF, 0x67, 0xED,
        0x91, 0xF9, 0xE6, 0x72, 0x5B, 0x47, 0x58, 0xC0,
        0x22, 0xE0, 0xB1, 0xEF, 0x42, 0x75, 0xBF, 0x7B,
        0x6C, 0x5B, 0xFC, 0x11, 0xD4, 0x5F, 0x90, 0x88,
        0xB9, 0x41, 0xF5, 0x4E, 0xB1, 0xE5, 0x9B, 0xB8,
        0xBC, 0x39, 0xA0, 0xBF, 0x12, 0x30, 0x7F, 0x5C,
        0x4F, 0xDB, 0x70, 0xC5, 0x81, 0xB2, 0x3F, 0x76,
        0xB6, 0x3A, 0xCA, 0xE1, 0xCA, 0xA6, 0xB7, 0x90,
        0x2D, 0x52, 0x52, 0x67, 0x35, 0x48, 0x8A, 0x0E,
        0xF1, 0x3C, 0x6D, 0x9A, 0x51, 0xBF, 0xA4, 0xAB,
        0x3A, 0xD8, 0x34, 0x77, 0x96, 0x52, 0x4D, 0x8E,
        0xF6, 0xA1, 0x67, 0xB5, 0xA4, 0x18, 0x25, 0xD9,
        0x67, 0xE1, 0x44, 0xE5, 0x14, 0x05, 0x64, 0x25,
        0x1C, 0xCA, 0xCB, 0x83, 0xE6, 0xB4, 0x86, 0xF6,
        0xB3, 0xCA, 0x3F, 0x79, 0x71, 0x50, 0x60, 0x26,
        0xC0, 0xB8, 0x57, 0xF6, 0x89, 0x96, 0x28, 0x56,
        0xDE, 0xD4, 0x01, 0x0A, 0xBD, 0x0B, 0xE6, 0x21,
        0xC3, 0xA3, 0x96, 0x0A, 0x54, 0xE7, 0x10, 0xC3,
        0x75, 0xF2, 0x63, 0x75, 0xD7, 0x01, 0x41, 0x03,
        0xA4, 0xB5, 0x43, 0x30, 0xC1, 0x98, 0xAF, 0x12,
        0x61, 0x16, 0xD2, 0x27, 0x6E, 0x11, 0x71, 0x5F,
        0x69, 0x38, 0x77, 0xFA, 0xD7, 0xEF, 0x09, 0xCA,
        0xDB, 0x09, 0x4A, 0xE9, 0x1E, 0x1A, 0x15, 0x97,
};
static unsigned char dh2048_g[] = {
        0x3F, 0xB3, 0x2C, 0x9B, 0x73, 0x13, 0x4D, 0x0B,
        0x2E, 0x77, 0x50, 0x66, 0x60, 0xED, 0xBD, 0x48,
        0x4C, 0xA7, 0xB1, 0x8F, 0x21, 0xEF, 0x20, 0x54,
        0x07, 0xF4, 0x79, 0x3A, 0x1A, 0x0B, 0xA1, 0x25,
        0x10, 0xDB, 0xC1, 0x50, 0x77, 0xBE, 0x46, 0x3F,
        0xFF, 0x4F, 0xED, 0x4A, 0xAC, 0x0B, 0xB5, 0x55,
        0xBE, 0x3A, 0x6C, 0x1B, 0x0C, 0x6B, 0x47, 0xB1,
        0xBC, 0x37, 0x73, 0xBF, 0x7E, 0x8C, 0x6F, 0x62,
        0x90, 0x12, 0x28, 0xF8, 0xC2, 0x8C, 0xBB, 0x18,
        0xA5, 0x5A, 0xE3, 0x13, 0x41, 0x00, 0x0A, 0x65,
        0x01, 0x96, 0xF9, 0x31, 0xC7, 0x7A, 0x57, 0xF2,
        0xDD, 0xF4, 0x63, 0xE5, 0xE9, 0xEC, 0x14, 0x4B,
        0x77, 0x7D, 0xE6, 0x2A, 0xAA, 0xB8, 0xA8, 0x62,
        0x8A, 0xC3, 0x76, 0xD2, 0x82, 0xD6, 0xED, 0x38,
        0x64, 0xE6, 0x79, 0x82, 0x42, 0x8E, 0xBC, 0x83,
        0x1D, 0x14, 0x34, 0x8F, 0x6F, 0x2F, 0x91, 0x93,
        0xB5, 0x04, 0x5A, 0xF2, 0x76, 0x71, 0x64, 0xE1,
        0xDF, 0xC9, 0x67, 0xC1, 0xFB, 0x3F, 0x2E, 0x55,
        0xA4, 0xBD, 0x1B, 0xFF, 0xE8, 0x3B, 0x9C, 0x80,
        0xD0, 0x52, 0xB9, 0x85, 0xD1, 0x82, 0xEA, 0x0A,
        0xDB, 0x2A, 0x3B, 0x73, 0x13, 0xD3, 0xFE, 0x14,
        0xC8, 0x48, 0x4B, 0x1E, 0x05, 0x25, 0x88, 0xB9,
        0xB7, 0xD2, 0xBB, 0xD2, 0xDF, 0x01, 0x61, 0x99,
        0xEC, 0xD0, 0x6E, 0x15, 0x57, 0xCD, 0x09, 0x15,
        0xB3, 0x35, 0x3B, 0xBB, 0x64, 0xE0, 0xEC, 0x37,
        0x7F, 0xD0, 0x28, 0x37, 0x0D, 0xF9, 0x2B, 0x52,
        0xC7, 0x89, 0x14, 0x28, 0xCD, 0xC6, 0x7E, 0xB6,
        0x18, 0x4B, 0x52, 0x3D, 0x1D, 0xB2, 0x46, 0xC3,
        0x2F, 0x63, 0x07, 0x84, 0x90, 0xF0, 0x0E, 0xF8,
        0xD6, 0x47, 0xD1, 0x48, 0xD4, 0x79, 0x54, 0x51,
        0x5E, 0x23, 0x27, 0xCF, 0xEF, 0x98, 0xC5, 0x82,
        0x66, 0x4B, 0x4C, 0x0F, 0x6C, 0xC4, 0x16, 0x59,
};

static int setup_dh(SSL_CTX *ctx, char *dh_file) {
    DH *dh;
    int res;

    if (dh_file != NULL) {
        BIO *bio = BIO_new_file(dh_file, "r");

        if (bio == NULL) {
            return 0;
        }
        dh = PEM_read_bio_DHparams(bio, NULL, NULL, NULL);
        BIO_free(bio);
        if (dh == NULL) {
            return 0;
        }
    } else {
        dh = DH_new();
        if (dh == NULL) {
            return 0;
        }
        BIGNUM *dh_p = BN_bin2bn(dh2048_p, sizeof(dh2048_p), NULL);
        BIGNUM *dh_g = BN_bin2bn(dh2048_g, sizeof(dh2048_g), NULL);
        if (dh_p == NULL || dh_g == NULL) {
            BN_free(dh_p);
            BN_free(dh_g);
            DH_free(dh);
            return 0;
        }

        DH_set0_pqg(dh, dh_p, NULL, dh_g);
    }

    SSL_CTX_set_options(ctx, SSL_OP_SINGLE_DH_USE);
    res = (int) SSL_CTX_set_tmp_dh(ctx, dh);

    DH_free(dh);
    return res;
}

#endif

#ifndef SSL_OP_NO_RENEGOTIATION
static void ssl_info_callback(const SSL *s, int where, int ret) {
    state_t *d = (state_t *) SSL_get_ex_data(s, ssl_index);
    if ((where & SSL_CB_HANDSHAKE_START)) {
        d->handshakes++;
    }
}
#endif

static char *create_ssl_for_cert(char *, state_t *);

static cert_info_t *lookup_certfile(const char *domain) {
    cert_info_t *ret = NULL;
    cert_info_t *info = NULL;

    if (domain) {
        size_t len = strlen(domain);
        if (len) {
            char name[len + 1];
            name[len] = 0;
            size_t i = 0;
            for (i = 0; i < len; i++)
                name[i] = tolower(domain[i]);
            HASH_FIND_STR(certfiles_map, name, info);
            if (info && info->file)
                ret = info;
            else {
                /* Replace the first domain part with '*' and retry */
                char *dot = strchr(name, '.');
                if (dot != NULL && name[0] != '.') {
                    char *glob = dot - 1;
                    glob[0] = '*';
                    HASH_FIND_STR(certfiles_map, glob, info);
                    if (info && info->file)
                        ret = info;
                }
            }
        }
    }
    return ret;
}

static int ssl_sni_callback(const SSL *s, int *foo, void *data) {
    cert_info_t *info = NULL;
    char *err_str = NULL;
    const char *servername = NULL;
    int ret = SSL_TLSEXT_ERR_OK;
    state_t *state = (state_t *) SSL_get_ex_data(s, ssl_index);

    servername = SSL_get_servername(s, TLSEXT_NAMETYPE_host_name);
    enif_rwlock_rlock(certfiles_map_lock);
    info = lookup_certfile(servername);
    if (info) {
        if (strcmp(info->file, state->cert_file))
            err_str = create_ssl_for_cert(info->file, state);
        if (err_str) {
            state->sni_error = err_str;
            ret = SSL_TLSEXT_ERR_ALERT_FATAL;
        }
    } else if (strlen(state->cert_file) == 0) {
        state->sni_error =
                "Failed to find a certificate matching the domain in SNI extension";
        ret = SSL_TLSEXT_ERR_ALERT_FATAL;
    }
    enif_rwlock_runlock(certfiles_map_lock);

    return ret;
}

#define ERR_T(T) enif_make_tuple2(env, enif_make_atom(env, "error"), T)
#define OK_T(T) enif_make_tuple2(env, enif_make_atom(env, "ok"), T)

#define SET_CERTIFICATE_FILE_ACCEPT 1
#define SET_CERTIFICATE_FILE_CONNECT 2
#define VERIFY_NONE 0x10000
#define COMPRESSION_NONE 0x100000

static ERL_NIF_TERM ssl_error(ErlNifEnv *env, const char *errstr) {
    size_t rlen;
    ErlNifBinary err;
    char error_string[256];
    size_t error_string_length;
    size_t errstrlen = strlen(errstr);
    unsigned long error_code = ERR_get_error();

    if (error_code) {
        ERR_error_string_n(error_code, error_string, sizeof(error_string));
        error_string_length = strlen(error_string),
        rlen = errstrlen + error_string_length + 2;
    } else {
        error_string_length = 0;
        rlen = errstrlen;
    }
    enif_alloc_binary(rlen, &err);
    memcpy(err.data, errstr, errstrlen);
    if (error_code) {
        memcpy(err.data + errstrlen, ": ", 2);
        memcpy(err.data + 2 + errstrlen, error_string, error_string_length);
    }
    return ERR_T(enif_make_binary(env, &err));
}

static SSL_CTX *create_new_ctx(char *cert_file, char *ciphers,
                               char *dh_file, char *ca_file,
                               unsigned int command,
                               char **err_str) {
    long verifyopts;
    int res = 0;

    SSL_CTX *ctx = SSL_CTX_new(SSLv23_method());
    if (!ctx) {
        *err_str = "SSL_CTX_new failed";
        return NULL;
    }
    if (cert_file) {
        res = SSL_CTX_use_certificate_chain_file(ctx, cert_file);
        if (res <= 0) {
            SSL_CTX_free(ctx);
            *err_str = "SSL_CTX_use_certificate_file failed";
            return NULL;
        }
        res = SSL_CTX_use_PrivateKey_file(ctx, cert_file, SSL_FILETYPE_PEM);
        if (res <= 0) {
            SSL_CTX_free(ctx);
            *err_str = "SSL_CTX_use_PrivateKey_file failed";
            return NULL;
        }
        res = SSL_CTX_check_private_key(ctx);
        if (res <= 0) {
            SSL_CTX_free(ctx);
            *err_str = "SSL_CTX_check_private_key failed";
            return NULL;
        }
    }

    if (command == SET_CERTIFICATE_FILE_ACCEPT) {
        SSL_CTX_set_tlsext_servername_callback(ctx, &ssl_sni_callback);
        verifyopts = SSL_VERIFY_PEER | SSL_VERIFY_CLIENT_ONCE;
        if (ca_file) {
            SSL_CTX_set_client_CA_list(ctx, SSL_load_client_CA_file(ca_file));
        }
    } else {
        verifyopts = SSL_VERIFY_PEER;
    }

    if (ciphers[0] == 0)
        SSL_CTX_set_cipher_list(ctx, CIPHERS);
    else
        SSL_CTX_set_cipher_list(ctx, ciphers);

#ifndef OPENSSL_NO_ECDH
    setup_ecdh(ctx);
#endif
#ifndef OPENSSL_NO_DH
    res = setup_dh(ctx, dh_file);
    if (res <= 0) {
        SSL_CTX_free(ctx);
        *err_str = "Setting DH parameters failed";
        return NULL;
    }
#endif

    SSL_CTX_set_session_cache_mode(ctx, SSL_SESS_CACHE_OFF);
    if (ca_file)
        SSL_CTX_load_verify_locations(ctx, ca_file, NULL);
    else
        SSL_CTX_set_default_verify_paths(ctx);

#ifdef SSL_MODE_RELEASE_BUFFERS
    SSL_CTX_set_mode(ctx, SSL_MODE_RELEASE_BUFFERS);
#endif
    SSL_CTX_set_verify(ctx, verifyopts, verify_callback);

#ifndef SSL_OP_NO_RENEGOTIATION
    SSL_CTX_set_info_callback(ctx, &ssl_info_callback);
#endif

    *err_str = NULL;
    return ctx;
}

static void set_ctx(state_t *state, SSL_CTX *ctx) {
    if (state->ssl)
        SSL_set_SSL_CTX(state->ssl, ctx);
    else
        state->ssl = SSL_new(ctx);
}

static char *create_ssl_for_cert(char *cert_file, state_t *state) {
    char *ciphers = state->ciphers;
    char *dh_file = state->dh_file;
    char *ca_file = state->ca_file;
    long options = state->options;
    unsigned int command = state->command;

    char *ret = NULL;
    cert_info_t *info = NULL;
    cert_info_t *new_info = NULL;
    cert_info_t *old_info = NULL;
    size_t key_size =
            strlen(cert_file) + strlen(ciphers) + 8 +
            strlen(dh_file) + strlen(ca_file) + 1;
    char key[key_size];
    sprintf(key, "%s%s%08lx%s%s", cert_file, ciphers,
            options, dh_file, ca_file);

    enif_rwlock_rlock(certs_map_lock);

    HASH_FIND_STR(certs_map, key, info);

    if (strlen(cert_file) == 0) cert_file = NULL;
    if (strlen(dh_file) == 0) dh_file = NULL;
    if (strlen(ca_file) == 0) ca_file = NULL;

    if (info == NULL) {
        enif_rwlock_runlock(certs_map_lock);

        enif_rwlock_rwlock(certs_map_lock);
        SSL_CTX *ctx = create_new_ctx(cert_file, ciphers, dh_file, ca_file, command, &ret);
        if (ret == NULL) {
            new_info = enif_alloc(sizeof(cert_info_t));
            if (new_info) {
                memset(new_info, 0, sizeof(cert_info_t));
                new_info->key = enif_alloc(key_size);
                if (new_info->key) {
                    memcpy(new_info->key, key, key_size);
                    new_info->ssl_ctx = ctx;
                    HASH_REPLACE_STR(certs_map, key, new_info, old_info);
                    free_cert_info(old_info);
                    set_ctx(state, ctx);
                } else {
                    enif_free(new_info);
                    SSL_CTX_free(ctx);
                    ret = "Memory allocation failed";
                }
            } else {
                SSL_CTX_free(ctx);
                ret = "Memory allocation failed";
            }
        }
        enif_rwlock_rwunlock(certs_map_lock);
    } else {
        set_ctx(state, info->ssl_ctx);
        enif_rwlock_runlock(certs_map_lock);
    }
    return ret;
}

static ERL_NIF_TERM open_nif(ErlNifEnv *env, int argc,
                             const ERL_NIF_TERM argv[]) {
    unsigned int command;
    unsigned int flags;
    char *sni = NULL;
    ErlNifBinary ciphers_bin;
    ErlNifBinary certfile_bin;
    ErlNifBinary protocol_options_bin;
    ErlNifBinary dhfile_bin;
    ErlNifBinary cafile_bin;
    ErlNifBinary sni_bin;
    ErlNifBinary alpn_bin;
    long options = 0L;
    state_t *state = NULL;
    size_t po_len_left = 0;
    unsigned char *po = NULL;

    ERR_clear_error();

    if (argc != 8)
        return enif_make_badarg(env);

    if (!enif_get_uint(env, argv[0], &flags))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[1], &certfile_bin))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[2], &ciphers_bin))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[3], &protocol_options_bin))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[4], &dhfile_bin))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[5], &cafile_bin))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[6], &sni_bin))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[7], &alpn_bin))
        return enif_make_badarg(env);

    command = flags & 0xffff;
    if (protocol_options_bin.size) {
        po_len_left = protocol_options_bin.size;
        po = protocol_options_bin.data;
    } else {
        po = (unsigned char *) PROTOCOL_OPTIONS;
        po_len_left = strlen((char *) po);
    }

    while (po_len_left) {
        unsigned char *pos = memchr(po, '|', po_len_left);

        if (!pos) {
            set_option_flag(po, po_len_left, &options);
            break;
        }
        set_option_flag(po, pos - po, &options);
        po_len_left -= pos - po + 1;
        po = pos + 1;
    }

    state = init_tls_state();
    if (!state) return ERR_T(enif_make_atom(env, "enomem"));

    state->cert_file = enif_alloc(certfile_bin.size + 1 +
                                  ciphers_bin.size + 1 +
                                  dhfile_bin.size + 1 +
                                  cafile_bin.size + 1 +
                                  sni_bin.size + 1);
    if (!state->cert_file) {
        enif_release_resource(state);
        return ERR_T(enif_make_atom(env, "enomem"));
    }
    state->ciphers = state->cert_file + certfile_bin.size + 1;
    state->dh_file = state->ciphers + ciphers_bin.size + 1;
    state->ca_file = state->dh_file + dhfile_bin.size + 1;
    sni = state->ca_file + cafile_bin.size + 1;
    state->options = options;
    state->command = command;

    memcpy(state->cert_file, certfile_bin.data, certfile_bin.size);
    state->cert_file[certfile_bin.size] = 0;
    memcpy(state->ciphers, ciphers_bin.data, ciphers_bin.size);
    state->ciphers[ciphers_bin.size] = 0;
    memcpy(state->dh_file, dhfile_bin.data, dhfile_bin.size);
    state->dh_file[dhfile_bin.size] = 0;
    memcpy(state->ca_file, cafile_bin.data, cafile_bin.size);
    state->ca_file[cafile_bin.size] = 0;
    memcpy(sni, sni_bin.data, sni_bin.size);
    sni[sni_bin.size] = 0;

    char *err_str = create_ssl_for_cert(state->cert_file, state);
    if (err_str) {
        enif_release_resource(state);
        return ssl_error(env, err_str);
    }

    if (!state->ssl) {
        enif_release_resource(state);
        return ssl_error(env, "SSL_new failed");
    }

    if (flags & VERIFY_NONE)
        SSL_set_verify(state->ssl, SSL_VERIFY_NONE, verify_callback);

#ifdef SSL_OP_NO_COMPRESSION
    if (flags & COMPRESSION_NONE)
        SSL_set_options(state->ssl, SSL_OP_NO_COMPRESSION);
#endif

    SSL_set_ex_data(state->ssl, ssl_index, state);

    state->bio_read = BIO_new(BIO_s_mem());
    state->bio_write = BIO_new(BIO_s_mem());

    SSL_set_bio(state->ssl, state->bio_read, state->bio_write);

    if (command == SET_CERTIFICATE_FILE_ACCEPT) {
        options |= (SSL_OP_NO_TICKET | SSL_OP_ALL | SSL_OP_NO_SSLv2);

        SSL_set_options(state->ssl, options);

        SSL_set_accept_state(state->ssl);
    } else {
        options |= (SSL_OP_NO_TICKET | SSL_OP_NO_SSLv2);

        SSL_set_options(state->ssl, options);

        if (strlen(sni) > 0) SSL_set_tlsext_host_name(state->ssl, sni);

#if OPENSSL_VERSION_NUMBER >= 0x10002000L
        if (alpn_bin.size)
            SSL_set_alpn_protos(state->ssl, alpn_bin.data, alpn_bin.size);
#endif

        SSL_set_connect_state(state->ssl);
    }

#ifdef SSL_OP_NO_RENEGOTIATION
    SSL_set_options(state->ssl, SSL_OP_NO_RENEGOTIATION);
#endif

    ERL_NIF_TERM result = enif_make_resource(env, state);
    enif_release_resource(state);
    return OK_T(result);
}

static ERL_NIF_TERM
get_data_to_write(ErlNifEnv *env, state_t* state) {
    ERL_NIF_TERM data;
    size_t size = BIO_ctrl_pending(state->bio_write);

    unsigned char *buf = enif_make_new_binary(env, size, &data);
    BIO_read(state->bio_write, buf, (int)size);

    return data;
}

static int
get_decrypted_data(ErlNifEnv *env, state_t* state, int bytes_to_read, ERL_NIF_TERM *ret) {
    ErlNifBinary buf;
    size_t pos = 0;
    int res;

    if (bytes_to_read == 0) {
        enif_make_new_binary(env, 0, ret);
        return 1;
    }

    if (bytes_to_read < 0 || bytes_to_read > BUF_SIZE) {
        res = enif_alloc_binary(BUF_SIZE, &buf);
    } else {
        res = enif_alloc_binary((size_t)bytes_to_read, &buf);
    }

    if (!res) {
        *ret = ERR_T(enif_make_atom(env, "enomem"));
        return 2;
    }

    while ((res = SSL_read(state->ssl, buf.data + pos, (int)(buf.size - pos))) > 0) {
        pos += res;
        if (pos == bytes_to_read)
            break;
        if (buf.size - pos < BUF_SIZE && buf.size != bytes_to_read) {
            size_t new_size = bytes_to_read > 0 && buf.size * 2 > bytes_to_read ?
                    bytes_to_read : buf.size * 2;
            if (!enif_realloc_binary(&buf, new_size)) {
                *ret = ERR_T(enif_make_atom(env, "enomem"));
                return 2;
            }
        }
    }
    enif_realloc_binary(&buf, pos);
    *ret = enif_make_binary(env, &buf);
    return 1;
}

static ERL_NIF_TERM
return_read_write(ErlNifEnv *env, state_t* state, int bytes_to_read) {
    ERL_NIF_TERM read;
    if (get_decrypted_data(env, state, bytes_to_read, &read) == 2) {
        enif_mutex_unlock(state->mtx);
        return read;
    }
    ERL_NIF_TERM write = get_data_to_write(env, state);

    enif_mutex_unlock(state->mtx);

    return enif_make_tuple2(env, write, read);
}

static int
do_recv(ErlNifEnv *env, state_t *state, ERL_NIF_TERM *err, ErlNifBinary *recv) {
    int res;

    if (recv->size == 0)
        return 1;

    res = BIO_write(state->bio_read, recv->data, (int)recv->size);
    if (res <= 0) {
        enif_mutex_unlock(state->mtx);
        *err = ERR_T(enif_make_atom(env, "write_failed"));
        return 2;
    }
    return 1;
}

static int
do_send(ErlNifEnv *env, state_t *state, ERL_NIF_TERM *err, ErlNifBinary *to_send) {
    int res = 1;

    if (state->to_send_queue->size > 0) {
        res = SSL_write(state->ssl, state->to_send_queue->buf,
                        (int) state->to_send_queue->size);
        if (res > 0) {
            ioqueue_consume(state->to_send_queue, (size_t) res);
        }
    }
    if (to_send->size) {
        int consumed = 0;
        if (res > 0 && state->to_send_queue->size == 0) {
            res = SSL_write(state->ssl, to_send->data, (int) to_send->size);
            consumed = res > 0 ? res : 0;
        }
        if (consumed < to_send->size) {
            if (!ioqueue_append(state->to_send_queue, (char *) to_send->data + consumed,
                                to_send->size - consumed)) {
                enif_mutex_unlock(state->mtx);
                *err = ERR_T(enif_make_atom(env, "enomem"));
                return 2;
            }
        }
    }

    return res > 0 ? 1 : res;
}

static int
do_send_queue(ErlNifEnv *env, state_t *state, ERL_NIF_TERM *err, ErlNifBinary *to_send) {
    int res = 1;

    if (to_send->size) {
        if (!ioqueue_append(state->to_send_queue, (char *) to_send->data,
                            to_send->size)) {
            enif_mutex_unlock(state->mtx);
            *err = ERR_T(enif_make_atom(env, "enomem"));
            return 2;
        }
    }

    return res > 0 ? 1 : res;
}

static ERL_NIF_TERM
loop_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    state_t *state = NULL;
    ErlNifBinary to_send;
    ErlNifBinary received;
    int bytes_to_read;

    if (argc != 4)
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], tls_state_t, (void *) &state))
        return enif_make_badarg(env);

    if (!enif_inspect_iolist_as_binary(env, argv[1], &to_send))
        return enif_make_badarg(env);

    if (!enif_inspect_iolist_as_binary(env, argv[2], &received))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[3], &bytes_to_read))
        return enif_make_badarg(env);

    if (!state->mtx || !state->ssl)
        return enif_make_badarg(env);

    enif_mutex_lock(state->mtx);

    if (!state->valid) {
        enif_mutex_unlock(state->mtx);
        return ERR_T(enif_make_atom(env, "closed"));
    }

    ERR_clear_error();

    int res;
    ERL_NIF_TERM err_term;

    res = do_recv(env, state, &err_term, &received);
    if (res == 2) {
        return err_term;
    }

    if (!SSL_is_init_finished(state->ssl)) {
        res = SSL_do_handshake(state->ssl);
        if (res <= 0) {
            res = SSL_get_error(state->ssl, res);
            if (res == SSL_ERROR_WANT_READ || res == SSL_ERROR_WANT_WRITE) {
                res = do_send_queue(env, state, &err_term, &to_send);
                if (res == 2) {
                    return err_term;
                }
                return return_read_write(env, state, bytes_to_read);
            } else {
                enif_mutex_unlock(state->mtx);
                int reason = ERR_GET_REASON(ERR_peek_error());
                if (reason == SSL_R_DATA_LENGTH_TOO_LONG ||
                    reason == SSL_R_PACKET_LENGTH_TOO_LONG ||
                    reason == SSL_R_UNKNOWN_PROTOCOL ||
                    reason == SSL_R_UNEXPECTED_MESSAGE ||
                    reason == SSL_R_WRONG_VERSION_NUMBER ||
		    reason == SSL_R_HTTP_REQUEST ||
		    reason == SSL_R_HTTPS_PROXY_REQUEST)
                    /* Do not report badly formed Client Hello */
                    return ERR_T(enif_make_atom(env, "closed"));
                else if (state->sni_error)
                    return ssl_error(env, state->sni_error);
                else
                    return ssl_error(env, "SSL_do_handshake failed");
            }
        }
        if (!SSL_is_init_finished(state->ssl)) {
            res = do_send_queue(env, state, &err_term, &to_send);
            if (res == 2) {
                return err_term;
            }
            return return_read_write(env, state, bytes_to_read);
        }
    }

    res = do_send(env, state, &err_term, &to_send);
    if (res == 2) {
        return err_term;
    }

    if (res <= 0)
        res = SSL_get_error(state->ssl, res);
    return return_read_write(env, state, bytes_to_read);
}

static ERL_NIF_TERM get_verify_result_nif(ErlNifEnv *env, int argc,
                                          const ERL_NIF_TERM argv[]) {
    long res;
    state_t *state = NULL;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], tls_state_t, (void *) &state))
        return enif_make_badarg(env);

    if (!state->mtx || !state->ssl) return enif_make_badarg(env);
    enif_mutex_lock(state->mtx);

    if (!state->valid) {
        enif_mutex_unlock(state->mtx);
        return ERR_T(enif_make_atom(env, "closed"));
    }

    ERR_clear_error();
    res = SSL_get_verify_result(state->ssl);
    enif_mutex_unlock(state->mtx);
    return OK_T(enif_make_long(env, res));
}

static ERL_NIF_TERM get_peer_certificate_nif(ErlNifEnv *env, int argc,
                                             const ERL_NIF_TERM argv[]) {
    X509 *cert = NULL;
    state_t *state = NULL;
    int rlen;
    ErlNifBinary output;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], tls_state_t, (void *) &state))
        return enif_make_badarg(env);

    if (!state->mtx || !state->ssl) return enif_make_badarg(env);
    enif_mutex_lock(state->mtx);

    if (!state->valid) {
        enif_mutex_unlock(state->mtx);
        return ERR_T(enif_make_atom(env, "closed"));
    }

    ERR_clear_error();

    cert = SSL_get_peer_certificate(state->ssl);
    if (!cert) {
        enif_mutex_unlock(state->mtx);
        return ssl_error(env, "SSL_get_peer_certificate failed");
    }
    rlen = i2d_X509(cert, NULL);
    if (rlen >= 0) {
        if (!enif_alloc_binary(rlen, &output)) {
            enif_mutex_unlock(state->mtx);
            return ERR_T(enif_make_atom(env, "enomem"));
        }
        i2d_X509(cert, &output.data);
        X509_free(cert);
        enif_mutex_unlock(state->mtx);
        return OK_T(enif_make_binary(env, &output));
    } else {
        X509_free(cert);
        enif_mutex_unlock(state->mtx);
        return ERR_T(enif_make_atom(env, "notfound"));
    }
}

static ERL_NIF_TERM add_certfile_nif(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[]) {
    ErlNifBinary domain, file;
    cert_info_t *info = NULL;
    cert_info_t *old_info = NULL;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &domain))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[1], &file))
        return enif_make_badarg(env);

    info = enif_alloc(sizeof(cert_info_t));
    if (info) {
        memset(info, 0, sizeof(cert_info_t));
        info->key = enif_alloc(domain.size + 1);
        info->file = enif_alloc(file.size + 1);
        if (info->key && info->file) {
            memcpy(info->key, domain.data, domain.size);
            memcpy(info->file, file.data, file.size);
            info->key[domain.size] = 0;
            info->file[file.size] = 0;
            enif_rwlock_rwlock(certfiles_map_lock);
            HASH_REPLACE_STR(certfiles_map, key, info, old_info);
            free_cert_info(old_info);
            enif_rwlock_rwunlock(certfiles_map_lock);
        } else {
            free_cert_info(info);
        }
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM delete_certfile_nif(ErlNifEnv *env, int argc,
                                        const ERL_NIF_TERM argv[]) {
    ErlNifBinary domain;
    char *ret = "false";
    cert_info_t *info = NULL;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &domain))
        return enif_make_badarg(env);

    char key[domain.size + 1];
    memcpy(key, domain.data, domain.size);
    key[domain.size] = 0;
    enif_rwlock_rwlock(certfiles_map_lock);
    HASH_FIND_STR(certfiles_map, key, info);
    if (info) {
        HASH_DEL(certfiles_map, info);
        free_cert_info(info);
        ret = "true";
    }
    enif_rwlock_rwunlock(certfiles_map_lock);

    return enif_make_atom(env, ret);
}

static ERL_NIF_TERM get_certfile_nif(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[]) {
    ErlNifBinary domain;
    cert_info_t *info = NULL;
    ERL_NIF_TERM file, result;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &domain))
        return enif_make_badarg(env);

    char key[domain.size + 1];
    memcpy(key, domain.data, domain.size);
    key[domain.size] = 0;
    enif_rwlock_rlock(certfiles_map_lock);
    info = lookup_certfile(key);
    if (info) {
        unsigned char *tmp = enif_make_new_binary(env, strlen(info->file), &file);
        if (tmp) {
            memcpy(tmp, info->file, strlen(info->file));
            result = enif_make_tuple2(env, enif_make_atom(env, "ok"), file);
        } else
            result = enif_make_atom(env, "error");
    } else {
        result = enif_make_atom(env, "error");
    }
    enif_rwlock_runlock(certfiles_map_lock);

    return result;
}

static ERL_NIF_TERM clear_cache_nif(ErlNifEnv *env, int argc,
                                    const ERL_NIF_TERM argv[]) {
    clear_certs_map();
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM invalidate_nif(ErlNifEnv *env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    state_t *state = NULL;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], tls_state_t, (void *) &state))
        return enif_make_badarg(env);

    if (!state->mtx || !state->ssl) return enif_make_badarg(env);

    enif_mutex_lock(state->mtx);
    state->valid = 0;
    enif_mutex_unlock(state->mtx);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM get_negotiated_cipher_nif(ErlNifEnv *env, int argc,
                                              const ERL_NIF_TERM argv[]) {
    state_t *state = NULL;

    if (argc != 1)
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], tls_state_t, (void *) &state))
        return enif_make_badarg(env);

    if (!state->mtx || !state->ssl) return enif_make_badarg(env);

    enif_mutex_lock(state->mtx);

    if (!state->valid) {
        enif_mutex_unlock(state->mtx);
        return ERR_T(enif_make_atom(env, "closed"));
    }

    const char *version = SSL_get_version(state->ssl);
    const char *cipher = SSL_get_cipher_name(state->ssl);
    enif_mutex_unlock(state->mtx);

    ErlNifBinary bin;
    size_t vl = strlen(version);
    size_t cl = strlen(cipher);
    if (!enif_alloc_binary(vl + cl + 1, &bin)) {
        return ERR_T(enif_make_atom(env, "enomem"));
    }
    memcpy(bin.data, version, vl);
    bin.data[vl] = ' ';
    memcpy(bin.data + vl + 1, cipher, cl);
    return enif_make_binary(env, &bin);
}

static ERL_NIF_TERM tls_get_peer_finished_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    state_t *state = NULL;
    if (!enif_get_resource(env, argv[0], tls_state_t, (void *) &state))
        return enif_make_badarg(env);

	/* OpenSSL does not offer an API to directly get the length of the
	 * expected TLS Finished message, so just do a dummy call to grab this
	 * information to allow caller to do an allocation with a correct size.
	 */
    ERL_NIF_TERM bin;
    size_t len = SSL_get_peer_finished(state->ssl, NULL, 0);
    if (len == 0)
        return ERR_T(enif_make_atom(env, "undefined"));
    unsigned char *buf = enif_make_new_binary(env, len, &bin);
    if (!buf)
        return ERR_T(enif_make_atom(env, "enomem"));

    (void) SSL_get_peer_finished(state->ssl, buf, len);
    return OK_T(bin);
}

static ERL_NIF_TERM tls_get_finished_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    state_t *state = NULL;
    if (!enif_get_resource(env, argv[0], tls_state_t, (void *) &state))
        return enif_make_badarg(env);

    ERL_NIF_TERM bin;
    size_t len = SSL_get_finished(state->ssl, NULL, 0);
    if (len == 0)
        return ERR_T(enif_make_atom(env, "undefined"));
    unsigned char *buf = enif_make_new_binary(env, len, &bin);
    if (!buf)
        return ERR_T(enif_make_atom(env, "enomem"));

    (void) SSL_get_finished(state->ssl, buf, len);
    return OK_T(bin);
}

static ErlNifFunc nif_funcs[] =
        {
                {"open_nif",                  8, open_nif},
                {"loop_nif",                  4, loop_nif},
                {"get_verify_result_nif",     1, get_verify_result_nif},
                {"get_peer_certificate_nif",  1, get_peer_certificate_nif},
                {"add_certfile_nif",          2, add_certfile_nif},
                {"delete_certfile_nif",       1, delete_certfile_nif},
                {"get_certfile_nif",          1, get_certfile_nif},
                {"clear_cache_nif",           0, clear_cache_nif},
                {"invalidate_nif",            1, invalidate_nif},
                {"get_negotiated_cipher_nif", 1, get_negotiated_cipher_nif},
                {"tls_get_peer_finished_nif", 1, tls_get_peer_finished_nif},
                {"tls_get_finished_nif",      1, tls_get_finished_nif}
        };

ERL_NIF_INIT(fast_tls, nif_funcs, load, NULL, NULL, unload)
