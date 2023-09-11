#include "dtls.h"

SSL_CTX *create_ctx(int dtls_srtp) {
  SSL_CTX *ssl_ctx = SSL_CTX_new(DTLS_method());
  if (ssl_ctx == NULL) {
    return NULL;
  }

  if (dtls_srtp == 1) {
    DEBUG("Setting SRTP extension");
    int res = SSL_CTX_set_tlsext_use_srtp(
        ssl_ctx, "SRTP_AES128_CM_SHA1_80:SRTP_AES128_CM_SHA1_32:SRTP_AEAD_AES_"
                 "128_GCM:SRTP_AEAD_AES_256_GCM");
    if (res != 0) {
      return NULL;
    }
  }

  return ssl_ctx;
}

SSL *create_ssl(SSL_CTX *ssl_ctx, int client_mode) {
  SSL *ssl = SSL_new(ssl_ctx);
  if (ssl == NULL) {
    return NULL;
  }

  if (client_mode) {
    SSL_set_connect_state(ssl);
  } else {
    SSL_set_accept_state(ssl);
  }

  BIO *rbio = BIO_new(BIO_s_mem());
  if (rbio == NULL) {
    DEBUG("Cannot create rbio");
    return NULL;
  }

  BIO *wbio = BIO_new(BIO_s_mem());
  if (wbio == NULL) {
    DEBUG("Cannot create wbio");
    return NULL;
  }

  SSL_set_bio(ssl, rbio, wbio);

  return ssl;
}

KeyingMaterial *export_keying_material(SSL *ssl) {
  SRTP_PROTECTION_PROFILE *profile = SSL_get_selected_srtp_profile(ssl);
  int master_key_len;
  int master_salt_len;
  switch (profile->id) {
  // Refer to RFC 3711 section 8.2
  case SRTP_AES128_CM_SHA1_80:
    master_key_len = 16;
    master_salt_len = 14;
    break;
  case SRTP_AES128_CM_SHA1_32:
    master_key_len = 16;
    master_salt_len = 14;
    break;
  // Refer to RFC 7741 section 12
  case SRTP_AEAD_AES_128_GCM:
    master_key_len = 16;
    master_salt_len = 12;
    break;
  case SRTP_AEAD_AES_256_GCM:
    master_key_len = 32;
    master_salt_len = 12;
    break;
  default:
    DEBUG("Unsupported SRTP protection profile\n");
    return NULL;
  }

  // Refer to RFC 5764 section 4.2
  int len = 2 * (master_key_len + master_salt_len);
  unsigned char *material = (unsigned char *)malloc(len * sizeof(char));
  memset(material, 0, len * sizeof(char));
  int res = SSL_export_keying_material(ssl, material, len,
                                       "EXTRACTOR-dtls_srtp", 19, NULL, 0, 0);
  if (res != 1) {
    free(material);
    return NULL;
  }

  KeyingMaterial *keying_material;
  keying_material = (KeyingMaterial *)malloc(sizeof(KeyingMaterial));
  memset(keying_material, 0, sizeof(KeyingMaterial));
  keying_material->client =
      (unsigned char *)malloc(len / 2 * sizeof(unsigned char));
  keying_material->server =
      (unsigned char *)malloc(len / 2 * sizeof(unsigned char));
  memset(keying_material->client, 0, len / 2 * sizeof(unsigned char));
  memset(keying_material->server, 0, len / 2 * sizeof(unsigned char));

  unsigned char *position = material;
  memcpy(keying_material->client, position, master_key_len);
  position += master_key_len;
  memcpy(keying_material->server, position, master_key_len);
  position += master_key_len;
  memcpy(keying_material->client + master_key_len, position, master_salt_len);
  position += master_salt_len;
  memcpy(keying_material->server + master_key_len, position, master_salt_len);
  position = NULL;
  keying_material->protection_profile = profile->id;
  keying_material->len = len / 2;

  free(material);
  return keying_material;
}

EVP_PKEY *gen_key() {
  // Based on https://www.openssl.org/docs/man1.1.1/man3/EVP_PKEY_keygen.html
  // and https://www.openssl.org/docs/man3.0/man7/EVP_PKEY-RSA.html
  EVP_PKEY *pkey = NULL;
  // TODO: possible optimization by storing and reusing keygen context
  EVP_PKEY_CTX *pctx = EVP_PKEY_CTX_new_id(EVP_PKEY_RSA, NULL);
  if (!pctx) {
    DEBUG("Cannot create EVP_PKEY_CTX");
    goto gen_key_exit;
  }

  if (EVP_PKEY_keygen_init(pctx) <= 0) {
    DEBUG("Cannot initialize keygen");
    goto gen_key_exit;
  }
  if (EVP_PKEY_CTX_set_rsa_keygen_bits(pctx, 2048) <= 0) {
    DEBUG("Cannot set bits");
    goto gen_key_exit;
  }
  if (EVP_PKEY_keygen(pctx, &pkey) <= 0 || !pkey) {
    DEBUG("Cannot create EVP_PKEY");
    goto gen_key_exit;
  }
gen_key_exit:
  if (pctx != NULL) {
    EVP_PKEY_CTX_free(pctx);
  }
  return pkey;
}

X509 *gen_cert(EVP_PKEY *pkey) {
  X509 *x509 = X509_new();
  if (x509 == NULL) {
    DEBUG("Cannot create X509");
    return NULL;
  }

  ASN1_INTEGER_set(X509_get_serialNumber(x509), 1);

  if (X509_gmtime_adj(X509_get_notBefore(x509), -31536000L) == 0 ||
      X509_gmtime_adj(X509_get_notAfter(x509), 31536000L) == 0) {
    DEBUG("Cannot set cert time validity");
    return NULL;
  }

  if (X509_set_pubkey(x509, pkey) == 0) {
    DEBUG("Cannot set cert pub key");
    return NULL;
  }

  X509_NAME *name = X509_get_subject_name(x509);
  if (X509_NAME_add_entry_by_txt(name, "C", MBSTRING_ASC, (unsigned char *)"PL",
                                 -1, -1, 0) == 0 ||
      X509_NAME_add_entry_by_txt(name, "O", MBSTRING_ASC,
                                 (unsigned char *)"ExDTLS", -1, -1, 0) == 0 ||
      X509_NAME_add_entry_by_txt(name, "CN", MBSTRING_ASC,
                                 (unsigned char *)"ExDTLS", -1, -1, 0) == 0) {
    DEBUG("Cannot set cert name");
    return NULL;
  }

  if (X509_set_issuer_name(x509, name) == 0) {
    DEBUG("Cannot set cert issuer name");
    return NULL;
  }

  if (X509_sign(x509, pkey, EVP_sha1()) == 0) {
    DEBUG("Cannot sign cert");
    return NULL;
  }

  return x509;
}

EVP_PKEY *decode_pkey(unsigned char *buf, int len) {
  const unsigned char *p;
  p = buf;
  return d2i_AutoPrivateKey(NULL, &p, len);
}

X509 *decode_cert(unsigned char *buf, int len) {
  const unsigned char *p;
  p = buf;
  return d2i_X509(NULL, &p, len);
}
