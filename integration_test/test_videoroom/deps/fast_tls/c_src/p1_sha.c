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

#include <erl_nif.h>
#include <stdio.h>

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data,
		   void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM to_hexlist(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    ErlNifBinary in;
    ErlNifBinary out;
    int res, i;

    if (argc == 1) {
	if (enif_inspect_iolist_as_binary(env, argv[0], &in)) {
	    res = enif_alloc_binary(2*in.size, &out);
	    if (res) {
		for (i = 0; i<in.size; i++) {
		    sprintf((char *) (out.data + 2*i), "%02x", in.data[i]);
		}
		return enif_make_binary(env, &out);
	    }
	}
    }

    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] =
    {
	{"to_hexlist", 1, to_hexlist}
    };

ERL_NIF_INIT(p1_sha, nif_funcs, load, NULL, upgrade, NULL)
