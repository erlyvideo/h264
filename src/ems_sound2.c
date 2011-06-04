#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "erl_nif.h"

#include "ems_mpg123.c"

// #include "ems_jpeg.c"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  if(!mpg123_resource) {
    mpg123_init();
    mpg123_resource = enif_open_resource_type(env, NULL, "mpg123_resource", mpg123_destructor, ERL_NIF_RT_CREATE, NULL);
  }

  return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv,
          ERL_NIF_TERM load_info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}



static ErlNifFunc ems_sound2_funcs[] =
{
    {"init_mpg123", 1, init_mpg123}
    // {"mpeg4_raw", 2, mpeg4_raw},
    // {"jpeg_rgb", 1, jpeg_rgb}
};

ERL_NIF_INIT(ems_sound2, ems_sound2_funcs, load, reload, upgrade, unload)
