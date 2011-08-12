#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "erl_nif.h"

#include "ems_x264.c"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  if(!x264_resource) {
    x264_resource = enif_open_resource_type(env, NULL, "x264_resource", x264_destructor, ERL_NIF_RT_CREATE, NULL);
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



static ErlNifFunc ems_video_funcs[] =
{
    {"real_init_x264", 1, init_x264},
    {"real_yuv_x264", 3, yuv_x264}
};

ERL_NIF_INIT(ems_video, ems_video_funcs, load, reload, upgrade, unload)
