#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "erl_nif.h"

#define HAS_X264 1
#define HAS_MPEG2 1

#ifdef HAS_XVID
#include "ems_xvid.c"
#endif /* HAS_XVID */

#ifdef HAS_X264
#include "ems_x264.c"
#endif /* HAS_X264 */

#ifdef HAS_MPEG2
#include "ems_mpeg2.c"
#endif /* HAS_MPEG2 */

// #include "ems_jpeg.c"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  #ifdef HAS_XVID
  if(!xvid_resource) {
    xvid_resource = enif_open_resource_type(env, NULL, "xvid_resource", xvid_destructor, ERL_NIF_RT_CREATE, NULL);
  }
  #endif

  #ifdef HAS_X264
  if(!x264_resource) {
    x264_resource = enif_open_resource_type(env, NULL, "x264_resource", x264_destructor, ERL_NIF_RT_CREATE, NULL);
  }
  #endif

  #ifdef HAS_MPEG2
  if(!mpeg2_resource) {
    mpeg2_resource = enif_open_resource_type(env, NULL, "mpeg2_resource", mpeg2_destructor, ERL_NIF_RT_CREATE, NULL);
  }
  #endif

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
#ifdef HAS_XVID
    {"init_xvid", 0, init_xvid},
    {"mpeg4_raw", 2, mpeg4_raw},
#endif /* HAS_XVID */
#ifdef HAS_X264
    {"init_x264", 1, init_x264},
    {"real_yuv_x264", 4, yuv_x264},
#endif /* HAS_X264 */
#ifdef HAS_MPEG2
    {"init_mpeg2", 0, init_mpeg2},
    {"real_mpeg2_raw", 2, mpeg2_raw},
    {"mpeg2_getopt", 2, mpeg2_getopt}
#endif /* HAS_MPEG2 */
    // {"jpeg_rgb", 1, jpeg_rgb}
};

ERL_NIF_INIT(ems_video, ems_video_funcs, load, reload, upgrade, unload)
