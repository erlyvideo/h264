
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "erl_nif.h"


#define _GNU_SOURCE             /* See feature_test_macros(7) */
#define __USE_GNU
#include <sys/types.h>
#include <linux/unistd.h>
#include <sys/syscall.h>
#include <sched.h>
pid_t gettid() { return syscall( __NR_gettid );}


#include <x264.h>

#include <libswscale/swscale.h>

ErlNifResourceType* x264_resource;

void *h264_encode(void *);


typedef struct {
  x264_param_t	param;
  x264_t			*encoder;

  x264_picture_t	picture;

  uint64_t		frame;
  uint64_t    dts_delta;
  uint64_t    base_pts;
  uint8_t     have_seen_pts;
  uint32_t		level;
  uint32_t    width;
  uint32_t    height;
  struct SwsContext* convertCtx;
  ErlNifTid   tid;
  ErlNifMutex*        lock;
  ErlNifCond*         cond;
  ErlNifPid owner_pid;
  
  int          has_yuv;
  ERL_NIF_TERM yuv;
  ErlNifSInt64 pts;
  
  ErlNifEnv*  env;
  
  
  
  char        preset[256];
  char        tune[256];
} X264;

static void encoder_log(void *private, int i_level, const char *format, va_list va)
{
  int len = strlen(format);
  char *fmt = NULL;
  if(format[len - 1] == '\n' && format[len-2] != '\r') {
    fmt = malloc(len + 2);
    strcpy(fmt, format);
    fmt[len-1] = '\r';
    fmt[len] = '\n';
    fmt[len+1] = 0;
  }
  //vfprintf(stderr, (const char *)(fmt || format), va);
  vfprintf(stderr, format, va);
  if(fmt) free(fmt);
}


static void
x264_destructor(ErlNifEnv* env, void* obj)
{
  X264 *x264;
  x264 = obj;
  
  x264_encoder_close(x264->encoder);
  x264_picture_clean(&x264->picture);
}



static ERL_NIF_TERM
init_x264(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  X264 *x264;
  
  ERL_NIF_TERM opt, opts, *kv;
  int arity;
  
  unsigned int width = -1, height = -1, fps;
  
  ErlNifBinary encoder_config;
  int has_config = 0;
  
  
  if (argc < 1 || !enif_is_list(env, argv[0])) {
    return enif_make_badarg(env);
  }
  
  fps = 25;
  
  x264 = (X264 *)enif_alloc_resource(x264_resource, sizeof(X264));
  memset(x264, 0, sizeof(X264));
  strcpy(x264->preset, "faster");
  strcpy(x264->tune, "film");
  
  
  
  opts = argv[0];
  while(enif_get_list_cell(env, opts, &opt, &opts)) {
      if(enif_is_tuple(env, opt)) {
          enif_get_tuple(env, opt, &arity, (const ERL_NIF_TERM **)&kv);
          if(arity < 2) continue;
          if(!enif_compare(kv[0], enif_make_atom(env, "preset"))) {
              if(enif_is_list(env, kv[1])) {
                  enif_get_string(env, kv[1], x264->preset, sizeof(x264->preset), ERL_NIF_LATIN1);
              }
          }
          if(!enif_compare(kv[0], enif_make_atom(env, "tune"))) {
              if(enif_is_list(env, kv[1])) {
                  enif_get_string(env, kv[1], x264->tune, sizeof(x264->tune), ERL_NIF_LATIN1);
              }
          }

          if(!enif_compare(kv[0], enif_make_atom(env, "width"))) {
            enif_get_uint(env, kv[1], &width);
          }

          if(!enif_compare(kv[0], enif_make_atom(env, "height"))) {
            enif_get_uint(env, kv[1], &height);
          }

          if(!enif_compare(kv[0], enif_make_atom(env, "config"))) {
            if(enif_inspect_binary(env, kv[1], &encoder_config)) {
              has_config = 1;
            } else {
              return enif_make_badarg(env);
            }
          }
      }
  }
  
  
  x264_param_default_preset(&x264->param, x264->preset, x264->tune);
  x264->param.i_width = width;
  x264->param.i_height = height;
  x264->param.i_sps_id = 1;
  x264->param.b_annexb = 0;
  
  x264->param.i_csp = X264_CSP_I420;
  x264->param.i_level_idc = -1;
  x264->param.pf_log = encoder_log;
  x264->width = width;
  x264->height = height;
  //x264_param_default(&x264->param);
  
  
  if(has_config) {
    char *key, *value;
    char *config_copy, *ptr;
    ptr = config_copy = (char *)malloc(encoder_config.size+5);
    memcpy(ptr, encoder_config.data, encoder_config.size);
    ptr[encoder_config.size] = '\n';
    ptr[encoder_config.size+1] = 0;
    while(*ptr) {
      key = ptr;

      while(*ptr && isspace(*ptr)) ptr++; 
      if(!*ptr) break;
      value = NULL;

      for(;*ptr;) {
        ptr++;
        if(*ptr == ' ') {
          *ptr = 0;
          ptr++;
          value = ptr;
          break;
        }
      }
      
      
      while(*ptr && isspace(*ptr)) ptr++; 
      if(!*ptr) break;

      for(;*ptr;) {
        ptr++;
        if(*ptr == '\n' || *ptr == '\r') {
          *ptr = 0;
          ptr++;
          break;
        }
      }
 
      if(key[0] == '#') continue;

      int ret;
      if(!strcmp(key, "profile"))
        ret = x264_param_apply_profile(&x264->param, value);
      else
        ret = x264_param_parse(&x264->param, key, value);
      if(ret) {
        fprintf(stderr, "invalid param: %s = %s\n", key, value);
        exit(1);
      } else {
        fprintf(stderr, "set %s = %s\r\n", key, value);
      }
    }
  }
  
  fprintf(stderr, "X264 threads: %d\r\n", x264->param.i_threads);
  
  x264->encoder = x264_encoder_open(&x264->param);
  
  if (x264_picture_alloc(&x264->picture, X264_CSP_I420, width, height) < 0) {
    printf("Couldn't allocate picture for %ux%u\n", width, height);
	}
	
  int in_w, out_w, in_h, out_h;
  
  in_w = out_w = width;
  in_h = out_h = height;
  // x264->convertCtx = sws_getContext(in_w, in_h, PIX_FMT_RGB24, out_w, out_h, PIX_FMT_YUV420P, SWS_FAST_BILINEAR, NULL, NULL, NULL);
  

  x264_nal_t* nals;
  int count, i, len;
  ErlNifBinary config;
  
  
  len = x264_encoder_headers(x264->encoder, &nals, &count);
  enif_alloc_binary(len, &config);
  len = 0;
  
  for(i = 0; i < count; i++) {
    memcpy(config.data + len, nals[i].p_payload, nals[i].i_payload);
    len += nals[i].i_payload;
  }
  
  
  x264->lock = enif_mutex_create("x264-lock");
  x264->cond = enif_cond_create("x264-cond");
  x264->has_yuv = 0;
  enif_self(env, &x264->owner_pid);
  enif_thread_create("x264-thread", &x264->tid, h264_encode, x264, NULL);
  return enif_make_tuple3(env, enif_make_atom(env, "ok"), enif_make_resource(env, x264), enif_make_binary(env, &config));
}




static ERL_NIF_TERM
yuv_x264(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  X264 *x264;
  ErlNifBinary yuv;
  
  if(!enif_get_resource(env, argv[0], x264_resource, (void **)&x264)) {
    return enif_make_badarg(env);
  }
  
  if(x264->has_yuv) {
    return enif_make_atom(env, "busy");
  }
  
  enif_mutex_lock(x264->lock);
  
  if(!enif_is_binary(env, argv[1])) {
    return enif_make_badarg(env);
  }

  if(!enif_get_int64(env, argv[2], &x264->pts)) {
    fprintf(stderr, "Invalid PTS\r\n");
    return enif_make_badarg(env);
  }
  
  x264->env = enif_alloc_env();
  x264->yuv = enif_make_copy(x264->env, argv[1]);
  x264->has_yuv = 1;
  enif_cond_signal(x264->cond);
  enif_mutex_unlock(x264->lock);
  return enif_make_atom(env, "wait");
}  
 

void run_h264_encode(X264 *x264);
 
void *h264_encode(void *arg) {
  X264 *x264 = (X264 *)arg;
  
  cpu_set_t p_aff;
  memset( &p_aff, 0, sizeof(p_aff) );
  CPU_SET(0, &p_aff);
  sched_setaffinity(0, sizeof(p_aff), &p_aff );
  
  int i;
  for(i = 0; i <= 4; i++) {
    memset( &p_aff, 0, sizeof(p_aff) );
    CPU_SET(i, &p_aff);
    sched_setaffinity(gettid() - 2 - i, sizeof(p_aff), &p_aff );
  }
  
  enif_keep_resource(x264);
  
  while(1) {
    run_h264_encode(x264);
  }
}

void run_h264_encode(X264 *x264) {
  
  while(!x264->has_yuv) {
    enif_cond_wait(x264->cond, x264->lock);
  }

  ErlNifBinary h264, yuv;
  
  enif_inspect_binary(x264->env, x264->yuv, &yuv);
  
  
  // if(!x264->have_seen_pts) {
  //   x264->have_seen_pts = 1;
  //   // x264->base_pts = x264->pts;
  //   x264->base_pts = 0;
  // }
  
  // x264->pts -= x264->base_pts;
  
  uint32_t plane_size = x264->width*x264->height;
  if(plane_size*3/2 != yuv.size) {
    fprintf(stderr, "Invalid YUV size: %lu, %u\r\n", yuv.size, plane_size*3/2);
    return;
    // return enif_make_badarg(env);
  }
  
  x264->picture.img.plane[0] = yuv.data;
  x264->picture.img.i_stride[0] = x264->width;
  
  x264->picture.img.plane[1] = yuv.data+plane_size;
  x264->picture.img.i_stride[1] = x264->width/2;
  
  x264->picture.img.plane[2] = yuv.data + plane_size*5/4;
  x264->picture.img.i_stride[2] = x264->width/2;
  
  x264->picture.i_dts = x264->pts;
	x264->picture.i_pts = x264->pts;
	x264->picture.i_type = X264_TYPE_AUTO;
	x264->picture.i_qpplus1 = 0;

  x264_nal_t* nals;
	int				count, i;
  x264_picture_t	output;
  int len = 0;
  
  len = x264_encoder_encode(x264->encoder, &nals, &count, &x264->picture, &output);
  // fprintf(stderr, "H264: %llu %lld\r\n", output.i_dts, output.i_pts - output.i_dts);
  
  enif_free_env(x264->env);
  x264->has_yuv = 0;
  
  if(len == -1) {
    // return enif_make_badarg(env);
    return;
  }
  
  
  ErlNifEnv* env = enif_alloc_env();
  ERL_NIF_TERM message;
  
  if(len == 0) {
    // return enif_make_atom(env, "ok");
    message = enif_make_atom(env, "ok");
  } else {
  	enif_alloc_binary(len, &h264);
    len = 0;

    for(i = 0; i < count; i++) {
      memcpy(h264.data + len, nals[i].p_payload, nals[i].i_payload);
      len += nals[i].i_payload;
    }
    
    // if(!x264->have_seen_pts) {
    //   x264->have_seen_pts = 1;
    //   fprintf(stderr, "First output: %ld, %ld, %ld\r\n", output.i_dts, output.i_pts, x264->base_pts);
    // }
    

    if(output.i_dts < 0 && !x264->base_pts) {
      x264->base_pts = output.i_pts - output.i_dts;
      fprintf(stderr, "Correcting output pts for: %d\r\n", x264->base_pts);
    }

    message = enif_make_tuple4(env, 
      enif_make_atom(env, output.b_keyframe ? "keyframe" : "frame"), 
      enif_make_int64(env, output.i_dts+x264->base_pts), 
      enif_make_int64(env, output.i_pts+x264->base_pts), 
      enif_make_binary(env, &h264));
    
  }
  
  
  enif_send(NULL, &x264->owner_pid, env, 
    enif_make_tuple3(env, 
      enif_make_atom(env, "ok"),
      enif_make_resource(env, x264),
      message));
  enif_release_binary(&h264);
  enif_free_env(env);
  
}

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



static ErlNifFunc x264_funcs[] =
{
    {"real_init_x264", 1, init_x264},
    {"real_yuv_x264", 3, yuv_x264}
};

ERL_NIF_INIT(x264, x264_funcs, load, reload, upgrade, unload)
