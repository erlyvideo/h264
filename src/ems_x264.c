
#include <x264.h>

#include <libswscale/swscale.h>

ErlNifResourceType* x264_resource;

typedef struct {
  x264_param_t	param;
	x264_t			*encoder;

	x264_picture_t	picture;

	uint64_t		frame;
  uint64_t    dts_delta;
  uint64_t    base_pts;
  uint8_t     have_seen_pts;
	uint32_t		level;
  uint8_t     *yuv;
  uint32_t    width;
  uint32_t    height;
  struct SwsContext* convertCtx;


  uint32_t    bitrate;
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
  vfprintf(stderr, (const char *)(fmt || format), va);
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

static encoder_set_params(x264_param_t *param)
{
//	"cabac=yes",
	param->b_cabac = 1;
//	"bframes=0",
	param->i_bframe = 0;
//	"keyint=125",  -I, --keyint <integer>      Maximum GOP size [250]
	param->i_keyint_max = 125;
//	"ref=5",   -r, --ref <integer>         Number of reference frames [3]
	param->i_frame_reference = 5;
//	"mixed-refs=yes",
	param->analyse.b_mixed_references = 1;
//	"direct=auto", #define X264_DIRECT_PRED_AUTO   3
	param->analyse.i_direct_mv_pred = X264_DIRECT_PRED_AUTO;
//	"me=umh", #define X264_ME_UMH                  2
	param->analyse.i_me_method = X264_ME_UMH;
//	"merange=24",
	param->analyse.i_me_range = 24;
//	"subme=7",
	param->analyse.i_subpel_refine = 7;
//	"trellis=2",
	param->analyse.i_trellis = 2;
//	"weightb=yes",
	param->analyse.b_weighted_bipred = 1;
//	"partitions=all",
	param->analyse.inter = ~0;
//	"non-deterministic=yes",
	param->b_deterministic = 1;
//	"vbv-maxrate=512",
	param->rc.i_vbv_max_bitrate = 512;
//	"vbv-bufsize=9000",
	param->rc.i_vbv_buffer_size = 900;
//	"ratetol=1000.0",
	param->rc.f_rate_tolerance = 1000.0;
//	"scenecut=60"
	param->i_scenecut_threshold = 60;
// from x264.c profile selection
	param->analyse.b_transform_8x8 = 0;
	param->i_cqm_preset = X264_CQM_FLAT;
	
  param->i_csp = X264_CSP_I420;
  param->i_level_idc = -1;
  param->i_fps_num = 10;
  param->i_fps_den = 1;
  
  param->i_slice_max_size = 0;
  param->i_slice_max_mbs = 0;
  param->i_slice_count = 0;
  
  param->b_annexb = 1;
  param->pf_log = encoder_log;
}



static ERL_NIF_TERM
init_x264(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  X264 *x264;
  
  ERL_NIF_TERM opt, opts, *kv, decoder_config;
  int arity;
  
  unsigned int width = -1, height = -1, fps;
  int bitrate = -1;
  char preset[1024], tune[1024];
  
  ErlNifBinary encoder_config;
  int has_config = 0;
  
  
  strcpy(preset, "faster");
  strcpy(tune, "film");
  
  if (argc < 1 || !enif_is_list(env, argv[0])) {
    return enif_make_badarg(env);
  }
  
  fps = 25;
  
  x264 = (X264 *)enif_alloc_resource(x264_resource, sizeof(X264));
  memset(x264, 0, sizeof(X264));
  
  
  opts = argv[0];
  while(enif_get_list_cell(env, opts, &opt, &opts)) {
      if(enif_is_tuple(env, opt)) {
          enif_get_tuple(env, opt, &arity, (const ERL_NIF_TERM **)&kv);
          if(arity < 2) continue;
          if(!enif_compare(kv[0], enif_make_atom(env, "bitrate"))) {
              enif_get_int(env, kv[1], &x264->bitrate); 
          }
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
            enif_get_int(env, kv[1], &width);
          }

          if(!enif_compare(kv[0], enif_make_atom(env, "height"))) {
            enif_get_int(env, kv[1], &height);
          }

          if(!enif_compare(kv[0], enif_make_atom(env, "config"))) {
            if(enif_inspect_binary(env, kv[1], &encoder_config)) {
              has_config = 1;
            }
          }
      }
  }
  
  
  x264_param_default(&x264->param);
  x264->param.i_width = width;
  x264->param.i_height = height;
  x264->param.i_sps_id = 1;
  
  x264->width = width;
  x264->height = height;
  
  encoder_set_params(&x264->param);
  
  x264_param_default_preset(&x264->param, x264->preset, x264->tune);
  x264->param.i_width = width;
  x264->param.i_height = height;
  // x264->param.rc.i_vbv_buffer_size = 10;
  x264->param.b_annexb = 0;
  
  if(has_config) {
    char *key, *value;
    int i;
    char *ptr = (char *)malloc(encoder_config.size+1);
    memcpy(ptr, encoder_config.data, encoder_config.size);
    ptr[encoder_config.size] = 0;
    while(*ptr) {
      key = ptr;
      for(i = 0; ptr[i]; i++) {
        if(ptr[i] == ' ') {
          ptr[i] = 0;
          i++;
          value = ptr+i;
          break;
        }
      }
      if(!ptr[i]) break;
      
      for(;ptr[i];i++) {
        if(ptr[i] == '\r' && ptr[i+1] == '\n') {
          ptr[i] = 0;
          ptr += i+2;
        }
        if(ptr[i] == '\n') {
          ptr[i] = 0;
          ptr += i+1;
        }
      }
      
      // fprintf(stderr, "X264: %s => %s\r\n", key, value);
      x264_param_parse(&x264->param, key, value);
    }
  }
  
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
  return enif_make_tuple3(env, enif_make_atom(env, "ok"), enif_make_resource(env, x264), enif_make_binary(env, &config));
}



static ERL_NIF_TERM
yuv_x264(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  X264 *x264;
  ErlNifBinary yuv, out, h264;
  ErlNifSInt64 pts;
  
  if(!enif_get_resource(env, argv[0], x264_resource, (void **)&x264)) {
    return enif_make_badarg(env);
  }

  if(!enif_inspect_binary(env, argv[1], &yuv)) {
    return enif_make_badarg(env);
  }

  if(!enif_get_int64(env, argv[2], &pts)) {
    fprintf(stderr, "Invalid PTS\r\n");
    return enif_make_badarg(env);
  }
  
  if(!x264->have_seen_pts) {
    x264->have_seen_pts = 1;
    x264->base_pts = pts;
  }
  
  pts -= x264->base_pts;
  
  uint32_t plane_size = x264->width*x264->height;
  if(plane_size*3/2 != yuv.size) {
    fprintf(stderr, "Invalid YUV size: %lu, %u\r\n", yuv.size, plane_size*3/2);
    return enif_make_badarg(env);
  }
  
  x264->picture.img.plane[0] = yuv.data;
  x264->picture.img.i_stride[0] = x264->width;
  
  x264->picture.img.plane[1] = yuv.data+plane_size;
  x264->picture.img.i_stride[1] = x264->width/2;
  
  x264->picture.img.plane[2] = yuv.data + plane_size*5/4;
  x264->picture.img.i_stride[2] = x264->width/2;
  
  x264->picture.i_dts = pts;
	x264->picture.i_pts = pts;
	x264->picture.i_type = X264_TYPE_AUTO;
	x264->picture.i_qpplus1 = 0;

  x264_nal_t* nals;
	int				count, i;
  x264_picture_t	output;
  int len = 0;
  
  len = x264_encoder_encode(x264->encoder, &nals, &count, &x264->picture, &output);
  // fprintf(stderr, "H264: %llu %lld\r\n", output.i_dts, output.i_pts - output.i_dts);
  if(len == -1) {
    return enif_make_badarg(env);
  }
  
  if(len == 0) {
    return enif_make_atom(env, "ok");
  }
  
	enif_alloc_binary(len, &h264);
  len = 0;
  
  for(i = 0; i < count; i++) {
    memcpy(h264.data + len, nals[i].p_payload, nals[i].i_payload);
    len += nals[i].i_payload;
  }
  
  return enif_make_tuple5(env, 
    enif_make_atom(env, "ok"), 
    enif_make_atom(env, output.b_keyframe ? "keyframe" : "frame"), 
    enif_make_int64(env, output.i_dts+x264->base_pts), 
    enif_make_int64(env, output.i_pts+x264->base_pts), 
    enif_make_binary(env, &h264));
}

