#include "encoder.h"
#include <stdio.h>

Encoder encoder_new() 
{
  return calloc(sizeof(ems_encoder), 1);
}


static void encoder_log(void *private, int i_level, const char *format, va_list va)
{
  vprintf(format, va);
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
	param->rc.i_vbv_buffer_size = 9000;
//	"ratetol=1000.0",
	param->rc.f_rate_tolerance = 1000.0;
//	"scenecut=60"
	param->i_scenecut_threshold = 60;
// from x264.c profile selection
	param->analyse.b_transform_8x8 = 0;
	param->i_cqm_preset = X264_CQM_FLAT;
	
  param->i_csp = X264_CSP_I420;
  param->i_level_idc = -1;
  param->i_fps_num = 24000;
  param->i_fps_den = 1001;
  
  param->i_slice_max_size = 0;
  param->i_slice_max_mbs = 0;
  param->i_slice_count = 0;
  
  param->b_annexb = 1;
  param->pf_log = encoder_log;
}

Encoder encoder_init(Encoder state)
{
  uint32_t width, height;
  if(!state) {
    state = encoder_new();
  }
  
  width = 848;
  height = 480;
  
  x264_param_default(&state->param);
  state->param.i_width = width;
  state->param.i_height = height;
  state->param.i_sps_id = 1;
  
  encoder_set_params(&state->param);
  printf("Hi1\n");
  
  state->encoder = x264_encoder_open(&state->param);
  printf("Hi2\n");
  
  x264_picture_clean(&state->picture);
  if (x264_picture_alloc(&state->picture, X264_CSP_I420, width, height) < 0) {
    printf("Couldn't allocate picture for %ux%u\n", width, height);
	}
	
  return state;
}

void encoder_free(Encoder state)
{
  encoder_close(state);
  free(state);
}

void encoder_close(Encoder state)
{
  x264_encoder_close(state->encoder);
  x264_picture_clean(&state->picture);
}





