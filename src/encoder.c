#include "encoder.h"
#include <stdio.h>
#include <string.h>

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

Encoder encoder_init(uint32_t width, uint32_t height, Encoder state)
{
  int in_w, in_h, out_w, out_h;

  if(!state) {
    state = encoder_new();
  }
  
  x264_param_default(&state->param);
  state->param.i_width = width;
  state->param.i_height = height;
  state->param.i_sps_id = 1;
  
  state->width = width;
  state->height = height;
  
  encoder_set_params(&state->param);
  
  x264_param_default_preset(&state->param, "faster", "zerolatency");
  state->param.i_width = width;
  state->param.i_height = height;
  state->param.i_threads = 4;
  state->param.b_sliced_threads = 1;
  state->param.i_frame_reference = 1;
  state->param.i_keyint_max = 20;
  state->param.i_bframe = 0;
  state->param.rc.i_vbv_buffer_size = 10;
  //	"ratetol=1000.0",
  state->param.b_annexb = 0;
  // state->param.
  // state->param.
  // state->param.
  // state->param.
  // state->param.
  
  state->encoder = x264_encoder_open(&state->param);
  
  if (x264_picture_alloc(&state->picture, X264_CSP_I420, width, height) < 0) {
    printf("Couldn't allocate picture for %ux%u\n", width, height);
	}
	
  
  in_w = out_w = width;
  in_h = out_h = height;
  state->convertCtx = sws_getContext(in_w, in_h, PIX_FMT_RGB24, out_w, out_h, PIX_FMT_YUV420P, SWS_FAST_BILINEAR, NULL, NULL, NULL);
	
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


Data encoder_config(Encoder state)
{
  x264_nal_t* nals;
  int count, i;
  Data data = {0, 0};
  
  x264_encoder_headers(state->encoder, &nals, &count);
  for(i = 0; i < count; i++) {
    data = concat_data(data, nals[i].p_payload, nals[i].i_payload);
  }
  return data;
}


Data concat_data(Data data, uint8_t *buf, size_t size)
{
  data.data = realloc(data.data, data.size + size);
  memmove(data.data + data.size, buf, size);
  data.size += size;
  return data;
}

Data encoder_encode(Encoder state, uint8_t *rgb)
{
  x264_picture_t	output;
	int				count, i;
  Data      data = {0, 0};
  x264_nal_t* nals;
  uint8_t *src[3] = {rgb, NULL, NULL};
  int srcstride[3] = {state->width*3, 0, 0};
  
  int plane = state->height*state->width;
  uint8_t *yuv = malloc(plane*3);
  memset(yuv, 127, plane*3);

  state->picture.img.plane[0] = yuv;
  state->picture.img.i_stride[0] = state->width;
  
  state->picture.img.plane[1] = yuv + plane;
  state->picture.img.i_stride[1] = state->width;
  
  state->picture.img.plane[2] = yuv + 2*plane;
  state->picture.img.i_stride[2] = state->width;
  
  sws_scale(state->convertCtx, src, srcstride, 0, state->height, state->picture.img.plane, state->picture.img.i_stride);
  
	state->picture.i_pts = (int64_t)state->frame * state->param.i_fps_den;
	state->picture.i_type = X264_TYPE_AUTO;
	state->picture.i_qpplus1 = 0;
	if (x264_encoder_encode(state->encoder, &nals, &count, &state->picture, &output) < 0) {
    return;
  }
  
	for (i = 0; i < count; i++) {
    data = concat_data(data, nals[i].p_payload, nals[i].i_payload);
	}

	state->frame++;
	
  free(yuv);
  return data;
}