#include "encoder.h"
#include <stdio.h>
#include <string.h>

Encoder encoder_new() 
{
  return calloc(sizeof(ems_encoder), 1);
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