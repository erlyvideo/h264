#ifndef _ENCODER_H_
#define _ENCODER_H_

#include <stdint.h>
#include <stdlib.h>
#include <x264.h>

#include <libswscale/swscale.h>


typedef struct {
  x264_param_t	param;
	x264_t			*encoder;

	x264_picture_t	picture;

	uint64_t		frame;
	uint32_t		level;
  uint8_t     *yuv;
  uint32_t    width;
  uint32_t    height;
  struct SwsContext* convertCtx;
} ems_encoder;

typedef struct {
  uint8_t *data;
  size_t  size;
} Data;
  
extern Data concat_data(Data, uint8_t *buf, size_t size);

typedef ems_encoder *Encoder;

extern Encoder encoder_new();
Encoder encoder_init(Encoder state);

extern void encoder_free(Encoder state);
extern void encoder_close(Encoder state);
extern Data encoder_config(Encoder state);

extern Data encoder_encode(Encoder state, uint8_t *rgb);

#endif /* _ENCODER_H_ */