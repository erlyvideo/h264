#ifndef _ENCODER_H_
#define _ENCODER_H_

#include <stdint.h>
#include <stdlib.h>
#include <x264.h>


typedef struct {
  x264_param_t	param;
	x264_t			*encoder;

	x264_picture_t	picture;

	uint64_t		frameNo;
	uint32_t		level;
} ems_encoder;

typedef ems_encoder *Encoder;

extern Encoder encoder_new();
Encoder encoder_init(Encoder state);

void encoder_free(Encoder state);
void encoder_close(Encoder state);


#endif /* _ENCODER_H_ */