#include <stdint.h>
#include <stdio.h>
#include "encoder.h"
#include "readjpeg.h"

int main(void)
{
  
  Encoder encoder;
  int i;
  char filename[256];
  FILE *out;
  Data h264;
  
  encoder = encoder_init(NULL);
  
  out = fopen("out.264", "wb");
  
  for(i = 1; i < 100; i++) {
    uint8_t *raw;
    sprintf(filename, "fixtures/images/images%05d.jpg", i);
    raw = readjpeg(filename);
    encoder_consume(encoder, raw);
    h264 = encoder_encode(encoder);
    fwrite(h264.data, h264.size, 1, out);
    free(h264.data);
    free(raw);
  }
}