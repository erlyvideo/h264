
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
  
  encoder = encoder_init(NULL);
  
  out = fopen("out.264", "wb");
  
  for(i = 1; i < 100; i++) {
    Data h264;
    uint8_t *raw;
    FILE *jpg;
    sprintf(filename, "fixtures/images/images%05d.jpg", i);
    raw = readjpeg(filename);

    // sprintf(filename, "fixtures/images/images%05d.raw", i);
    // jpg = fopen(filename, "rb");
    // if(!jpg) break;
    // raw = malloc(1221120);
    // fread(raw, 1221120, 1, jpg);
    // fclose(jpg);
    
    h264 = encoder_encode(encoder, raw);
    fwrite(h264.data, h264.size, 1, out);
    free(h264.data);
    free(raw);
  }
}