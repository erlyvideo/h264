
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
    sprintf(filename, "fixtures/images/images%05d.jpg", i);
    raw = readjpeg(filename);
    
    h264 = encoder_encode(encoder, raw);
    fwrite(h264.data, h264.size, 1, out);
    // free(h264.data);
    // free(raw);
  }
}