
#include <stdint.h>
#include <stdio.h>
#include <sys/stat.h>
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
  
  for(i = 1; i < 10000; i++) {
    Data h264;
    uint8_t *raw, *rgb;
    FILE *jpg;
    struct stat jpg_stat;
    sprintf(filename, "fixtures/images/images%05d.jpg", i);
    jpg = fopen(filename, "rb");
    if(!jpg) break;
    stat(filename, &jpg_stat);
    raw = malloc(jpg_stat.st_size);
    fread(raw, jpg_stat.st_size, 1, jpg);
    fclose(jpg);
    rgb = readjpeg(raw, jpg_stat.st_size);

    h264 = encoder_encode(encoder, rgb);
    fwrite(h264.data, h264.size, 1, out);
    free(h264.data);
    free(raw);
  }
}