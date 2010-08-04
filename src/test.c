
#include "encoder.h"

int main(void)
{
  
  Encoder encoder;
  int i;
  char filename[256];
  
  encoder = encoder_init(NULL);
  
  for(i = 1; i < 100; i++) {
    uint8_t *raw;
    sprintf(filename, "fixtures/images/images%05d.jpg", i);
    raw = readjpeg(filename);
    free(raw);
  }
}