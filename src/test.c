
#include <stdint.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include "encoder.h"
#include "readjpeg.h"

int main(void)
{
  
  Encoder encoder = NULL;
  Image *image = NULL;
  int i;
  char filename[256];
  FILE *out;
  
  out = fopen("out.264", "wb");
  
  for(i = 1; i < 100000; i++) {
    Data h264;
    uint8_t *jpeg;
    int fd;
    struct stat jpg_stat;
    sprintf(filename, "fixtures/images/images%05d.jpg", i);
    fd = open(filename, O_RDONLY);
    if(fd == -1) {
      break;
    }
    stat(filename, &jpg_stat);
    jpeg = mmap(NULL, (size_t)jpg_stat.st_size, PROT_READ, MAP_SHARED, fd, 0);
    
    image = readjpeg(jpeg, jpg_stat.st_size, image);
    
    if(!encoder) {
      encoder = encoder_init(image->width, image->height, NULL);
    }

    h264 = encoder_encode(encoder, image->data);
    fwrite(h264.data, h264.size, 1, out);

    munmap(jpeg, jpg_stat.st_size);
    close(fd);

    free(h264.data);
  }
}