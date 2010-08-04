#ifndef _READJPEG_H_
#define _READJPEG_H_

#include <stdio.h>
#include <stdint.h>
#include <jpeglib.h>
#include <setjmp.h>

typedef struct {
  int width;
  int height;
  uint8_t data[1]; 
} Image;

extern Image *readjpeg(uint8_t *data, size_t size, Image* prev);


#endif /* _READJPEG_H_ */
