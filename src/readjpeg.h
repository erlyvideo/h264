#ifndef _READJPEG_H_
#define _READJPEG_H_

#include <stdio.h>
#include <stdint.h>
#include <jpeglib.h>
#include <setjmp.h>

typedef struct {
  uint32_t width;
  uint32_t height;
  uint8_t data[1]; 
} Image;

extern Image *readjpeg(uint8_t *data, size_t size, Image* prev);


#endif /* _READJPEG_H_ */
