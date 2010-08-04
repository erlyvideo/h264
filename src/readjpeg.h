#ifndef _READJPEG_H_
#define _READJPEG_H_

#include <stdio.h>
#include <stdint.h>
#include <jpeglib.h>
#include <setjmp.h>

extern uint8_t *readjpeg(uint8_t *data, size_t size);

#endif /* _READJPEG_H_ */
