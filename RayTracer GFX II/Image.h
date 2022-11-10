/***************************************************************************
* Image.h                                                                  *
*                                                                          *
* The image class defines a trivial encoding for images known as PPM       *
* format; it simply consists of an array or RGB triples, with one byte     *
* per component, preceeded by a simple header giving the size of the       *
* image.                                                                   *
*                                                                          *
*                                                                          *
***************************************************************************/
#ifndef _IMAGE_H_
#define _IMAGE_H_

#include <stdio.h>

typedef unsigned char channel;  

class Pixel 
{
	public:
		channel r;
		channel g;
		channel b;
		
		Pixel();
		Pixel( channel r_, channel g_, channel b_ );
		
};

class Image 
{
	public:
		Pixel *pixels;
		int    width;
		int    height;

		Image( int x_res, int y_res );
		~Image();
		bool Write( const char *file_name );
		Pixel &operator()( int i, int j );
};

#endif
