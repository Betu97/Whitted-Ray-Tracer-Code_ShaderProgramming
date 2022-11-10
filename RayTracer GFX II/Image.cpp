#include "Image.h"

Pixel::Pixel()
{ 
	r = 0; 
	g = 0;
	b = 0; 
}

Pixel::Pixel( channel r_, channel g_, channel b_ ) 
{ 
	r = r_; 
	g = g_; 
	b = b_; 
}

Image::Image( int x_res, int y_res )
{
    width  = x_res;
    height = y_res;
    pixels = new Pixel[ width * height ];
    Pixel *p = pixels;
    for( int i = 0; i < width * height; i++ ) *p++ = Pixel(0,0,0);
}

Image::~Image() 
{ 
	delete[] pixels;
}

Pixel &Image::operator()( int i, int j ) 
{ 
	return *( pixels + ( i * width + j ) ); 
}  

bool Image::Write( const char *file_name )
{
    Pixel *p;
    FILE  *fp = fopen( file_name, "w+b" );
    if( fp == NULL ) return false; 
    fprintf( fp, "P6\n%d %d\n255\n", width, height );
	for( int i = height-1; i >= 0; i-- )
	{
		p = &(pixels[width*i]);
		for( int j = 0; j < width; j++ )
		{
			fprintf( fp, "%c%c%c", p->r, p->g, p->b );
			p++;
		}
	}
    fclose( fp );
    return true;
}