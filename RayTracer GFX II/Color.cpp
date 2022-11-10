#include <math.h>
#include "Color.h"

Color::Color()
{
	red = 0; 
	green = 0;
	blue = 0;
}

Color::Color( double r, double g, double b )
{
	red = r;
	green = g; 
	blue = b; 
}

Color Color::operator+( const Color &c ) const
{
	return Color( this->red + c.red, this->green + c.green, this->blue + c.blue );
}

Color Color::operator*( double c ) const
{
	return Color( c * this->red, c * this->green, c * this->blue );
}

Color Color::operator*( const Color &B ) const
{
	// Colors are multiplied component-wise, and result in another color, not
	// a scalar.  This is the most significant difference between the Vec3 class
	// and the Color class.
	return Color( this->red * B.red, this->green * B.green, this->blue * B.blue );
}

Color Color::operator/( double c ) const
{
	return Color( this->red / c, this->green / c, this->blue / c );
}

void Color::operator+=( const Color &B )
{
	this->red   += B.red;
	this->green += B.green;
	this->blue  += B.blue;
}

void Color::operator*=( double c )
{
	this->red   *= c;
	this->green *= c;
	this->blue  *= c;
}

void Color::operator/=( double c )
{
	this->red   /= c;
	this->green /= c;
	this->blue  /= c;
}

ostream &Color::operator<<( ostream &out )
{
	out << "[ " << this->red << ", " << this->green << ", " << this->blue << " ] ";
	return out;
}

double Color::Norm( void ) const
{
	return sqrt( this->red * this->red + this->green * this->green + this->blue * this->blue );
}

Color operator*( double c , const Color &a )
{
	return Color( c * a.red, c * a.green, c * a.blue );
}