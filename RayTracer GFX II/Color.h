/***************************************************************************
* Color.h                                                                  *
*                                                                          *
* Color is a trivial encapsulation of floating-point RGB colors.  It has   *
* all of the obvious operators defined.							           *
*                                                                          *
*                                                                          *
***************************************************************************/
#ifndef COLOR_H
#define COLOR_H

#include <iostream>

using namespace std;

class Color
{
	public:
		double red;
		double green;
		double blue;
	public:
		
		Color();

		Color( double r, double g, double b );
		
		Color operator+( const Color &c ) const;
		
		Color operator*( double c ) const;
		
		Color operator*( const Color &B ) const;

		Color operator/( double c ) const;

		void operator+=( const Color &B );

		void operator*=( double c );

		void operator/=( double c );

		ostream& operator<<( ostream &out );

		double Norm ( void ) const;

		friend Color operator*( double b , const Color &a );
};

#endif




