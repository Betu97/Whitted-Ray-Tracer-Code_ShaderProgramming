/***************************************************************************
* Vec3.h                                                                   *
*                                                                          *
* Vec3 is a trivial encapsulation of 3D floating-point coordinates.        *
* It has all of the obvious operators defined as inline functions.         *
*                                                                          *
*                                                                          *
***************************************************************************/
#ifndef _VEC3_H_
#define _VEC3_H_

#include <math.h>
#include <iostream>

using namespace std;

struct Vec3 
{
    inline Vec3()                               { x = 0; y = 0; z = 0; }
    inline Vec3( double a, double b, double c ) { x = a; y = b; z = c; }
    double x, y, z;
};

inline double LengthSquared( const Vec3 &A )
{
    return A.x * A.x + A.y * A.y + A.z * A.z;
}

inline double Length( const Vec3 &A )
{
    return sqrt( LengthSquared( A ) );
}

inline Vec3 operator+( const Vec3 &A, const Vec3 &B )
{
    return Vec3( A.x + B.x, A.y + B.y, A.z + B.z );
}

inline Vec3 operator-( const Vec3 &A, const Vec3 &B )
{
    return Vec3( A.x - B.x, A.y - B.y, A.z - B.z );
}

inline Vec3 operator-( const Vec3 &A )  // Unary minus.
{
    return Vec3( -A.x, -A.y, -A.z );
}

inline Vec3 operator*( double a, const Vec3 &A )
{
    return Vec3( a * A.x, a * A.y, a * A.z );
}

inline Vec3 operator*( const Vec3 &A, double a )
{
    return Vec3( a * A.x, a * A.y, a * A.z );
}

inline double operator*( const Vec3 &A, const Vec3 &B )  // Inner product.
{
    return (A.x * B.x) + (A.y * B.y) + (A.z * B.z);
}

inline Vec3 operator/( const Vec3 &A, double c )
{
    return Vec3( A.x / c, A.y / c, A.z / c );
}

inline Vec3 operator^( const Vec3 &A, const Vec3 &B ) // Cross product.
{
    return Vec3( 
        A.y * B.z - A.z * B.y,
        A.z * B.x - A.x * B.z,
        A.x * B.y - A.y * B.x
        );
}

inline Vec3& operator+=( Vec3 &A, const Vec3 &B )
{
    A.x += B.x;
    A.y += B.y;
    A.z += B.z;
    return A;
}

inline Vec3& operator-=( Vec3 &A, const Vec3 &B )
{
    A.x -= B.x;
    A.y -= B.y;
    A.z -= B.z;
    return A;
}

inline Vec3 &operator*=( Vec3 &A, double a )
{
    A.x *= a;
    A.y *= a;
    A.z *= a;
    return A;
}

inline Vec3& operator/=( Vec3 &A, double a )
{
    A.x /= a;
    A.y /= a;
    A.z /= a;
    return A;
}

inline Vec3 operator/( const Vec3 &A, const Vec3 &B )  // Remove component parallel to B.
{
    double x = LengthSquared( B );
    if( x > 0.0 ) return A - (( A * B ) / x) * B;
	return A;
}

inline Vec3 Unit( const Vec3 &A )
{
    double d = LengthSquared( A );
    return d > 0.0 ? A / sqrt(d) : Vec3(0,0,0);
}

inline double dist( const Vec3 &A, const Vec3 &B ) // Euclidean distance from A to B.
{ 
    return Length( A - B ); 
}

inline ostream &operator<<( ostream &out, const Vec3 &A )
{
    out << "( " << A.x << ", " << A.y << ", " << A.z << " ) ";
    return out;
}

inline Vec3 Reflection( const Vec3 &V, const Vec3 &N )
{
	return V - 2 * N * ( N * V );
}

inline Vec3 Refraction( const Vec3 &V, const Vec3 &N, double eta )
{
	double cos_in, cos_out_sq;

	cos_in = (-V) * N;
	cos_out_sq = 1 - ( 1 - cos_in * cos_in ) / ( eta * eta );

	// If cs2 < 0 --> Total Internal Reflection (no reflectivity)
	if( cos_out_sq < 0 )
	{
		return Vec3();
	}

	return V / eta - ( sqrt( cos_out_sq ) - cos_in / eta ) * N;
}

inline int insideRectangle (Vec3 Point, Vec3 Corners[4]) // Return true if the point is inside a rectangle
{
	int i;
	Vec3 CornerToCorner;
	Vec3 CornerToPoint;

	for (i=0; i<4; i++)
	{
		// Calcualte corner to corner vector
		CornerToCorner = Corners [( i+1 ) % 4] - Corners [i];

		// Calculate corner to point vector
		CornerToPoint = Point - Corners [i];

		// Compute dot vector between them
		if ( CornerToCorner  * CornerToPoint < 0.0 )
		{
			// Point outside the rectangle
			return false;
		}
	}

	// Point is inside the rectangle
	return true;
}


#endif


