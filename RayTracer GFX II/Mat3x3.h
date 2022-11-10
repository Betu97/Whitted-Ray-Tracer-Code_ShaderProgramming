/***************************************************************************
* Mat3x3.h                                                                 *
*                                                                          *
* Mat3x3 is a 3x3 matrix class, with associated operators.                 *
*                                                                          *
* History:                                                                 *
*   04/07/2003  Initial coding.                                            *
*                                                                          *
***************************************************************************/
#ifndef _MAT3x3_H_
#define _MAT3x3_H_

#include <iostream>
#include "Vec3.h"

using namespace std;

struct Mat3x3 {
    inline Mat3x3();
    inline Mat3x3( const Mat3x3 &M ) { *this = M; }
    inline ~Mat3x3() {}
    inline       double &operator()( int i, int j )       { return m[i][j]; }
    inline const double &operator()( int i, int j ) const { return m[i][j]; }
    inline static Mat3x3 Identity();
    double m[3][3];
    };

inline Mat3x3::Mat3x3()
    {
    for( int i = 0; i < 3; i++ )
    for( int j = 0; j < 3; j++ ) m[i][j] = 0.0;
    }

inline Mat3x3 operator+( const Mat3x3 &A, const Mat3x3 &B )
    {
    Mat3x3 C;
    for( int i = 0; i < 3; i++ )
    for( int j = 0; j < 3; j++ ) C(i,j) = A(i,j) + B(i,j);
    return C;
    }

inline Mat3x3 operator-( const Mat3x3 &A, const Mat3x3 &B )
    {
    Mat3x3 C;
    for( int i = 0; i < 3; i++ )
    for( int j = 0; j < 3; j++ ) C(i,j) = A(i,j) - B(i,j);
    return C;
    }

inline Mat3x3 operator*( double c, const Mat3x3 &M )
    {
    Mat3x3 A;
    for( int i = 0; i < 3; i++ )
    for( int j = 0; j < 3; j++ ) A(i,j) = c * M(i,j);
    return A;    
    }

inline Mat3x3 operator*( const Mat3x3 &M, double c )
    {
    return c * M;
    }

inline Vec3 operator*( const Mat3x3 &M, const Vec3 &A )
    {
    return Vec3(
        M(0,0) * A.x + M(0,1) * A.y + M(0,2) * A.z,
        M(1,0) * A.x + M(1,1) * A.y + M(1,2) * A.z,
        M(2,0) * A.x + M(2,1) * A.y + M(2,2) * A.z
        );
    }

inline Mat3x3 operator*( const Mat3x3 &A, const Mat3x3 &B )
    {
    Mat3x3 C;
    for( int i = 0; i < 3; i++ )
    for( int j = 0; j < 3; j++ )
        C(i,j) = A(i,0) * B(0,j) + A(i,1) * B(1,j) + A(i,2) * B(2,j);
    return C;
    }

inline Mat3x3 operator/( const Mat3x3 &M, double c )
    {
    return (1/c) * M;
    }

inline Mat3x3 Adjoint( const Mat3x3 &M )  // The Adjoint matrix is related to the inverse.
    {
    Mat3x3 A;
    A(0,0) = M(1,1) * M(2,2) - M(1,2) * M(2,1);
    A(0,1) = M(1,2) * M(2,0) - M(1,0) * M(2,2);
    A(0,2) = M(1,0) * M(2,1) - M(1,1) * M(2,0);
 
    A(1,0) = M(0,2) * M(2,1) - M(0,1) * M(2,2);
    A(1,1) = M(0,0) * M(2,2) - M(0,2) * M(2,0);
    A(1,2) = M(0,1) * M(2,0) - M(0,0) * M(2,1);

    A(2,0) = M(0,1) * M(1,2) - M(0,2) * M(1,1);
    A(2,1) = M(0,2) * M(1,0) - M(0,0) * M(1,2);
    A(2,2) = M(0,0) * M(1,1) - M(0,1) * M(1,0);
    return A;
    }

inline double det( const Mat3x3 &M )  // Determinant.
    {
    return
        M(0,0) * ( M(1,1) * M(2,2) - M(1,2) * M(2,1) )
      - M(0,1) * ( M(1,0) * M(2,2) - M(1,2) * M(2,0) )
      + M(0,2) * ( M(1,0) * M(2,1) - M(1,1) * M(2,0) );
    }

inline Mat3x3 Transpose( const Mat3x3 &M )
    {
    Mat3x3 W;
    for( int i = 0; i < 3; i++ )
    for( int j = 0; j < 3; j++ ) W(i,j) = M(j,i);
    return W;
    }

Mat3x3 Mat3x3::Identity()
    {
    Mat3x3 I;
    I(0,0) = 1.0;
    I(1,1) = 1.0;
    I(2,2) = 1.0;
    return I;
    }

inline ostream &operator<<( ostream &out, const Mat3x3 &M )
    {
    out << "\n";
    out << "| " << M(0,0) << " " << M(0,1) << " " << M(0,2) << " |\n";
    out << "| " << M(1,0) << " " << M(1,1) << " " << M(1,2) << " |\n";
    out << "| " << M(2,0) << " " << M(2,1) << " " << M(2,2) << " |\n";
    out << endl;
    return out;
    }

#endif
