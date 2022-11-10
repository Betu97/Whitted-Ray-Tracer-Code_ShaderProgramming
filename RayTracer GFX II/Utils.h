#ifndef UTILS_H
#define UTILS_H

	#include "Material.h"

	#include "Color.h"
	#include "Vec3.h"


	static const double Infinity = 1.0E10;
	static const double Epsilon  = 1.0E-4;
	static const double Pi       = 3.14159265358979;
    static const double TwoPi    = 2.0 * Pi;
    static const double FourPi   = 4.0 * Pi;
	static const int NUM_SAMPLES_SQRT =	7;	// Square root of the number of samples


	class Interval // An interval of real numbers.
	{
		public:
			double min;
			double max;
	};

	class Box3 // A box in R3, useful for bounding boxes.
	{           
		public:
			Interval X;
			Interval Y;
			Interval Z;
	};

	class HitGeom // Records geometric info for ray-object intersection.
	{        
		public:
			double distance;    // Distance along ray to the point of intersection.
			Vec3   point;       // The point of ray-object intersection.
			Vec3   normal;      // The surface normal at the point of intersection.
			Vec3   origin;      // Origin of ray that hit the surface.
	};

	class HitInfo // Records all shading info at ray-object intersection.
	{     
		public:
			HitGeom  geom;      // The geometric information.
			Material material;  // The material of the surface.

    };
	class Ray // A ray in R3.
		{   
			public:
				Vec3 origin;        // The ray originates from this point.
				Vec3 direction;     // Unit vector indicating direction of ray.
				bool no_emitters;	// Boolean used to know when emitters should return 0 irradiance
		};


	class Sample {         // A point and weight returned from a sampling algorithm.
		public:
			Vec3   P;
			double w;
    };
	
	inline double rand( double a, double b )
    {
		double x = float(rand()) / RAND_MAX;
		if( x < 0.0 ) x = -x;
		return a + x * ( b - a );
    }
#endif