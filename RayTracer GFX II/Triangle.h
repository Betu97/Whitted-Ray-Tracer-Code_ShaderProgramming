#ifndef TRIANGLE_H
#define TRIANGLE_H

#include "Object.h"
#include "Mat3x3.h"
#include "Plane.h"
class Triangle : public Object
{
    
	public:
		Mat3x3 M;     // Inverse of barycentric coord transform.
		Vec3   N;     // Normal to plane of triangle;
		double d;     // Distance from origin to plane of triangle.
		Box3   box;   // Bounding box;
		int    axis;  // The dominant axis;
		Vec3 A, B, C; // Corners of the triangle
		float area;   // Area of the triangle

		Triangle( const Vec3 &A, const Vec3 &B, const Vec3 &C );
		bool Intersect( const Ray &ray, HitGeom &hitgeom ) const;
		Box3 GetBounds() const;
		static Object *ReadString( const char *params );
		Sample GetSample( const Vec3 &P, const Vec3 &N_point ) const;
};

#endif 