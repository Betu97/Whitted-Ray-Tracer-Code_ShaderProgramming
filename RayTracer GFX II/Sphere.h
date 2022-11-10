#ifndef SPHERE_H
#define SPHERE_H

// To intersect a ray with a sphere with the given center and radius, we
// solve the following equation for s: || Q + sR - C || = radius, where Q is
// the ray origin, R is the ray direction, and C is the center of the sphere.
// This is equivalent to  ( A + sR )^T ( A + sR ) = radius^2, where A = Q - C.
// Expanding, we have A.A + 2 s A.R + s^2 R.R = radius^2, there A.A means
// the dot product of A with itself.  Since R is a unit vercor, R.R = 1.
// Rearranging, we have s^2 + (2 A.R) s + (A.A - radius^2) = 0, which is a
// quadratic equation in s, the distance along the ray to the point of
// intersection.  If this equation has complex roots, then the ray misses
// the sphere.  Otherwise, we must determine whether either of the roots
// falls on the positive part of the ray, and if so, which is closer.

#include "Object.h"
#include "Vec3.h"

class Sphere : public Object 
{
	public:
		Vec3  center;
		float radius;

		Sphere( const Vec3 &center, float radius );
		bool Intersect( const Ray &ray, HitGeom &hitgeom ) const;
		Box3 GetBounds() const;
		Sample GetSample( const Vec3 &P, const Vec3 &N ) const;
		static Object *ReadString( const char *params );
};

#endif