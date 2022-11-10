#include "Plane.h"

Plane::Plane( double A_, double B_, double C_, double D_ )  // Plane constructor
{
	A = A_;
	B = B_;
	C = C_;
	D = D_;
}

Vec3 Plane::getNormal() // Returns the normal of the plane
{
	Vec3 normal( A, B, C );
	return normal;
}

double Plane::Intersect( const Ray &ray) // Returns the intersection point in ray coordinates
{
	double div;
    Vec3 orig = ray.origin;
    Vec3 direc = ray.direction;

	div = A*direc.x + B*direc.y + C*direc.z;

	if (div != 0)
		return - ((A*orig.x + B*orig.y + C*orig.z + D) / div);

	// Ray and plane parallel
	return 0.0;
}