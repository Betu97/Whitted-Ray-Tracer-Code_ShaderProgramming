#ifndef PLANE_H
#define PLANE_H

#include "Vec3.h"
#include "Utils.h"
// A plane is defined by 4 coordinates:
// A, B and C define the normal of the plane
// D defines the offset on the normal direction of the plane
class Plane {
	public:
		Plane() { A = 0; B = 0; C = 0; D = 0; }
		Plane( double A_, double B_, double C_, double D_ );
		Vec3 getNormal();
		double Intersect( const Ray &ray );
		double A, B, C, D;
		};

#endif