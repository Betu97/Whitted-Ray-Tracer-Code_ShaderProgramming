#ifndef POLYGON_H
#define POLYGON_H

#include "Object.h"
#include "Mat3x3.h"
#include "Plane.h"
class Polygon : public Object
{
    
	public:
		Vec3   N;     // Normal to plane of polygon;
		double d;     // Distance from origin to plane of polygon.
		Box3   box;   // Bounding box;
		Vec3 A, B, C, D, E; // Corners of the polygon

		Polygon( const Vec3 &A, const Vec3 &B, const Vec3 &C , const Vec3 &D , const Vec3 &E );
		bool Intersect( const Ray &ray, HitGeom &hitgeom ) const;
		Box3 GetBounds() const;
		static Object *ReadString( const char *params );
};

#endif 