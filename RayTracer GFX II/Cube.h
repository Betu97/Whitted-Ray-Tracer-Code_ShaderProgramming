#ifndef CUBE_H
#define CUBE_H

#include "Object.h"
#include "Plane.h"
class Cube : public Object 
{
	public:
		Vec3 Min;
		Vec3 Max;

		Cube( const Vec3 &Min, const Vec3 &Max );
		bool Intersect( const Ray &ray, HitGeom &hitgeom ) const;

		Sample GetSample( const Vec3 &P, const Vec3 &N ) const;

		Box3 GetBounds() const;
		static Object *ReadString( const char *params );

};

#endif