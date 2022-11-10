#ifndef OBJECT_H
#define OBJECT_H
/***************************************************************************
*                                                                          *
* This file defines the methods associated with the objects, the most      *
* important of which is the Intersect methos.  The ray tracer can render   *
* anything that it can intersect a ray with.  When a ray is determined to  *
* hit an object, the intersector stores some information in the "HitGeom"  *
* structure, which will be passed to the shader.  IMPORTANT: an            *
* intersector is NOT ALLOWED to change anything in the HitGeom structure   *
* unless it finds a point of intersection that is CLOSER than              *
* "hitgeom.distance".  This is how the ray tracer always finds the closest *
* intersection.  If the intersector finds an intersection, but it is       *
* further away than hitgeom.distance, it is to report failure (i.e. return *
* false).                                                                  * 
*                                                                          *
*                                                                          *
***************************************************************************/

#include <stdio.h>

#include "Vec3.h"
#include "Color.h"
#include "Utils.h"


class Object	// This encodes all objects that are ray traced.
{
	public:
		Material material;
		Object  *next;
		
		Object();
		virtual ~Object(){}
		virtual bool Intersect( const Ray &ray, HitGeom &hitgeom ) const = 0;
		virtual Box3 GetBounds() const = 0;
		virtual Sample GetSample( const Vec3 &P, const Vec3 &N ) const {return Sample();}
		
};

#endif