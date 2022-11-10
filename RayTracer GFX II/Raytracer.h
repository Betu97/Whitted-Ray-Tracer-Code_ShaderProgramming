#ifndef RAYTRACER_H
#define RAYTRACER_H

/***************************************************************************
*                                                                          *
* This is the header file for a "Toy" ray tracer.  This ray tracer is very *
* minimal, and is intended simply for learning the basics of ray tracing.  *
* Here is a partial list of things that are missing from this program that *
* a serious ray tracer would have:                                         *
*                                                                          *
* 1) A method of reading in the geometry of a scene from a file.           *
*                                                                          *
* 2) At least one technique for speeding up ray-object intersection when   *
*    there are a large number of object (e.g. thousands).  In contrast,    *
*    this toy ray tracer uses "brute force" or "naive" ray intersection.   *
*    That is, it simply tests all objects and keeps the closest hit.       *
*                                                                          *
* 3) Some method of anti-aliasing; that is, a method for removing the      *
*    jagged edges, probably by casting multiple rays per pixel.            *
*                                                                          *
*                                                                          *
***************************************************************************/

#include "Utils.h"
#include "Image.h"
#include "World.h"

#include <GL/glut.h>

class Raytracer
{
	Image*	I;
	int		resolutionX;
	int		resolutionY;
	int		currentLine;
	bool	isDone;

	public:
		Raytracer( int x, int y )
		{
			I = new Image(x, y);
			resolutionX = x;
			resolutionY = y;
			currentLine = 0;
			isDone = false;
		}
		virtual ~Raytracer(){
			delete I;
		}
		void draw( void );
		void cast_line( World world );
		bool IsDone( void )
		{
			return isDone;
		}

	private:

		Pixel ToneMap( const Color &color );
		
		Color Trace(						// What color do I see looking along this ray?
					const Ray   &ray,       // Root of ray tree to recursively trace in scene.
					const Scene &scene,		// Global scene description, including lights.
					int max_tree_depth		// Limit to depth of the ray tree.
		);

		Color Shade(						// Surface shader.
					const HitInfo &hitinfo, // Geometry of ray-object hit + surface material.
					const Scene   &scene,    // Global scene description, including lights.
					int max_tree_depth		// Limit to depth of the ray tree.
		);

		Color Shade2(						// Surface shader.
					const HitInfo &hitinfo, // Geometry of ray-object hit + surface material.
					const Scene   &scene,    // Global scene description, including lights.
					int max_tree_depth		// Limit to depth of the ray tree.
		);

		int Cast(							// Casts a single ray to see what it hits.
					const Ray   &ray,       // The ray to cast into the scene.
					const Scene &scene,     // Global scene description, including lights.
					HitInfo     &hitinfo,    // All information about ray-object intersection.
					Object		*ignore	 = false  // Object that will be ignored for the intersection
		);

		int Cast2(							// Casts a single ray to see what it hits.
					const Ray   &ray,       // The ray to cast into the scene.
					const Scene &scene,     // Global scene description, including lights.
					HitInfo     &hitinfo,    // All information about ray-object intersection.
					Object		*ignore	 = false  // Object that will be ignored for the intersection
		);

		Sample SampleProjectedHemisphere(
					const Vec3 &N			// Normal of the surface
		);

		Sample SampleSpecularLobe(
					const Vec3 &R,			// Perfect reflective direction
					float phong_exp			// Phong exponent
		);
};

#endif