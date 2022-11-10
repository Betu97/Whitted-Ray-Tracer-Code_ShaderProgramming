#ifndef READER_H
#define READER_H

/***************************************************************************
*                                                                          *
* This file defines the ReadSceneDescription function, which reads a       *
* simple text description of a scene and the camera.                       *
*                                                                          *
*                                                                          *
***************************************************************************/

#include "Scene.h"
#include "Camera.h"
#include "Sphere.h"
#include "Cube.h"
#include "Triangle.h"
#include "Polygon.h"

class Reader
{
	public:

		char format[128];

		bool Get( const char *line , const char *name , Vec3 &coord );
		bool Get( const char *line , const char *name , Color &color );
		bool Get( const char *line , const char *name , float &value );
		bool Blank( char *line );

		// This is a very minimal scene description reader.  It assumes that
		// each line contains a complete entity: an object definition,
		// a camera parameter, a material parameter, etc.  (Blank lines, and
		// lines that begin with "#" are also okay.)  It fills in the fields of
		// the scene and camera as it parses the file.
		bool ReadSceneDescription( const char *file_name, Scene &scene, Camera &camera );
		
};

#endif