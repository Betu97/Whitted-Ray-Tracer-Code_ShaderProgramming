#ifndef SCENE_H
#define SCENE_H

#include "PointLight.h"
#include "Object.h"

class Scene 
{
	public:
		int num_lights;       // Number of light sources.
		Color ambient;        // The single ambient light.
		Color bgcolor;        // Background color, if ray does not hit anything. 
		PointLight light[10]; // Info about each light source.
		Object *first;        // The first of a list of objects.
};

#endif