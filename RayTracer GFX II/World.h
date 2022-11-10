#ifndef WORLD_H
#define WORLD_H

#include "Vec3.h"
#include "Camera.h"
#include "Scene.h"
#include "Reader.h"
#include "PointLight.h"

#include <iostream>
#include <stdio.h>

using namespace std;

class World
{
	private:
		Camera	cam;
		Scene	sce;
	public:
		World() {};
		virtual ~World() {};
		bool readScene( const char *filename );
		Camera getCamera( void );
		Scene getScene( void );
};

#endif