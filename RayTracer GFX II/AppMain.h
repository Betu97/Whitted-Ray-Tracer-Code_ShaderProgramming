#ifndef APPMAIN_H
#define APPMAIN_H


#include "World.h"
#include "Raytracer.h"

#include <GL/glut.h>

#define RESOLUTIONX 400
#define RESOLUTIONY 400

Raytracer g_raytracer( RESOLUTIONX , RESOLUTIONY );
World w;

#endif