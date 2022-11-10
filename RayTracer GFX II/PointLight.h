#ifndef POINTLIGHT_H
#define POINTLIGHT_H

#include "Vec3.h"
#include "Color.h"

class PointLight // Defines a single point light source.
{   
	public:
		Vec3  m_Position;
		Color m_Color;
};

#endif