#ifndef MATERIAL_H
#define MATERIAL_H

#include "Color.h"
	// Surface material for shading.
	class Material 
	{
		public:
			Color m_Diffuse;      // Diffuse color.
			Color m_Specular;     // Color of highlights.
			Color m_Emission;     // Emitted light.
			int   m_Type;         // Reserved for future use.
			float m_Phong_exp;    // Phong exponent for specular highlights.
			float m_Reflectivity; // Weight given to mirror reflection, between 0 and 1.
			float m_RefractiveIndex;	// (vel. llum en el buit) / (vel. llum en aquest material)
			float m_Opacity;			// [0-1] 0:transparent, 1:opac
			bool  Emitter() const { return ( m_Emission.red != 0 || m_Emission.blue != 0 || m_Emission.green != 0 ); } 
	};

#endif