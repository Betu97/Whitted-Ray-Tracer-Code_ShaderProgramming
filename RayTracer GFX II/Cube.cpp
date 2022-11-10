#include "Cube.h"

Cube::Cube( const Vec3 &Min_, const Vec3 &Max_ )
{
    Min = Min_;
    Max = Max_;
}

Object *Cube::ReadString( const char *params ) // Read params from string.
{
    float x1, y1, z1, x2, y2, z2;
    if( sscanf( params, "cube (%f,%f,%f) (%f,%f,%f)", &x1, &y1, &z1, &x2, &y2, &z2 ) == 6 )
        return new Cube( Vec3( x1, y1, z1 ), Vec3( x2, y2, z2 ) );
    return NULL;
}

Box3 Cube::GetBounds() const // Returns a bounding box (which is just the box itself).
{
    Box3 box;
    box.X.min = Min.x;  box.X.max = Max.x;
    box.Y.min = Min.y;  box.Y.max = Max.y;
    box.Z.min = Min.z;  box.Z.max = Max.z;
    return box;
}

bool Cube::Intersect( const Ray &ray, HitGeom &hitgeom ) const
{	

	Plane plans [6] = {Plane(1,0,0,-Max.x),
		Plane(0,1,0,-Max.y),
		Plane(0,0,1,-Max.z),
		Plane(-1,0,0,Min.x),
		Plane(0,-1,0,Min.y),
		Plane(0,0,-1,Min.z)
	};

	bool bInterseccio = false;
	int i;
	double raigIntersec;
	Vec3 posicio;
	bool dins;

	for (i = 0; i < 6 ; i++)
	{
		raigIntersec = plans[i].Intersect(ray);

		if( raigIntersec > 0.0 && raigIntersec < hitgeom.distance)
		{

			posicio = ray.origin + (ray.direction * raigIntersec);

			dins = false;
			switch (i)
			{
				case 0:
					if(posicio.y > Min.y && posicio.y < Max.y && posicio.z > Min.z && posicio.z < Max.z)
						dins = true;
					break;
				case 1:
					if(posicio.x > Min.x && posicio.x < Max.x && posicio.z > Min.z && posicio.z < Max.z)
						dins = true;
					break;
				case 2:
					if(posicio.y > Min.y && posicio.y < Max.y && posicio.x > Min.x && posicio.x < Max.x)
						dins = true;
					break;
				case 3:
					if(posicio.y > Min.y && posicio.y < Max.y && posicio.z > Min.z && posicio.z < Max.z)
						dins = true;
					break;
				case 4:
					if(posicio.x > Min.x && posicio.x < Max.x && posicio.z > Min.z && posicio.z < Max.z)
						dins = true;
					break;
				case 5:
					if(posicio.y > Min.y && posicio.y < Max.y && posicio.x > Min.x && posicio.x < Max.x)
						dins = true;
					break;
			}

			if (dins)
			{
			//	if((ray.direction * plans[i].getNormal()) < 0.0)
				{
					hitgeom.distance = raigIntersec;
					hitgeom.normal = plans[i].getNormal();
					hitgeom.origin = ray.origin;
					hitgeom.point = posicio;
					bInterseccio = true;
				}
			}

		}
	}
	
	return bInterseccio;
}

Sample Cube::GetSample( const Vec3 &P, const Vec3 &N ) const
{
	double s, t;

	int count = 0;
	double cos_theta; //, cos_theta_2;
	double d = 1.0;
	
	double projected_area;

	Sample sample;

	// Cosine of the angle between the light ray and the normal at the light source
	cos_theta = fabs( N * Unit( P - sample.P ) );
	// Cosine of the angle between the light ray and the normal at the light source
	//cos_theta_2 = fabs( Vec3( 0.0, -1.0, 0.0 ) * Unit( P - sample.P ) );

	projected_area = 1.0f;

	// Generates two random values for s and t coordinates
	s = rand( 0.0, 1.0 );
	t = rand( 0.0, 1.0 );
	// Adds the new position
	sample.P = Vec3( s, Min.y, t  );

	// Calcule the distance between the sample and the point P
	d = Length( sample.P - P );
	// Assigns the weight
	sample.w = projected_area / ( d * d );

	return sample;
}