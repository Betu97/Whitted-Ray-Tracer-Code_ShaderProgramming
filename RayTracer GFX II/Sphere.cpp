#include "Sphere.h"

Sphere::Sphere( const Vec3 &cent, float rad )
{
    center = cent;
    radius = rad;
}


Object *Sphere::ReadString( const char *params ) // Reads params from a string.
{
    float x, y, z, r;
    if( sscanf( params, "sphere (%f,%f,%f) %f", &x, &y, &z, &r ) == 4 )
        return new Sphere( Vec3( x, y, z ), r );
    return NULL;
}

Box3 Sphere::GetBounds() const // Returns a bounding box.
{
    Box3 box;
    box.X.min = center.x - radius;  box.X.max = center.x + radius;
    box.Y.min = center.y - radius;  box.Y.max = center.y + radius;
    box.Z.min = center.z - radius;  box.Z.max = center.z + radius;
    return box;
}

bool Sphere::Intersect( const Ray &ray, HitGeom &hitgeom ) const
{
    Vec3 A = ray.origin - center;
    Vec3 R = ray.direction;
    double b = 2.0 * ( A * R );
    double discr = b * b - 4.0 * ( A * A - radius * radius );  // The discriminant.

    // If the discriminant if negative, the quadratic equation had negative
    // roots, and the ray misses the sphere.

    if( discr < 0.0 ) return false;

    discr = sqrt( discr );

    // First try the smaller of the two roots.  If this is positive, it is the
    // closest intersection.

    double s = ( -b - discr ) * 0.5;
    if( s > 0.0 )
        {
        // If the closest intersection is too far away, report a miss.
        if( s > hitgeom.distance ) return false;
        }
    else
        {
        // Now try the other root, since the smallest root puts the
        // point of intersection "behind"us.  If the larger root is now
        // positive, it means we are inside the sphere.
        s = ( -b + discr ) * 0.5;
        if( s <= 0 ) return false;
        if( s > hitgeom.distance ) return false;
        }

    // We have an actual hit.  Fill in all the geometric information so
    // that the shader can shade this point.

    hitgeom.distance = s;
    hitgeom.point    = ray.origin + s * R;
    hitgeom.normal   = Unit( hitgeom.point - center );
    hitgeom.origin   = ray.origin;
    return true;
}

Sample Sphere::GetSample( const Vec3 &P, const Vec3 &N ) const
{
	Vec3 reflect;		// Vector of reflection between the sphere on 0,0,d and the current sphere
	float s, t;			// Coordinates of the sample on the unit square
	float r;
	float d;			// Distance between P and the center of the current sphere
	float alfa;			// Angle between 0,0,1 and every vector that goes from 0,0,0 to the sphere
						// with center in 0,0,d.
	float h;			// Distance from the origin to the base of the cap defined by the solid angle
	Ray ray;			// Ray used to find the intersecion of the vector sample direction and the sphere
	HitGeom hitgeom;	// Information about the intersection of the ray and the sphere
	float weight;		// Weight applied to every sample

	Sample sample;

	// Find the unit vector that lies between the vector from the point P to the
	// center of the sphere and the vector (0, 0, 1).

	reflect = Unit( Unit( center - P ) + Vec3( 0, 0, 1 ) );

	// Compute the distance from the point P to the base of the cap formed by the solid angle
	// substained by the sphere on the point P.

	// Compute distance d
	d = (float) dist( P, center );
	// Compute angle alfa
	alfa = (float) asin( radius / d );
	// Decrements alfa espsilon to avoid vectors pointing the tangent of the
	// sphere that can be missed when looking for its position into the sphere
	alfa -= alfa * (float) Epsilon;
	// Compute h
	h = (float) cos( alfa );

	// Compute the weigth
	// Weight is computed as the solid angle
	weight = ( (float)TwoPi * ( 1 - h ) );

	// Compute the sample

	// Generates two random values for s and t coordinates
	s = (float) rand( 0.0, 1.0 );
	t = (float) rand( 0.0, 1.0 );

    // Find the sample on the unit sphere centered in the origin
	// In fact, we are computing a unit vector

	// Compute factor r
	r = h + ( s * ( 1 - h ) );
	r *= r;
	r = (float) sqrt( 1 - r );
	// Compute the sample on the unit sphere
	sample.P = Vec3( r * cos( TwoPi * t ), r * sin( TwoPi * t ), (1-h)*s + h);

	// Rotate the unit vector to be pointing to the current sphere. Computes it
	// with a reflection on the middle vector found avobe

	sample.P = - sample.P;
	sample.P = Reflection( sample.P, reflect );

	// Find the sample on the current sphere by computing ray-sphere intersection.
	// The ray origin is the point P and it direction is the vector just computed

	ray.direction = sample.P;
	ray.origin = P;
	hitgeom.distance = Infinity;
	if( Intersect( ray, hitgeom ) == false )
	{
		// It shouldn't occur
		cout<<"Error, sample should hit the sphere"<<endl;
		return sample;
	}
	// Stores the sample coordinates
	sample.P = hitgeom.point;
	// Stores the sample weight
	sample.w = weight;

    return sample;
}