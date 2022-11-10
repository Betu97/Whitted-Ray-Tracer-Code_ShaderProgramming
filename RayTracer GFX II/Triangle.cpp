#include "Triangle.h"

//=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~//
// Triangle object.                                                       //
//                                                                        //
// The triangle object is defined by three vertices in R3.  This is a     //
// simple flat triangle with no normal vector interpolation.  The         //
// triangle structure is defined to accommodate the barycentric coord     //
// method of intersecting a ray with a triangle.                          //
//=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~//

Triangle::Triangle( const Vec3 &A_, const Vec3 &B_, const Vec3 &C_ )
    {
	float base;		// Length of the base of the triangle
	float height;	// Height of the triangle
	Vec3 N_pos;		// Normal with all values positive

	// Assign values to the corner points
	A = A_;
	B = B_;
	C = C_;

	// Compute the normal to plane of the triangle
	// Cross product between two vectors created by points fo the triangle

    N = Unit( ( C - B ) ^ ( A - B ) );

	// Compute the distance from origin to plane of triangle.
	// This equals to D on the plane coordinates

    d = -( A.x * N.x + A.y * N.y + A.z * N.z );

	// Computes the bounding box;

	// Initiallizes the box coordinates
    box.X.min = box.X.min  = A.x;
    box.Y.min = box.Y.min  = A.y;
    box.Z.min = box.Z.min  = A.z;
	// Check B coordinates
	if( B.x < box.X.min ) box.X.min = B.x;
	else if( B.x > box.X.max ) box.X.max = B.x;
	if( B.y < box.Y.min ) box.Y.min = B.y;
	else if( B.y > box.Y.max ) box.Y.max = B.y;
	if( B.z < box.Z.min ) box.Z.min = B.z;
	else if( B.z > box.Z.max ) box.Z.max = B.z;
	// Check C coordinates
	if( C.x < box.X.min ) box.X.min = C.x;
	else if( C.x > box.X.max ) box.X.max = C.x;
	if( C.y < box.Y.min ) box.Y.min = C.y;
	else if( C.y > box.Y.max ) box.Y.max = C.y;
	if( C.z < box.Z.min ) box.Z.min = C.z;
	else if( C.z > box.Z.max ) box.Z.max = C.z;

	// Compute absolute value for all components of the normal
	N_pos = N;
	N_pos.x = fabs( N_pos.x );
	N_pos.y = fabs( N_pos.y );
	N_pos.z = fabs( N_pos.z );
	
	if( N_pos.x >= N_pos.y && N_pos.x >= N_pos.z )
		axis = 0;
	else if( N_pos.y >= N_pos.x && N_pos.y >= N_pos.z )
		axis = 1;
	else if( N_pos.z >= N_pos.x && N_pos.z >= N_pos.y )
		axis = 2;
	
	// Compute the area

	// First compute the base of the triangle
	base = (float) dist( B, A );

	// Then compute the height of the triangle
	// It is computed as an point to line distance
	height = (float) Length( ( B - A ) ^ ( A - C ) ) / (float) Length( B - A );

	// Therefore, the area is:
	area = base * height * 0.5f;

    next = NULL;
    }

Object *Triangle::ReadString( const char *params ) // Read params from string.
    {
    float ax, ay, az, bx, by, bz, cx, cy, cz;
    if( sscanf( params, "triangle (%f,%f,%f) (%f,%f,%f) (%f,%f,%f)", 
        &ax, &ay, &az, &bx, &by, &bz, &cx, &cy, &cz ) == 9 )
        return new Triangle( Vec3( ax, ay, az ), Vec3( bx, by, bz ), Vec3( cx, cy, cz ) );
    return NULL;
    }

Box3 Triangle::GetBounds() const // Return pre-computed box.
    {
    return box;
    }

bool Triangle::Intersect( const Ray &ray, HitGeom &hitgeom ) const
    {
	Vec3 ab = B - A;
	Vec3 ac = C - A;
	Vec3 qp = ray.origin - (ray.origin + ray.direction * hitgeom.distance);
	Vec3 n = ab ^ ac;
	float d = qp * n;
	if (d <= 0.0f) {
			return false;
	}


	Vec3 ap = ray.origin - A;
	double t = ap * n;
	if (t < 0.0f) {
		return false;
	}
	if (t > d) {
		return false;
	}

	//Compute barycentric coordinate components and test if within bounds
	Vec3 e = qp ^ ap;
	float v = ac * e;
	if (v < 0.0f || v > d) {
		return false;
	}
	float w = -(ab * e);
	if (w < 0.0f || v + w > d) {
		return false;
	}

	//Compute the last barycentric coordinate component
	float ood = 1.0f / d;
	t *= ood;
	v *= ood;
	w *= ood;
	float u = 1.0f - v - w;

	Vec3 T = A + v * ab + w * ac;
	Vec3 R = ray.origin + t * qp;


	hitgeom.normal = n;
	hitgeom.origin = ray.origin;
	hitgeom.distance = dist(R, T);
	hitgeom.point = T;
	return true;
	//return false;
	}

Sample Triangle::GetSample( const Vec3 &P, const Vec3 &N_point ) const
{
	Sample sample;
	return sample;
}