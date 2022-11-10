#include "Polygon.h"

//=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~//
// Polygon object.                                                        //
//                                                                        //
// The polygon object is defined by 5 vertices in R3.  This is a          //
// simple flat polygon.                                                   //
//=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~//

Polygon::Polygon( const Vec3 &A_, const Vec3 &B_, const Vec3 &C_, const Vec3 &D_, const Vec3 &E_ )
    {

	// Assign values to the corner points
	A = A_;
	B = B_;
	C = C_;
	D = D_;
	E = E_;

	// Compute the normal to plane of the triangle
	// Cross product between two vectors created by points fo the triangle

    N = Unit( ( C - B ) ^ ( A - B ) );

	// Compute the distance from origin to plane of triangle.
	// This equals to D on the plane coordinates

    d = -( A.x * N.x + A.y * N.y + A.z * N.z );

	// Computes the bounding box;

	// Initiallizes the box coordinates
    box.X.min = box.X.max  = A.x;
    box.Y.min = box.Y.max  = A.y;
	box.Z.min = box.Z.max  = A.z;
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
	// Check D coordinates
	if( D.x < box.X.min ) box.X.min = D.x;
	else if( D.x > box.X.max ) box.X.max = D.x;
	if( D.y < box.Y.min ) box.Y.min = D.y;
	else if( D.y > box.Y.max ) box.Y.max = D.y;
	if( D.z < box.Z.min ) box.Z.min = D.z;
	else if( D.z > box.Z.max ) box.Z.max = D.z;
	// Check E coordinates
	if( E.x < box.X.min ) box.X.min = E.x;
	else if( E.x > box.X.max ) box.X.max = E.x;
	if( E.y < box.Y.min ) box.Y.min = E.y;
	else if( E.y > box.Y.max ) box.Y.max = E.y;
	if( E.z < box.Z.min ) box.Z.min = E.z;
	else if( E.z > box.Z.max ) box.Z.max = E.z;

    next = NULL;
    }

Object *Polygon::ReadString( const char *params ) // Read params from string.
    {
    float ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez;
    if( sscanf( params, "polygon (%f,%f,%f) (%f,%f,%f) (%f,%f,%f) (%f,%f,%f) (%f,%f,%f)", 
        &ax, &ay, &az, &bx, &by, &bz, &cx, &cy, &cz, &dx, &dy, &dz, &ex, &ey, &ez ) == 15 )
        return new Polygon( Vec3( ax, ay, az ), Vec3( bx, by, bz ), Vec3( cx, cy, cz ), Vec3( dx, dy, dz ), Vec3( ex, ey, ez ) );
    return NULL;
    }

Box3 Polygon::GetBounds() const // Return pre-computed box.
    {
    return box;
    }

bool Polygon::Intersect( const Ray &ray, HitGeom &hitgeom ) const
    {
	return false;
	}