#ifndef CAMERA_H
#define CAMERA_H

class Camera // Defines the position of the camera.
{     
	public:
		Vec3  eye;          // Position of eye.
		Vec3  lookat;       // The point we are looking toward.
		Vec3  up;           // A vector not parallel to the direction of gaze.
		float vpdist;       // Distance to the view plane.
};

#endif