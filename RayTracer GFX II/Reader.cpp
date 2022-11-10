#include "Reader.h"

bool Reader::Get( const char *line, const char *name, Vec3 &coord )
{
	sprintf( format, "%s (%%lf,%%lf,%%lf)", name );
	return sscanf( line, format, &coord.x, &coord.y, &coord.z ) == 3;
}

bool Reader::Get( const char *line, const char *name, Color &color )
{
	sprintf( format, "%s [%%lf,%%lf,%%lf]", name );
	return sscanf( line, format, &color.red, &color.green, &color.blue ) == 3;
}

bool Reader::Get( const char *line, const char *name, float &value )
{
	sprintf( format, "%s %%f", name );
	return sscanf( line, format, &value ) == 1;
}

bool Reader::Blank( char *line )
{
	if( *line == '#' ) return true;  // Comment lines start with '#'
	while( *line != '\n' )
	{
		if( *line != ' ' && *line != '\t' ) return false;
		line++;
	}
	return true;
}

// This is a very minimal scene description reader.  It assumes that
// each line contains a complete entity: an object definition,
// a camera parameter, a material parameter, etc.  (Blank lines, and
// lines that begin with "#" are also okay.)  It fills in the fields of
// the scene and camera as it parses the file.
bool Reader::ReadSceneDescription( const char *file_name, Scene &scene, Camera &camera )
{
	static char buff[512];
	Object *newobj;
	Object *obj = NULL;
	int line_num = 0;

	FILE *fp = fopen( file_name, "r" );
	if( fp == NULL ) return false;

	// Begin looping over all the lines of the file.
	// Keep processing lines until the end of file is reached, or
	// we find a line that is unrecognizable.

	for(;;)
	{
		char *line = fgets( buff, 512, fp );
		if( line == NULL ) break;
		if( Blank(line) ) continue;
		line_num++;

		// Ask each object if it recognizes the line.  If it does, it will
		// create a new instance of the object and return it as the function
		// value.

        if( ( newobj = Sphere  ::ReadString( line )) != NULL ) { newobj->next = obj; obj = newobj; continue; }
        if( ( newobj = Cube    ::ReadString( line )) != NULL ) { newobj->next = obj; obj = newobj; continue; }
        if( ( newobj = Triangle::ReadString( line )) != NULL ) { newobj->next = obj; obj = newobj; continue; }
        if( ( newobj = Polygon ::ReadString( line )) != NULL ) { newobj->next = obj; obj = newobj; continue; }

		// Now look for all the other stuff...  materials, camera,
		// lights, etc.

		if( Get( line, "diffuse"     , obj->material.m_Diffuse		) ) continue;
		if( Get( line, "specular"    , obj->material.m_Specular		) ) continue;
		if( Get( line, "reflectivity", obj->material.m_Reflectivity	) ) continue;
		if( Get( line, "refractive_index", obj->material.m_RefractiveIndex	) ) continue;
		if( Get( line, "opacity"		 , obj->material.m_Opacity			) ) continue;
		if( Get( line, "Phong_exp"   , obj->material.m_Phong_exp	) ) continue;
        if( Get( line, "emission"    , obj->material.m_Emission     ) ) continue;
		if( Get( line, "eye"         , camera.eye			        ) ) continue;
		if( Get( line, "lookat"      , camera.lookat			    ) ) continue;            
		if( Get( line, "up"          , camera.up					) ) continue;            
		if( Get( line, "vpdist"      , camera.vpdist				) ) continue;            
		if( Get( line, "lightpos"  , scene.light[scene.num_lights].m_Position ) )continue;
		// Only increments light number after reading lightcolor
		// It is assumed that lightcolor is always placed after lightpos in the scene file
		if( Get( line, "lightcolor"    , scene.light[scene.num_lights].m_Color  ) ){
			scene.num_lights++;
			continue;            
		}
		if( Get( line, "amblight"    , scene.ambient				) ) continue;            
		if( Get( line, "bgcolor"     , scene.bgcolor			    ) ) continue;  

		cerr << "Error reading scene file, line " << line_num 
			 << ": " << line << endl;
		return false;
	}

	scene.first = obj;
	cout << "done reading file." << endl;
	return true;
}