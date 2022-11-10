#include "windows.h"
#include "AppMain.h"

void Keyboard(unsigned char tecla, int x, int y)
{
	if (tecla == 27) 
		exit(0);
}

void Draw( void )
{
	g_raytracer.draw();
}

void Idle( void )
{
	if (!g_raytracer.IsDone())
	{
		g_raytracer.cast_line( w );
		glFlush(); 
		glutPostRedisplay();
	}
	else
	{
		Sleep (100);
		glFlush(); 
		glutPostRedisplay();
	}
}

void main(int argc, char** argv)
{
	glutInit(&argc, argv);
	glutInitDisplayMode( GLUT_SINGLE | GLUT_RGBA);
	glutInitWindowPosition(200,100);
	glutInitWindowSize( RESOLUTIONX , RESOLUTIONY );
	glutCreateWindow("Raytracer");

	glClearColor (0.0, 0.0, 0.0, 0.0);

	if ( w.readScene("escena.sdf") )
	{
		glutKeyboardFunc( Keyboard );
		glutIdleFunc( Idle );
		glutDisplayFunc( Draw );	
		glutMainLoop();
	}
}