---
title: "OGL view and projection"
slug: "ogl-view-and-projection"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

About model matrix, view matrix, orthographic- and perspective projection

## Implement a camera in OGL 4.0 GLSL 400
If we want to look at a scene as if we had photographed it with a camera, we must first define some things:

  - The position from which the scene is viewed, the eye position `pos`.
  - The point we look at in the scene (`target`). 
    It is also common to define the direction in which we look. Technically we need a *line of sight*.
    One straight in space is mathematically defined either by 2 points or by a point and a vector.
    The first part of the definition is the eye position and the 2nd is either the `target` or the line of sight vector `los`.
  - The direction upwards `up`.
  - The field of view '`fov_y`.
    This means the angle between the two straight lines, starting at the eye position and ending at the leftmost point
    and the rightmost point, which can be seen simultaneously.
  - The large and the aspect ratio of the viewport to which we project our image `vp`.
  - The *near plane* `near` and the *far plane* `far`. The *near plane* is distance from the eye position to the plane
    from where the objects become visible to us. The *far plane* is the distance from the eye position to the plane
    to which the objects of the scene are visible to us.
    An explanation of what the *near plane* and *far plane* are needed will follow later..

A definition of this data in C++ and in Python may look like this:

**C++**

    using TVec3 = std::array<float,3>;
    struct Camera
    {
        TVec3 pos    {0.0, -8.0, 0.0};
        TVec3 target {0.0, 0.0, 0.0};
        TVec3 up     {0.0, 0.0, 1.0};
        float fov_y  {90.0};
        TSize vp     {800, 600};
        float near   {0.5};
        float far    {100.0};
    };

**Python**

    class Camera:
        def __init__(self):
            self.pos    = (0, -8, 0)
            self.target = (0, 0, 0)
            self.up     = (0, 0, 1)
            self.fov_y  = 90
            self.vp     = (800, 600)
            self.near   = 0.5
            self.far    = 100.0

In order to take all this information into consideration when drawing a scene, a projection matrix and a view matrix are usually used. In order to arrange the individual parts of a scene in the scene, model matrices are used.
However, these are mentioned here only for the sake of completeness and will not be dealt with here.

- Projection matrix:
  The projection matrix describes the mapping from 3D points in the world as they are seen from of a pinhole camera, to 2D points of the viewport.

- View matrix:
  The view matrix defines the *eye* position and the viewing direction on the scene.

- Model matrix:
  The model matrix defines the location and the relative size of an object in the scene.
     
After we have filled the data structures above with the corresponding data, we have to translate them into the appropriate matrices.
In the OGL compatibility mode, this can be done with the `gluLookAt` and `gluPerspective` functions that set the built in uniforms `gl_ModelViewMatrix`, `gl_NormalMatrix`, and `gl_ModelViewProjectionMatrix`.
In OGL 3.1 and GLSL #version 150 the built in uniforms were removed, because the entire fixed-function matrix stack became deprecated.
If we want to use OGL high level shader with GLSL version 330 or even higher we have to define and set the matrix uniforms
our self (Apart from the use of GLSL `compatibility` keyword).

## Set up the perspective - Projection matrix ##

A point on the viewport is visible when it is in the native AABB (axis aligned bounding box) defined by the points
`(-1.0, -1.0, -1.0)` and `(1.0, 1.0, 1.0)`. This is called the Normalized Device Coordinates (NDC).
A point with the coordinates `(-1.0, -1.0, z)` will be painted to the lower left corner of the viewport and a point
with the coordinates `(1.0, 1.0, z)` will be painted to the upper right corner of the viewport.
The Z-coordinate is mapped from the interval (-1.0, 1.0) to the interval (0.0, 1.0) and written into the Z-buffer.

All we can see from the scene is within a 4-sided pyramid. The top of the pyramid is the *eye position*. The 4 sides of
the pyramid are defined by the filed of view (`fov_y`) and the aspect ratio (`vp[0]/vp[1]`).
The projection matrix has to map the points from inside the pyramid to the NDC defined by the points
`(-1.0, -1.0, -1.0)` and `(1.0, 1.0, 1.0)`.
At this point our pyramid is infinite, it has no end in depth and we can not map an infinite space to a finite one.
For this we now need the *near plane* and the *far plane*, they transform the pyramid into a frustum by cutting the top
and limiting the pyramid in the depth.
The near plane and the far plane have to be chosen in such a way that they include everything that should be visible
from the scene.

[![enter image description here][1]][1]

The mapping from the points within a frustum to the NDC is pure mathematics and can be generally solved. 
The development of the formulas were often discussed and repeatedly published throughout the web.
Since you can not insert a LaTeX formula into a Stack Overflow documentation this is dispensed with here
and only the completed C++ and Python source code is added.
Note that the eye coordinates are defined in the right-handed coordinate system, but NDC uses the left-handed
coordinate system.
The projection matrix is calculated from, the *field of view* `fov_y`, the *aspect ratio* `vp[0]/vp[1]`,
the *near plane* `near`and the *far plane* `far`. 

**C++**

    using TVec4  = std::array< float, 4 >;
    using TMat44 = std::array< TVec4, 4 >;
    TMat44 Camera::Perspective( void )
    {
        float fn  = far + near;
        float f_n = far - near;
        float r   = (float)vp[0] / vp[1];
        float t   = 1.0f / tan( ToRad( fov_y ) / 2.0f );
        return TMat44{ 
            TVec4{ t / r, 0.0f,  0.0f,                     0.0f },
            TVec4{ 0.0f,  t,     0.0f,                     0.0f },
            TVec4{ 0.0f,  0.0f, -fn / f_n,                -1.0  },
            TVec4{ 0.0f,  0.0f, -2.0f * far * near / f_n,  0.0f } };
    }

**Python**

    def Perspective(self):
        fn, = self.far + self.near
        f_n = self.far - self.near
        r = self.vp[0] / self.vp[1]
        t = 1 / math.tan( math.radians( self.fov_y ) / 2 )
        return numpy.matrix( [ 
            [ t/r, 0,  0,                               0 ],
            [ 0,   t,  0,                               0 ],
            [ 0,   0, -fn/f_n,                         -1 ],
            [ 0,   0, -2 * self.far * self.near / f_n,  0 ] ] )

## Set up the look at the scene - View matrix ##

In the coordinate system on the viewport, the Y-axis points upwards `(0, 1, 0)` and the X axis
points to the right `(1, 0, 0)`. This results a Z-axis which points out of the viewport
( `(0, 0, -1) = cross( X-axis, Y-axis )`).

In the scene, the X axis points to the east, the Y axis to the north, and the Z axis to the top.

[![enter image description here][2]][2]

The X axis of the viewport `(1, 0, 0)` matches the Y-axis of the scene `(1, 0, 0)`,
the Y axis of the viewport `(0, 1, 0 )` matches the Z axis of the scene `(0, 0, 1)` and
the Z axis of the viewport `(0, 0, 1 )` matches the negated Y axis of the scene `(0, -1, 0)`.

Each point and each vector from the reference system of the scene must therefore be converted first into viewport coordinates.
This can be done by some swapping and inverting operations in the the scalar vectors.

     x  y  z
    --------
     1  0  0  | x' =  x
     0  0  1  | y' =  z
     0 -1  0  | z' = -y

To setup a view matrix  the position `pos`, the target `target` and the up vector `up` have to be mapped into the viewport coordinate system, as described above. This give the 2 points `p` and `t` and the vector `u`, as in the following code snippet.
The Z axis of the view matrix is the inverse line of sight, which is calculated by `p - t`. 
The Y axis is the up vector `u`. The X axis is calculated by the cross product of Y axis and Z axis.
For orthonormalising the view matrix, the cross product is used a second time, to calculate the Y axis from the Z axis and the X axis (Of course the Gram-Schmidt orthogonalization would work just as well). At the end, all 3 axles must be normalized and the *eye position* `pos` has to be set as th origin of the view matrix.

The code below defines a matrix that exactly encapsulates the steps necessary to calculate a look at the scene:
1. Converting model coordinates into viewport coordinates.
2. Rotate in the direction of the direction of view.
3. Movement to the eye position

**C++**

    template< typename T_VEC >
    TVec3 Cross( T_VEC a, T_VEC b )
    {
        return { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
    }

    template< typename T_A, typename T_B >
    float Dot( T_A a, T_B b )
    { 
        return a[0]*b[0] + a[1]*b[1] + a[2]*b[2]; 
    }

    template< typename T_VEC >
    void Normalize( T_VEC & v )
    { 
        float len = sqrt( v[0] * v[0] + v[1] * v[1] + v[2] * v[2] ); v[0] /= len; v[1] /= len; v[2] /= len; 
    }

    TMat44 Camera::LookAt( void )
    { 
        TVec3 mz = { pos[0] - target[0], pos[1] - target[1], pos[2] - target[2] };
        Normalize( mz );
        TVec3 my = { up[0], up[1], up[2] };
        TVec3 mx = Cross( my, mz );
        Normalize( mx );
        my = Cross( mz, mx );
  
        TMat44 v{
            TVec4{ mx[0], my[0], mz[0], 0.0f },
            TVec4{ mx[1], my[1], mz[1], 0.0f },
            TVec4{ mx[2], my[2], mz[2], 0.0f },
            TVec4{ Dot(mx, pos), Dot(my, pos), Dot(TVec3{-mz[0], -mz[1], -mz[2]}, pos), 1.0f }
        };
  
        return v;
    }

**python**

    def LookAt(self):
        mz = Normalize( (self.pos[0]-self.target[0], self.pos[1]-self.target[1], self.pos[2]-self.target[2]) ) # inverse line of sight
        mx = Normalize( Cross( self.up, mz ) )
        my = Normalize( Cross( mz, mx ) )
        tx = Dot( mx, self.pos )
        ty = Dot( my, self.pos )
        tz = Dot( (-mz[0], -mz[1], -mz[2]), self.pos )   
        return = numpy.matrix( [ 
            [mx[0], my[0], mz[0], 0],
            [mx[1], my[1], mz[1], 0],
            [mx[2], my[2], mz[2], 0],
            [tx, ty, tz, 1] ] )
        


The matrices are finally written in uniforms and used in the vertex shader to transform the model positions.

**Vertex shader**

In the vertex shader, one transformation after the other is performed.
1. The model matrix brings the object (mesh) to its place in the scene.
   (This is only listed for the sake of completeness and has not been documented here since it has nothing to do with the view of to the scene)
2. The view matrix defines the direction from which the scene is viewed.
   The transformation with the view matrix rotates the objects of the scene so that they are viewed from the desired
   direction of view with reference to the coordinate system of the viewport.
3. The projection matrix transforms the objects from a parallel view into a perspective view. 


    #version 400

    layout (location = 0) in vec3 inPos;
    layout (location = 1) in vec3 inCol;
    
    out vec3 vertCol;
    
    uniform mat4 u_projectionMat44;
    uniform mat4 u_viewMat44;
    uniform mat4 u_modelMat44;
    
    void main()
    {
        vertCol       = inCol;
        vec4 modelPos = u_modelMat44 * vec4( inPos, 1.0 );
        vec4 viewPos  = u_viewMat44 * modelPos;
        gl_Position   = u_projectionMat44 * viewPos;
    }

**Fragment shader**

The fragment shader is listed here only for the sake of completeness.
The work was done before.

    #version 400
    
    in vec3 vertCol;
    
    out vec4 fragColor;
    
    void main()
    {
        fragColor = vec4( vertCol, 1.0 );
    }

After the shader are compiled and liked, the matrices can bound to the uniform variables.

**C++**

    int shaderProg = ;
    Camera camera;

    // ...

    int prjMatLocation  = glGetUniformLocation( shaderProg, "u_projectionMat44" );
    int viewMatLocation = glGetUniformLocation( shaderProg, "u_viewMat44" );
    glUniformMatrix4fv( prjMatLocation,  1, GL_FALSE, camera.Perspective().data()->data() );
    glUniformMatrix4fv( viewMatLocation, 1, GL_FALSE, camera.LookAt().data()->data() );

**Python**

    shaderProg =
    camera = Camera()

    # ...

    prjMatLocation  = glGetUniformLocation( shaderProg, b"u_projectionMat44" )
    viewMatLocation = glGetUniformLocation( shaderProg, b"u_viewMat44" )
    glUniformMatrix4fv( prjMatLocation,  1, GL_FALSE, camera.Perspective() )
    glUniformMatrix4fv( viewMatLocation, 1, GL_FALSE, camera.LookAt() )


In addition, I have added the entire code dump of a Python example (To add the C ++ example would unfortunately exceed the limit of 30000 characters).
In this example, the camera moves elliptically around a tetrahedron at a focal point of the ellipse.
The viewing direction is always directed to the tetraeder.

**Python**

To run the Python script [NumPy](http://www.numpy.org/) must be installed.

    from OpenGL.GL import *
    from OpenGL.GLUT import *
    from OpenGL.GLU import *
    import numpy
    from time import time
    import math
    import sys
    
    def Cross( a, b ): return ( a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0], 0.0 )
    def Dot( a, b ): return a[0]*b[0] + a[1]*b[1] + a[2]*b[2]
    def Normalize( v ): 
        len = math.sqrt( v[0] * v[0] + v[1] * v[1] + v[2] * v[2] )
        return (v[0] / len, v[1] / len, v[2] / len)
    
    class Camera:
        def __init__(self):
            self.pos    = (0, -8, 0)
            self.target = (0, 0, 0)
            self.up     = (0, 0, 1)
            self.fov_y  = 90
            self.vp     = (800, 600)
            self.near   = 0.5
            self.far    = 100.0
        def Perspective(self):
            fn, f_n = self.far + self.near, self.far - self.near
            r, t = self.vp[0] / self.vp[1], 1 / math.tan( math.radians( self.fov_y ) / 2 )
            return numpy.matrix( [ [t/r,0,0,0], [0,t,0,0], [0,0,-fn/f_n,-1], [0,0,-2*self.far*self.near/f_n,0] ] )
        def LookAt(self):
             mz = Normalize( (self.pos[0]-self.target[0], self.pos[1]-self.target[1], self.pos[2]-self.target[2]) ) # inverse line of sight
        mx = Normalize( Cross( self.up, mz ) )
        my = Normalize( Cross( mz, mx ) )
        tx = Dot( mx, self.pos )
        ty = Dot( my, self.pos )
        tz = Dot( (-mz[0], -mz[1], -mz[2]), self.pos )   
        return = numpy.matrix( [ [mx[0], my[0], mz[0], 0], [mx[1], my[1], mz[1], 0], [mx[2], my[2], mz[2], 0], [tx, ty, tz, 1] ] )
        
    # shader program object
    class ShaderProgram:
        def __init__( self, shaderList, uniformNames ):
            shaderObjs = []
            for sh_info in shaderList: shaderObjs.append( self.CompileShader(sh_info[0], sh_info[1] ) )
            self.LinkProgram( shaderObjs )
            self.__unifomLocation = {}
            for name in uniformNames:
                self.__unifomLocation[name] = glGetUniformLocation( self.__prog, name )
                print( "uniform %-30s at loaction %d" % (name, self.__unifomLocation[name]) )
        def Use(self):
            glUseProgram( self.__prog )
        def SetUniformMat44( self, name, mat ):
            glUniformMatrix4fv( self.__unifomLocation[name], 1, GL_FALSE, mat )
        # read shader program and compile shader
        def CompileShader(self, sourceFileName, shaderStage):
            with open( sourceFileName, 'r' ) as sourceFile:
                sourceCode = sourceFile.read()
            nameMap = { GL_VERTEX_SHADER: 'vertex', GL_FRAGMENT_SHADER: 'fragment' }    
            print( '\n%s shader code:' % nameMap.get( shaderStage, '' ) )
            print( sourceCode )
            shaderObj = glCreateShader( shaderStage )
            glShaderSource( shaderObj, sourceCode )
            glCompileShader( shaderObj )
            result = glGetShaderiv( shaderObj, GL_COMPILE_STATUS )
            if not (result):
                print( glGetShaderInfoLog( shaderObj ) )
                sys.exit()
            return shaderObj
        # linke shader objects to shader program
        def LinkProgram(self, shaderObjs):
            self.__prog = glCreateProgram()
            for shObj in shaderObjs: glAttachShader( self.__prog, shObj )
            glLinkProgram( self.__prog )
            result = glGetProgramiv( self.__prog, GL_LINK_STATUS )
            if not ( result ):
                print( 'link error:' )
                print( glGetProgramInfoLog( self.__prog ) )
                sys.exit()
    
    # vertex array object
    class VAObject:
        def __init__( self, dataArrays, tetIndices ):
            self.__obj = glGenVertexArrays( 1 )
            self.__noOfIndices = len( tetIndices )
            self.__indexArr = numpy.array( tetIndices, dtype='uint' )
            noOfBuffers = len( dataArrays )
            buffers = glGenBuffers( noOfBuffers )
            glBindVertexArray( self.__obj )
            for i_buffer in range( 0, noOfBuffers ):
                vertexSize, dataArr = dataArrays[i_buffer]
                glBindBuffer( GL_ARRAY_BUFFER, buffers[i_buffer] )
                glBufferData( GL_ARRAY_BUFFER, numpy.array( dataArr, dtype='float32' ), GL_STATIC_DRAW )
                glEnableVertexAttribArray( i_buffer )
                glVertexAttribPointer( i_buffer, vertexSize, GL_FLOAT, GL_FALSE, 0, None )
        def Draw(self):
            glBindVertexArray( self.__obj )
            glDrawElements( GL_TRIANGLES, self.__noOfIndices, GL_UNSIGNED_INT, self.__indexArr )
    
    # glut window
    class Window:
        def __init__( self, cx, cy ):
            self.__vpsize = ( cx, cy )
            glutInitDisplayMode( GLUT_RGBA | GLUT_DOUBLE | GLUT_ALPHA | GLUT_DEPTH )
            glutInitWindowPosition( 0, 0 )
            glutInitWindowSize( self.__vpsize[0], self.__vpsize[1] )
            self.__id = glutCreateWindow( b'OGL window' ) 
            glutDisplayFunc( self.OnDraw ) 
            glutIdleFunc( self.OnDraw )
        def Run( self ):
            self.__startTime = time()
            glutMainLoop()
    
        # draw event
        def OnDraw(self):
            self.__vpsize = ( glutGet( GLUT_WINDOW_WIDTH ), glutGet( GLUT_WINDOW_HEIGHT ) )
            currentTime = time()
            # set up camera
            camera = Camera()
            camera.vp = self.__vpsize
            camera.pos = self.EllipticalPosition( 7, 4, self.CalcAng( currentTime, 10 ) )
            
            # set up attributes and shader program
            glEnable( GL_DEPTH_TEST )
            glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )
            prog.Use()
            prog.SetUniformMat44( b"u_projectionMat44", camera.Perspective()  )
            prog.SetUniformMat44( b"u_viewMat44", camera.LookAt() )
            
            # draw object
            modelMat = numpy.matrix(numpy.identity(4), copy=False, dtype='float32')
            prog.SetUniformMat44( b"u_modelMat44", modelMat )
            tetVAO.Draw()
        
            glutSwapBuffers()
    
        def Fract( self, val ): return val - math.trunc(val)
        def CalcAng( self, currentTime, intervall ): return self.Fract( (currentTime - self.__startTime) / intervall ) * 2.0 *     math.pi
        def CalcMove( self, currentTime, intervall, range ):
            pos = self.Fract( (currentTime - self.__startTime) / intervall ) * 2.0
            pos = pos if pos < 1.0 else (2.0-pos)
            return range[0] + (range[1] - range[0]) * pos
        def EllipticalPosition( self, a, b, angRag ):
            a_b = a * a - b * b
            ea = 0 if (a_b <= 0) else math.sqrt( a_b )
            eb = 0 if (a_b >= 0) else math.sqrt( -a_b )
            return ( a * math.sin( angRag ) - ea, b * math.cos( angRag ) - eb, 0 )
    
    # initialize glut
    glutInit()
    
    # create window
    wnd = Window( 800, 600 )
    
    # define tetrahedron vertex array opject
    sin120 = 0.8660254
    tetVAO = VAObject(
        [ (3, [ 0.0, 0.0, 1.0, 0.0, -sin120, -0.5, sin120 * sin120, 0.5 * sin120, -0.5, -sin120 * sin120, 0.5 * sin120, -0.5 ]),
          (3, [ 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, ])
        ], 
        [ 0, 1, 2, 0, 2, 3, 0, 3, 1, 1, 3, 2 ]
    )
    
    # load, compile and link shader
    prog = ShaderProgram( 
        [ ('python/ogl4camera/camera.vert', GL_VERTEX_SHADER),
          ('python/ogl4camera/camera.frag', GL_FRAGMENT_SHADER)
        ],
        [b"u_projectionMat44", b"u_viewMat44", b"u_modelMat44"] ) 
    
    # start main loop
    wnd.Run()


  [1]: https://i.stack.imgur.com/4JNS2.png
  [2]: https://i.stack.imgur.com/2LSzg.png

