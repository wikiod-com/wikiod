---
title: "Basic Lighting"
slug: "basic-lighting"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Phong Lighting Model
> NOTE: This example is WIP, it will be updated with diagrams, images,
> more examples, etc.

**What is Phong?**

Phong is a very basic, but real looking light model for surfaces that has three parts: ambient, diffuse, and specular lighting. 

**Ambient Lighting:**

Ambient lighting is the simplest of the three parts to understand and calculate. Ambient lighting is light that floods the scene and lights up the object evenly in all directions. 

The two variables in ambient lighting are the strength of the ambient and the color of the ambient. In your fragment shader, the following will work for ambient:

    in vec3 objColor;
    
    out vec3 finalColor;
    
    uniform vec3 lightColor;
    
    void main() {
       float ambientStrength = 0.3f;
       vec3 ambient = lightColor * ambientStrength;
       finalColor = ambient * objColor;
    }

**Diffuse Lighting:**

Diffuse  lighting is slightly more complex then ambient. Diffuse lighting is directional light, essentially meaning that faces facing towards the light source will be better illuminated and faces pointing away will be darker due to how the light is hitting them. 

*Note: diffuse lighting will require the use of normals for each face which I will not show how to calculate here. If you want to learn how to do this, check out the 3D math page.* 


To model the reflection of light in computer graphics is used a Bidirectional reflectance distribution function (BRDF).
BRDF is a function that gives the relation between the light reflected along an outgoing direction and the light incident from an incoming direction. 

A perfect diffuse surface has a BRDF that has the same value for all incident and outgoing directions. This substantially reduces the computations and thus it is commonly used to model diffuse surfaces as it is physically plausible, even though there are no pure diffuse materials in the real world.  This BRDF is called Lambertian reflection because it obeys Lambert's cosine law. 

Lambertian reflection is often used as a model for diffuse reflection. This technique causes all closed polygons (such as a triangle within a 3D mesh) to reflect light equally in all directions when rendered The diffusion coefficient is calculated from the angle between the normal vector and the light vector.

    f_Lambertian = max( 0.0, dot( N, L )

where `N` is the normal vector of the surface, and `L` is the vector towards to the light source.  

## How it works ##

In general The *dot* product of 2 vectors is equal the *cosine* of the angle between the 2 vectors multiplied by the magnitude (lenght) of both vectors. 

    dot( A, B ) == length( A ) * length( B ) * cos( angle_A_B ) 

This follows, that the *dot* product of 2 unit vectors is equal the *cosine* of the angle between the 2 vectors, because the length of a unit vector is 1.

    uA = normalize( A )
    uB = normalize( B )
    cos( angle_A_B ) == dot( uA, uB )

[![enter image description here][1]][1]

If we take a look at the *cos(x)* function between the angles -90° and 90° then we can see that it has a maximum of 1 at an angle of 0° and It goes down to 0 at the angles of 90° and -90°.

[![enter image description here][2]][2]

This behavior is exactly that what we want for the reflection model. When the nromal vetor of the surface and the diretion to the light source are in the same direction (the angle between is 0°) then we want a maximium of reflection.
In contrast, if the vectors a orthonormalized (the angle in between is 90°) then we want a minimum of reflection and we want a smooth and continuous functional running between the two borders of 0° and 90°.

[![enter image description here][3]][3]

If the light is calculated per vertex, the reflection is calculated for each corner of the primitive. In between the primitives the reflections are interpolate according to its barycentric coordinates.
See the resulting reflections on a spherical surface:

[![enter image description here][4]][4]

Ok, so to start off with our fragment shader, we will need four inputs.

 - Vertex normals (should be in buffer and specified by vertex attribute pointers)
 - Fragment position (should be outputted from vertex shader into frag shader)
 - Light source position (uniform)
 - Light color (uniform)

   

    in vec3 normal;
    in vec3 fragPos;
        
    out vec3 finalColor;
        
    uniform vec3 lightColor;
    uniform vec3 lightPos;
    uniform vec3 objColor;

Inside of main is where we need to do some math. The whole concept of diffuse lighting is based off of the angle between the normal and the light direction. The greater the angle, the less light there is until 90° where there is no light at all.

Before we can begin calculating the amount of light, we need the light direction vector. This can be retrieved by simply subtracting the light position from the fragment position which returns a vector from the light position pointing to the fragment position. 

    vec3 lightDir = lightPos-fragPos;

Also, go ahead and normalize the `normal` and `lightDir` vectors so they are the same length to work with.

    normal  = normalize(normal);
    lightDir = normalize(lightDir);
Now that we have our vectors, we can calculate the difference between them. To do this, we are going to use the dot product function. Basically, this takes 2 vectors and returns the cos() of the angle formed. This is perfect because at 90 degrees it will yield 0 and at 0 degrees it will yield 1. As a result, when the light is pointing directly at the object it will be fully lit and vice versa.

    float diff = dot(normal, lightDir);

There is one more thing we have to do to the calculated number, we need to make sure it is always positive. If you think about it, a negative number doesn't make sense in context because that means the light is behind the face. We could use an if statement, or we can use the `max()` function which returns the maximum of two inputs. 

    diff = max(diff, 0.0);
With that done, we are now ready to calculate the final output color for the fragment. 

    vec3 diffuse = diff * lightColor;
    finalColor = diffuse * objColor;
It should look like this:
[![enter image description here][5]][5]

**Specular Lighting:**

> Work in progres, check back later.

**Combined**

> Work in progress, check back later.

The below code and image show these three lighting concepts combined. 


  [1]: https://i.stack.imgur.com/yc5B0.png
  [2]: https://i.stack.imgur.com/rzKtB.png
  [3]: https://i.stack.imgur.com/yOx2S.png
  [4]: https://i.stack.imgur.com/Gyl11.png
  [5]: http://i.stack.imgur.com/03p1T.png

