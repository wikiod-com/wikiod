---
title: "3d Math"
slug: "3d-math"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Introduction to matrices
When you are programming in OpenGL or any other graphics api you will hit a brick wall when you are not that good in math. Here I will explain with example code how you can achieve movement/scaling and many other cool stuff with your 3d object.

Let's take a real life case... You've made a awesome (three dimensional) cube in OpenGL and you want to move it to any direction. 

    glUseProgram(cubeProgram)
    glBindVertexArray(cubeVAO)
    glEnableVertexAttribArray ( 0 );
    glDrawArrays ( GL_TRIANGLES, 0,cubeVerticesSize)
    
In game engines like Unity3d this would be easy. You would just call transform.Translate() and be done with it, but OpenGL does not include a math library.

A good math library is [glm][1] but to get my point across I will code all the (important) mathematical methods for you out.

First we must understand that a 3d object in OpenGL contains a lot of information, there are many variables that depend on each other. A smart way to manage all these variables is by using matrices.

A matrix is a collection of variables written in columns and rows. A matrix can be 1x1, 2x4 or any arbitrary number.

    [1|2|3]
    [4|5|6]
    [7|8|9] //A 3x3 matrix

You can do really cool stuff with them... but how can they help me with moving my cube? To actually understand this we first need to know several things.

 - How do you make a matrix from a position?
 - How do you translate a matrix?
 - How do you pass it to OpenGL?

Let's make a class containing all our important matrix data and methods
(written in c++)


    template<typename T>
    //Very simple vector containing 4 variables
    struct Vector4{
        T x, y, z, w;
        Vector4(T x, T y, T z, T w) : x(x), y(y), z(z), w(w){}
        Vector4(){}
    
        Vector4<T>& operator=(Vector4<T> other){
            this->x = other.x;
            this->y = other.y;
            this->z = other.z;
            this->w = other.w;
            return *this;
        }
    }

    template<typename T>
    struct Matrix4x4{
         /*!
         *  You see there are columns and rows like this
         */
        Vector4<T> row1,row2,row3,row4;
    
        /*!
         *  Initializes the matrix with a identity matrix. (all zeroes except the ones diagonal)
         */
        Matrix4x4(){
            row1 = Vector4<T>(1,0,0,0);
            row2 = Vector4<T>(0,1,0,0);
            row3 = Vector4<T>(0,0,1,0);
            row4 = Vector4<T>(0,0,0,1);
        }
    
        static Matrix4x4<T> identityMatrix(){
            return Matrix4x4<T>(
                            Vector4<T>(1,0,0,0),
                            Vector4<T>(0,1,0,0),
                            Vector4<T>(0,0,1,0),
                            Vector4<T>(0,0,0,1));
        }
    

    
        Matrix4x4(const Matrix4x4<T>& other){
            this->row1 = other.row1;
            this->row2 = other.row2;
            this->row3 = other.row3;
            this->row4 = other.row4;
        }
    
        Matrix4x4(Vector4<T> r1, Vector4<T> r2, Vector4<T> r3, Vector4<T> r4){
            this->row1 = r1;
            this->row2 = r2;
            this->row3 = r3;
            this->row4 = r4;
        }

          /*!
         *  Get all the data in an Vector
         *  @return rawData The vector with all the row data
         */
        std::vector<T> getRawData() const{
            return{
                row1.x,row1.y,row1.z,row1.w,
                row2.x,row2.y,row2.z,row2.w,
                row3.x,row3.y,row3.z,row3.w,
                row4.x,row4.y,row4.z,row4.w
            };
        }

    }

First we notice a very peculiar thing in the default constructor of a 4 by 4 matrix. When called it doesn't start all on zero but like:

    [1|0|0|0]
    [0|1|0|0]
    [0|0|1|0]
    [0|0|0|1] //A identity 4 by 4 matrix

All matrices should start with ones on the diagonal. (just because >.<)

Alright so let's declare at our epic cube a 4 by 4 matrix.

    glUseProgram(cubeProgram)
    Matrix4x4<float> position;
    glBindVertexArray(cubeVAO)
    glUniformMatrix4fv(shaderRef, 1, GL_TRUE, cubeData);
    glEnableVertexAttribArray ( 0 );
    glDrawArrays ( GL_TRIANGLES, 0,cubeVerticesSize)

Now we actually have all our variables we can finally start to do some math!
Let's do translation. If you have programmed in Unity3d you might remember a Transform.Translate function. Let's implement it in our own matrix class

     /*!
     *  Translates the matrix to
     *  @param vector, The vector you wish to translate to
     */
    static Matrix4x4<T> translate(Matrix4x4<T> mat, T x, T y, T z){
        Matrix4x4<T> result(mat);
        result.row1.w += x;
        result.row2.w += y;
        result.row3.w += z;
        return result;
    }


This is all the math needed to move the cube around(Not rotation or scaling mind you)
It works at all the angles. 
Let's implement this in our real life scenario

    glUseProgram(cubeProgram)
    Matrix4x4<float> position;
    position = Matrix4x4<float>::translate(position, 1,0,0);
    glBindVertexArray(cubeVAO)
    glUniformMatrix4fv(shaderRef, 1, GL_TRUE, &position.getRawData()[0]);
    glEnableVertexAttribArray ( 0 );
    glDrawArrays ( GL_TRIANGLES, 0,cubeVerticesSize)


Our shader needs to use our marvellous matrix

    #version 410 core
    uniform mat4 mv_matrix;
    layout(location = 0) in vec4 position;

    void main(void){
        gl_Position = v_matrix * position;
    }

And it should work.... but it seems we already have a bug in our program.
When you move along the z axis your object seems to disappear right into thin air.
This is because we don't have a projection matrix.
To solve this bug we need to know two things:

 1. How does a projection matrix look like?
 2. How can we combine it with our position matrix?

Well we can make a perspective (we are using three dimensions after all) matrix
The code

    template<typename T>
    Matrix4x4<T> perspective(T fovy, T aspect, T near, T far){
        
        T q = 1.0f / tan((0.5f * fovy) * (3.14 / 180));
        T A = q / aspect;
        T B = (near + far) / (near - far);
        T C = (2.0f * near * far) / (near - far);
        
        return Matrix4x4<T>(
            Vector4<T>(A,0,0,0),
            Vector4<T>(0,q,0,0),
            Vector4<T>(0,0,B,-1),
            Vector4<T>(0,0,C,0));
    }

It looks scary, but this method will actually calculate a matrix of how far you wish to look into the distance(and how close) and your field of view.

Now we have a projection matrix and a position matrix.. But how do we combine them?
Well fun thing is that we can actually multiply two matrices with each other.

    /*!
     *  Multiplies a matrix with an other matrix
     *  @param other, the matrix you wish to multiply with
     */
    static Matrix4x4<T> multiply(const Matrix4x4<T>& first,const Matrix4x4<T>& other){
        //generate temporary matrix
        Matrix4x4<T> result;
        //Row 1
        result.row1.x = first.row1.x * other.row1.x + first.row1.y * other.row2.x + first.row1.z * other.row3.x + first.row1.w * other.row4.x;
        result.row1.y = first.row1.x * other.row1.y + first.row1.y * other.row2.y + first.row1.z * other.row3.y + first.row1.w * other.row4.y;
        result.row1.z = first.row1.x * other.row1.z + first.row1.y * other.row2.z + first.row1.z * other.row3.z + first.row1.w * other.row4.z;
        result.row1.w = first.row1.x * other.row1.w + first.row1.y * other.row2.w + first.row1.z * other.row3.w + first.row1.w * other.row4.w;
        
        //Row2
        result.row2.x = first.row2.x * other.row1.x + first.row2.y * other.row2.x + first.row2.z * other.row3.x + first.row2.w * other.row4.x;
        result.row2.y = first.row2.x * other.row1.y + first.row2.y * other.row2.y + first.row2.z * other.row3.y + first.row2.w * other.row4.y;
        result.row2.z = first.row2.x * other.row1.z + first.row2.y * other.row2.z + first.row2.z * other.row3.z + first.row2.w * other.row4.z;
        result.row2.w = first.row2.x * other.row1.w + first.row2.y * other.row2.w + first.row2.z * other.row3.w + first.row2.w * other.row4.w;
        
        //Row3
        result.row3.x = first.row3.x * other.row1.x + first.row3.y * other.row2.x + first.row3.z * other.row3.x + first.row3.w * other.row4.x;
        result.row3.y = first.row3.x * other.row1.y + first.row3.y * other.row2.y + first.row3.z * other.row3.y + first.row3.w * other.row4.y;
        result.row3.z = first.row3.x * other.row1.z + first.row3.y * other.row2.z + first.row3.z * other.row3.z + first.row3.w * other.row4.z;
        result.row3.w = first.row3.x * other.row1.w + first.row3.y * other.row2.w + first.row3.z * other.row3.w + first.row3.w * other.row4.w;
        
        //Row4
        result.row4.x = first.row4.x * other.row1.x + first.row4.y * other.row2.x + first.row4.z * other.row3.x + first.row4.w * other.row4.x;
        result.row4.y = first.row4.x * other.row1.y + first.row4.y * other.row2.y + first.row4.z * other.row3.y + first.row4.w * other.row4.y;
        result.row4.z = first.row4.x * other.row1.z + first.row4.y * other.row2.z + first.row4.z * other.row3.z + first.row4.w * other.row4.z;
        result.row4.w = first.row4.x * other.row1.w + first.row4.y * other.row2.w + first.row4.z * other.row3.w + first.row4.w * other.row4.w;
        
        return result;
    }

Ooef.. that's a lot of code that actually looks more scarier then it actually looks. It can be done in a for loop but I (probably mistakenly) thought this would be clearer for people that never ever worked with matrices.

Look at the code and notice a repeating pattern. Multiply the column with the row add it and continue.(this is the same for any size matrix)

*Note that multiplication with matrices is not like normal multiplication. A X B != B x A  *

Now we know how to project and add this to our position matrix our real life code will probably look like:

    glUseProgram(cubeProgram)
    Matrix4x4<float> position;
    position = Matrix4x4<float>::translate(position, 1,0,0);
    position = Matrix4x4<float>::multiply(Matrix<float>::perspective<float>(50, 1 , 0.1f, 100000.0f), position);
    glBindVertexArray(cubeVAO)
    glUniformMatrix4fv(shaderRef, 1, GL_TRUE, &position.getRawData()[0]);
    glEnableVertexAttribArray ( 0 );
    glDrawArrays ( GL_TRIANGLES, 0,cubeVerticesSize)


Now our bug is squashed and our cube looks pretty epic in the distance.
If you would like to scale your cube the formula is this:

     /*!
     *  Scales the matrix with given vector
     *  @param s The vector you wish to scale with
     */
    static Matrix4x4<T> scale(const Matrix4x4<T>& mat, T x, T y, T z){
        Matrix4x4<T> tmp(mat);
        tmp.row1.x *= x;
        tmp.row2.y *= y;
        tmp.row3.z *= z;
        return tmp;
    }

You only need to adjust the diagonal variables.

For rotation you need to take a closer look at Quaternions.





  [1]: http://glm.g-truc.net/0.9.7/index.html

