---
title: "Encapsulating OpenGL objects with C++ RAII"
slug: "encapsulating-opengl-objects-with-c++-raii"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

Examples of various ways to have OpenGL objects work with C++ RAII.

RAII encapsulation of OpenGL objects has dangers. The most unavoidable is that OpenGL objects are associated with the OpenGL context that created them. So the destruction of a C++ RAII object must be done in a OpenGL context which shares ownership of the OpenGL object managed by that C++ object.

This also means that if all contexts which own the object are destroyed, then any existing RAII encapsulated OpenGL objects will try to destroy objects which no longer exist.

You must take manual steps to deal with context issues like this.

## In C++11 and later
C++11 offers tools that enhance the functionality of RAII-encapsulated OpenGL objects. Without C++11 features like move semantics, such objects would have to be dynamically allocated if you want to pass them around, since they cannot be copied. Move support allows them to be passed back and forth like normal values, though not by copying:

<!-- language-all: lang-cpp -->

    class BufferObject
    {
    public:
        BufferObject(GLenum target, GLsizeiptr size, const void *data, GLenum usage)
        {
            glGenBuffers(1, &object_);
            glBindBuffer(target, object_);
            glBufferData(target, size, data, usage);
            glBindBuffer(target, 0);
        }
        
        //Cannot be copied.
        BufferObject(const BufferObject &) = delete;
        BufferObject &operator=(const BufferObject &) = delete;
        
        //Can be moved
        BufferObject(BufferObject &&other) noexcept : object_(other.Release())
        {}
        
        //Self-assignment is OK with this implementation.
        BufferObject &operator=(BufferObject &&other) noexcept
        {
            Reset(other.Release());
        }
        
        //Destroys the old buffer and claims ownership of a new buffer object.
        //It's OK to call glDeleteBuffers on buffer object 0.
        GLuint Reset(GLuint object = 0)
        {
            glDeleteBuffers(1, &object_);
            object_ = object;
        }
        
        //Relinquishes ownership of the object without destroying it
        GLuint Release()
        {
            GLuint ret = object_;
            object_ = 0;
            return ret;
        }    
        
        ~BufferObject()
        {
            Reset();
        }
        
        //Accessors and manipulators
        void Bind(GLenum target) const {glBindBuffer(target, object_);}
        GLuint GetObject() const {return object_;}
    
    private:
        GLuint object_;
    };

Such a type can be returned by a function:

    BufferObject CreateStaticBuffer(GLsizeiptr byteSize) {return BufferObject(GL_ARRAY_BUFFER, byteSize, nullptr, GL_STATIC_DRAW);}

Which allows you to store them in your own (implicitly move-only) types:

    struct Mesh
    {
    public:
    private:
        //Default member initializer.
        BufferObject buff_ = CreateStaticBuffer(someSize);
    };

A scoped binder class can also have move semantics, thus allowing the binder to be returned from functions and stored in C++ standard library containers:

    class BindBuffer
    {
    public:
        BindBuffer(GLenum target, const BufferObject &buff) : target_(target)
        {
            buff.Bind(target_);
        }
    
        //Non-copyable.
        BindBuffer(const BindBuffer &) = delete;
        BindBuffer &operator=(const BindBuffer &) = delete;
        
        //Move-constructible.
        BindBuffer(BindBuffer &&other) noexcept : target_(other.target_)
        {
            other.target_ = 0;
        }
        
        //Not move-assignable.
        BindBuffer &operator=(BindBuffer &&) = delete;
        
        ~BindBuffer()
        {
            //Only unbind if not moved from.
            if(target_)
                glBindBuffer(target_, 0);
        }
        
    private:
        GLenum target_;
    };

Note that the object is move constructible but not move-assignable. The idea with this is to prevent rebinding of a scoped buffer binding. Once it is set, the only thing that can unset it is being moved from.

## In C++98/03
Encapsulating an OpenGL object in C++98/03 requires obeying the C++ rule of 3. This means adding a copy constructor, copy assignment operator, and destructor.

However, copy constructors should logically copy the object. And copying an OpenGL object is a non-trivial undertaking. Equally importantly, it's almost certainly something that the user does not wish to do.

So we will instead make the object non-copyable:

<!-- language-all: lang-cpp -->
    class BufferObject
    {
    public:
        BufferObject(GLenum target, GLsizeiptr size, const void *data, GLenum usage)
        {
            glGenBuffers(1, &object_);
            glBindBuffer(target, object_);
            glBufferData(target, size, data, usage);
            glBindBuffer(target, 0);
        }
        
        ~BufferObject()
        {
            glDeleteBuffers(1, &object_);
        }
        
        //Accessors and manipulators
        void Bind(GLenum target) const {glBindBuffer(target, object_);}
        GLuint GetObject() const {return object_;}
    
    private:
        GLuint object_;
        
        //Prototypes, but no implementation.
        BufferObject(const BufferObject &);
        BufferObject &operator=(const BufferObject &);
    };

The constructor will create the object and initialize the buffer object's data. The destructor will destroy the object. By declaring the copy constructor/assignment *without* defining them, the linker will give an error if any code tries to call them. And by declaring them private, only members of `BufferObject` will even be able to call them.

Note that `BufferObject` does not retain the `target` passed to the constructor. That is because an OpenGL buffer object can be used with any target, not just the one it was initially created with. This is unlike texture objects, which [must always be bound to the target they were initially created with.][1]

Because OpenGL is very dependent on binding objects to the context for various purposes, it is often useful to have RAII-style scoped object binding as well. Because different objects have different binding needs (some have targets, others do not), we have to implement one for each object individually.

    class BindBuffer
    {
    public:
        BindBuffer(GLenum target, const BufferObject &buff) : target_(target)
        {
            buff.Bind(target_);
        }
        
        ~BindBuffer()
        {
            glBindBuffer(target_, 0);
        }
        
    private:
        GLenum target_;
    
        //Also non-copyable.
        BindBuffer(const BindBuffer &);
        BindBuffer &operator=(const BindBuffer &);
    };

`BindBuffer` is non-copyable, since copying it makes no sense. Note that it does not retain access to the `BufferObject` it binds. That is because it is unnecessary.


  [1]: https://www.opengl.org/wiki/Texture#Texture_Objects

