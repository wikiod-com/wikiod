---
title: "Composite pattern"
slug: "composite-pattern"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Composite lets clients treat individual objects and compositions of objects uniformly. For example consider a program that manipulates a file system. Files are simple objects and folders are composition of files and folders. However, for example, they both have size, name etc. functions. It would be easier and more convenient to treat both file and folder objects uniformly by defining a File System Resource Interface

The composite pattern applies when there is a part-whole hierarchy of objects and a client needs to deal with objects uniformly regardless of the fact that an object might be a leaf (simple object) or a branch (composite object). 

## pseudocode for a dumb file manager
    /* 
    * Component is an interface 
    * which all elements (files,
    * folders, links ...) will implement
    */
    class Component
    {
    public:
        virtual int getSize() const = 0;
    };

    /*
    * File class represents a file
    * in file system.   
    */
    class File : public Component
    {
    public:
        virtual int getSize() const {
            // return file size
        }   
    };

    /*
    * Folder is a component and 
    * also may contain files and 
    * another folders. Folder is a
    * composition of components
    */
    class Folder : public Component
    {
    public:
        void addComponent(Component* aComponent) {
            // mList append aComponent;
        }
        void removeComponent(Component* aComponent) {
            // remove aComponent from mList
        }
        virtual int getSize() const {
            int size = 0;
            foreach(component : mList) {
                size += component->getSize();
            }
            return size;
        } 

    private:
        list<Component*> mList;
    };

