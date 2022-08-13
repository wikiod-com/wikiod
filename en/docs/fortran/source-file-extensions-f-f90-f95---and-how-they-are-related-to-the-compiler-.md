---
title: "Source file extensions (.f, .f90, .f95, ...) and how they are related to the compiler."
slug: "source-file-extensions-f-f90-f95--and-how-they-are-related-to-the-compiler"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Fortran files come under a variety of extensions and each of them have a separate meaning. They specify the Fortran release version, code formatting style and the usage of preprocessor directives similar to C programming language.

## Extensions and Meanings
The following are some of the common extensions used in Fortran source files and the functionalities they can work on.

**Lowercase f in the extension**

These files do not have the features of preprocessor directives similar to C-programming language. They can be directly compiled to create object files. eg: .f, .for, .f95

**Uppercase F in the extension**

These files do have the features of preprocessor directives similar to C-programming language. The preprocessors are either defined within the files or using C/C++ like header files or both. These files have to be pre-processed to get the lower case extension files which can be used for compiling. eg: .F, .FOR, .F95

**.f,  .for, .f77, .ftn**

These are used for Fortran files that use **Fixed style format** and thus uses **Fortran 77** release version. Since they are lower case extensions, they cannot have preprocessor directives.

**.F,  .FOR, .F77, .FTN**

These are used for Fortran files that use **Fixed style format** and thus uses **Fortran 77** release version. Since they are upper case extensions, they can have preprocessor directives and thus they have to be preprocessed to get the lower case extension files.

**.f90, .f95, .f03, .f08**
These are used for Fortran files that use **Free style format** and thus uses later release versions of Fortran. The release versions are in the name. 

 - f90 - Fortran 90
 - f95 - Fortran 95
 - f03 - Fortran 2003
 - f08 - Fortran 2008

Since they are lower case extensions, they cannot have preprocessor directives.

**.F90, .F95, .F03, .F08**
These are used for Fortran files that use **Free style format** and thus uses later release versions of Fortran. The release versions are in the name. 

 - F90 - Fortran 90
 - F95 - Fortran 95
 - F03 - Fortran 2003
 - F08 - Fortran 2008

Since they are upper case extensions, they have preprocessor directives and thus they have to be preprocessed to get the lower case extension files.

