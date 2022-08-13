---
title: "Hello World"
slug: "hello-world"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Hello World Example
<!-- language: c++ -->
```
#include <vtkAutoInit.h>

VTK_MODULE_INIT(vtkRenderingOpenGL2);
VTK_MODULE_INIT(vtkRenderingFreeType);
VTK_MODULE_INIT(vtkInteractionStyle);

#include <vtkSmartPointer.h>
#include <vtkTextActor.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>

int main(int /*argc*/, char ** /*argv*/)
{
    auto textActor = vtkSmartPointer<vtkTextActor>::New();
    textActor->SetInput("Hello World");

    auto renderer = vtkSmartPointer<vtkRenderer>::New();
    renderer->AddActor(textActor);
    renderer->ResetCamera();

    auto interactor = vtkSmartPointer<vtkRenderWindowInteractor>::New();
    
    auto renderWindow = vtkSmartPointer<vtkRenderWindow>::New();
    renderWindow->AddRenderer(renderer);
    renderWindow->SetInteractor(interactor);
    
    interactor->Start();

    return 0;
}
```

# Breakdown:

<!-- language: c++ -->
```
#include <vtkAutoInit.h>

VTK_MODULE_INIT(vtkRenderingOpenGL2);
VTK_MODULE_INIT(vtkRenderingFreeType);
VTK_MODULE_INIT(vtkInteractionStyle);
```

VTK design uses a [factory method](https://en.wikipedia.org/wiki/Factory_method_pattern) design pattern to create new instances of [vtkObject](http://www.vtk.org/doc/nightly/html/classvtkObject.html) derived classes using the `<ClassName>::New()` method. This allows a platform specific implementation to be selected during runtime to satisfy a required interface.

For this mechanism to work, factory classes need to "register" themselves so that they can be selected by the vtk infrastructure. Details on this topic is available [here](http://www.vtk.org/Wiki/VTK/Build_System_Migration#How_Implementation_Modules_Are_Initialized). 

`VTK_MODULE_INIT` is a macro used to automatically initialize the required modules/library(ies)(`vtkRenderingOpenGL2`, `vtkRenderingFreeType`, `vtkInteractionStyle` in this example). Failure to initialize the modules will result in `<ClassName>::New()` calls to return `NULL` and therefore runtime errors.

<!-- language: c++ -->
```
#include <vtkSmartPointer.h>
#include <vtkTextActor.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
```

[`vtkSmartPointer`](http://www.vtk.org/doc/nightly/html/classvtkSmartPointer.html) role is similar to that of a `std::unique_ptr` in that it manages the reference count that controls the lifetime of `vtkObject` derived class instances.

[`vtkTextActor`](http://www.vtk.org/doc/nightly/html/classvtkTextActor.html) is a simple class that can be used to display strings on the screen.

[`vtkRenderer`](http://www.vtk.org/doc/nightly/html/classvtkRenderer.html) is a class responsible for managing a scene's contents. Specifically it manages the collection of 
 * 2D actors derived from [`vtkActor2D`](http://www.vtk.org/doc/nightly/html/classvtkActor2D.html)
 * 3D actors derived from [`vtkProp3D`](http://www.vtk.org/doc/nightly/html/classvtkProp3D.html)
 * Volumes : [`vtkVolume`](http://www.vtk.org/doc/nightly/html/classvtkVolume.html)
 * Camera : [`vtkCamera`](http://www.vtk.org/doc/nightly/html/classvtkCamera.html)
 * Lights : [`vtkLight`](http://www.vtk.org/doc/nightly/html/classvtkLight.html)

[`vtkRenderWindow`](http://www.vtk.org/doc/nightly/html/classvtkRenderWindow.html) is a class that provides platform independent interface for 

* managing a collection of renderers.
* handling user input and forwarding it to [`vtkRenderWindowInteractor`](http://www.vtk.org/doc/nightly/html/classvtkRenderWindowInteractor.html) for further processing

[`vtkRenderWindowInteractor`](http://www.vtk.org/doc/nightly/html/classvtkRenderWindowInteractor.html) is a class responsible for mapping the user input (mouse/keyboard/timing) events to a corresponding action. Internally it uses a [`vtkInteractorStyle`](http://www.vtk.org/doc/nightly/html/classvtkInteractorStyle.html) to provide different mapping behaviors.

<!-- language: c++ -->
```
auto textActor = vtkSmartPointer<vtkTextActor>::New();
textActor->SetInput("Hello World");
```

Create text actor and set the string to display

<!-- language: c++ -->
```
auto renderer = vtkSmartPointer<vtkRenderer>::New();
renderer->AddActor(textActor);
renderer->ResetCamera();
```

* Create a renderer
* Add the text actor to it 
* Resets the camera position to make sure the actor is visible in the screen.

<!-- language: c++ -->
```
auto interactor = vtkSmartPointer<vtkRenderWindowInteractor>::New();
```

<!-- language: c++ -->
```
auto renderWindow = vtkSmartPointer<vtkRenderWindow>::New();
renderWindow->AddRenderer(renderer);
renderWindow->SetInteractor(interactor);
```

Create a window to render into, add the renderer to it and set the interactor.
The factory function will automatically pick a suitable implementation based on the available/registered factory classes

<!-- language: c++ -->
```
interactor->Start();
```

This is a blocking call that returns only when the user requests a quit (<kbd>q</kbd> key) or closes the window. Runs a message loop and dispatches the messages.

Running this should create a window that looks like this

[![Hello World][1]][1]

# Notes

This list of DLLs that were used by this exe are:

`VTKCommonCore-7.0.DLL`

`VTKInteractionStyle-7.0.DLL`

`VTKRenderingCore-7.0.DLL`

`VTKRenderingFreeType-7.0.DLL`

`VTKRenderingOpenGL2-7.0.DLL`

[1]: http://i.stack.imgur.com/wvwfY.png

