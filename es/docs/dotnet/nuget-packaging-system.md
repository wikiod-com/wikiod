---
title: "Sistema de empaquetado NuGet"
slug: "sistema-de-empaquetado-nuget"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

[NuGet.org](https://www.nuget.org/):

> NuGet es el administrador de paquetes para la plataforma de desarrollo de Microsoft, incluido .NET. Las herramientas de cliente de NuGet brindan la capacidad de producir y consumir paquetes. La Galería NuGet es el repositorio central de paquetes que utilizan todos los autores y consumidores de paquetes.

Imágenes en ejemplos cortesía de [NuGet.org](https://www.nuget.org/).

## Desinstalar un paquete de un proyecto en una solución
    PM> Uninstall-Package -ProjectName MyProjectB EntityFramework

## Instalar una versión específica de un paquete
    PM> Install-Package EntityFramework -Version 6.1.2  


## Instalación del administrador de paquetes NuGet
Para poder administrar los paquetes de sus proyectos, necesita NuGet Package Manager. Esta es una extensión de Visual Studio, explicada en los documentos oficiales: [Instalación y actualización del cliente NuGet] (https://docs.nuget.org/consume/installing-nuget).

A partir de Visual Studio 2012, NuGet se incluye en todas las ediciones y se puede usar desde: Herramientas -> Administrador de paquetes NuGet -> Consola del administrador de paquetes.

Lo haces a través del menú Herramientas de Visual Studio, haciendo clic en Extensiones y actualizaciones:

[![ingrese la descripción de la imagen aquí][1]][1]

[1]: http://i.stack.imgur.com/zTzgp.png

Esto instala tanto la GUI:

* Disponible al hacer clic en "Administrar paquetes NuGet..." en un proyecto o su carpeta de Referencias

Y la consola del administrador de paquetes:

* Herramientas -> Administrador de paquetes NuGet -> Consola del administrador de paquetes.

## Agregar un feed de fuente de paquete (MyGet, Klondike, ect)
    nuget sources add -name feedname -source http://sourcefeedurl

## Gestión de paquetes a través de la interfaz de usuario
Cuando hace clic con el botón derecho en un proyecto (o su carpeta de referencias), puede hacer clic en la opción "Administrar paquetes NuGet...". Esto muestra el [diálogo del administrador de paquetes] (https://docs.nuget.org/consume/package-manager-dialog).

[![ingrese la descripción de la imagen aquí][1]][1]

[1]: http://i.stack.imgur.com/Fi0Uq.png

## Gestión de paquetes a través de la consola
Haga clic en los menús Herramientas -> Administrador de paquetes NuGet -> Consola del administrador de paquetes para mostrar la consola en su IDE. [Documentación oficial aquí] (https://docs.nuget.org/consume/package-manager-console-powershell-reference).

Aquí puede ejecutar, entre otros, comandos `install-package` que instalan el paquete ingresado en el "Proyecto predeterminado" actualmente seleccionado:

    Install-Package Elmah

También puede proporcionar el proyecto para instalar el paquete, anulando el proyecto seleccionado en el menú desplegable "Proyecto predeterminado":

    Install-Package Elmah -ProjectName MyFirstWebsite

## Actualizar un paquete
Para actualizar un paquete, use el siguiente comando:

    PM> Update-Package EntityFramework
donde EntityFramework es el nombre del paquete que se actualizará. Tenga en cuenta que la actualización se ejecutará para todos los proyectos, por lo que es diferente de `Install-Package EntityFramework`, que se instalaría solo en el "Proyecto predeterminado".

También puede especificar un solo proyecto explícitamente:

    PM> Update-Package EntityFramework -ProjectName MyFirstWebsite



## Desinstalar un paquete
    PM> Uninstall-Package EntityFramework  

## desinstalar una versión específica del paquete
    
    PM> uninstall-Package EntityFramework -Version 6.1.2

## Usando diferentes fuentes de paquetes Nuget (locales) usando la interfaz de usuario
Es común que la empresa configure su propio servidor nuget para la distribución de paquetes entre diferentes equipos.

1. Vaya al Explorador de soluciones y haga clic en el botón <kbd>Botón derecho del mouse</kbd> y luego elija `Administrar paquetes NuGet para la solución`

[![ingrese la descripción de la imagen aquí][1]][1]

2. En la ventana que se abre, haga clic en `Configuración`

[![ingrese la descripción de la imagen aquí][2]][2]

3. Haga clic en `+` en la esquina superior derecha y luego agregue el nombre y la URL que apunte a su servidor nuget local.

[![ingrese la descripción de la imagen aquí][3]][3]


[1]: http://i.stack.imgur.com/PhB3d.png
[2]: http://i.stack.imgur.com/8vKM6.png
[3]: http://i.stack.imgur.com/h85QG.png

