---
title: "Ejemplos de AssemblyInfo.cs"
slug: "ejemplos-de-assemblyinfocs"
draft: false
images: []
weight: 9764
type: docs
toc: true
---

El nombre de archivo `AssemblyInfo.cs` se usa por convención como el archivo de origen donde los desarrolladores colocan atributos de metadatos que describen todo el ensamblaje que están creando.

## Información de ensamblaje global y local


## [Versión de ensamblaje]
Este atributo aplica una versión al ensamblaje.

    [assembly: AssemblyVersion("1.0.*")]

El carácter `*` se utiliza para incrementar automáticamente una parte de la versión automáticamente cada vez que se compila (a menudo se utiliza para el número de "compilación")

## [Título de Asamblea]
Este atributo se utiliza para dar un nombre a este conjunto en particular.

    [assembly: AssemblyTitle("MyProduct")]



## [Producto de ensamblaje]
Este atributo se utiliza para describir el producto para el que es este ensamblaje en particular. Varios ensamblajes pueden ser componentes del mismo producto, en cuyo caso todos pueden compartir el mismo valor para este atributo.

    [assembly: AssemblyProduct("MyProduct")]


## Control de versiones automatizado


## Campos comunes


## [InternalsVisibleTo]
Si desea hacer que las clases o funciones "internas" de un ensamblaje sean accesibles desde otro ensamblaje, declare esto mediante "InternalsVisibleTo" y el nombre del ensamblaje al que se le permite acceder.


En este ejemplo, el código del ensamblado `MyAssembly.UnitTests` puede llamar elementos `internos` desde `MyAssembly`.

    [assembly: InternalsVisibleTo("MyAssembly.UnitTests")]

Esto es especialmente útil para pruebas unitarias para evitar declaraciones 'públicas' innecesarias.

## Lectura de atributos de ensamblado
Con las API de reflexión enriquecidas de .NET, puede obtener acceso a los metadatos de un ensamblaje. Por ejemplo, puede obtener el atributo de título de `este` ensamblaje con el siguiente código

    using System.Linq;
    using System.Reflection;
    
    ...
    
    Assembly assembly = typeof(this).Assembly;
    var titleAttribute = assembly.GetCustomAttributes<AssemblyTitleAttribute>().FirstOrDefault();
    
    Console.WriteLine($"This assembly title is {titleAttribute?.Title}");


## [Configuración de ensamblaje]
AssemblyConfiguration: el atributo AssemblyConfiguration debe tener la configuración que se usó para compilar el ensamblaje.
Utilice la compilación condicional para incluir correctamente diferentes configuraciones de ensamblaje.
Use el bloque similar al ejemplo a continuación. Agregue tantas configuraciones diferentes como use habitualmente.


    #if (DEBUG)
    
    [assembly: AssemblyConfiguration("Debug")]

    #else

    [assembly: AssemblyConfiguration("Release")]
    
    #endif


## [Archivo de clave de ensamblaje]
Siempre que queramos que nuestro ensamblaje se instale en GAC, debe tener un nombre seguro. Para un ensamblaje de nombres fuerte, tenemos que crear una clave pública.
Para generar el archivo `.snk`.

Para crear un archivo de clave de nombre seguro

> 1. Símbolo del sistema de desarrolladores para VS2015 (con acceso de administrador)
> 2. En el símbolo del sistema, escriba cd C:\Directory_Name y presione ENTER.
> 3. En el símbolo del sistema, escriba sn -k KeyFileName.snk y luego presione ENTRAR.

una vez que keyFileName.snk se crea en el directorio especificado, dé referencia en su proyecto. asigne al atributo `AssemblyKeyFileAttribute` la ruta al archivo `snk` para generar la clave cuando construimos nuestra biblioteca de clases.
    
> Propiedades -> AssemblyInfo.cs
    
    [assembly: AssemblyKeyFile(@"c:\Directory_Name\KeyFileName.snk")]

Esto creará un ensamblado de nombre seguro después de la compilación. Después de crear su ensamblaje de nombre seguro, puede instalarlo en GAC

Codificación feliz :)

