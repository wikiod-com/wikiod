---
title: "Primeros pasos con el lenguaje C#"
slug: "primeros-pasos-con-el-lenguaje-c"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creando una nueva aplicación de consola (Visual Studio)
1. Abra Visual Studio
2. En la barra de herramientas, vaya a **Archivo** → **Nuevo proyecto**
3. Seleccione el tipo de proyecto **Aplicación de consola**
4. Abra el archivo `Program.cs` en el Explorador de soluciones
5. Agregue el siguiente código a `Main()`:


    public class Program
    {
        public static void Main()
        {
            // Prints a message to the console.
            System.Console.WriteLine("Hello, World!");

            System.Console.ReadKey();
        }
    }

6. En la barra de herramientas, haga clic en **Depurar** -> **Iniciar depuración** o presione **F5** o **ctrl + F5** (ejecutar sin depurador) para ejecutar el programa.


[Demostración en vivo en ideone][1]

-------------------------------------------------- ---------------------------------

# Explicación

- `Programa de clase` es una declaración de clase. La clase `Program` contiene las definiciones de datos y métodos que utiliza su programa. Las clases generalmente contienen múltiples métodos. Los métodos definen el comportamiento de la clase. Sin embargo, la clase `Program` solo tiene un método: `Main`.

- `static void Main()` define el método `Main`, que es el punto de entrada para todos los programas de C#. El método `Main` indica lo que hace la clase cuando se ejecuta. Solo se permite un método `Main` por clase.

- El método `System.Console.WriteLine("¡Hola, mundo!");` imprime un dato dado (en este ejemplo, `¡Hola, mundo!`) como una salida en la ventana de la consola.

- `System.Console.ReadKey()`, asegura que el programa no se cerrará inmediatamente después de mostrar el mensaje. Lo hace esperando que el usuario presione una tecla en el teclado. Cualquier pulsación de tecla por parte del usuario terminará el programa. El programa termina cuando ha terminado la última línea de código en el método `main()`.

-------------------------------------------------- ---------------------------------

# Usando la línea de comando

Para compilar a través de la línea de comandos, use `MSBuild` o `csc.exe` _(el compilador de C#)_, ambos parte de [Microsoft Build Tools](https://www.visualstudio.com/downloads/download-visual- paquete studio-vs#d-build-tools).

Para compilar este ejemplo, ejecute el siguiente comando en el mismo directorio donde se encuentra `HelloWorld.cs`:

<!-- idioma: lang-ninguno -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs

También puede ser posible que tenga dos métodos principales dentro de una aplicación. En este caso, debe decirle al compilador qué método principal debe ejecutar escribiendo el siguiente comando en la **consola**. (Supongamos que la clase `ClassA` también tiene un método principal en el mismo archivo `HelloWorld.cs` en HelloWorld espacio de nombres)

<!-- idioma: lang-ninguno -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs /main:HelloWorld.ClassA 

donde HelloWorld es el espacio de nombres


***Nota**: Esta es la ruta donde se encuentra **.NET framework v4.0** en general. Cambie la ruta según su versión de .NET. Además, el directorio podría ser **framework** en lugar de **framework64** si usa .NET Framework de 32 bits. Desde el símbolo del sistema de Windows, puede enumerar todas las rutas de Framework csc.exe ejecutando los siguientes comandos (el primero para Frameworks de 32 bits):*

    dir %WINDIR%\Microsoft.NET\Framework\csc.exe /s/b
    dir %WINDIR%\Microsoft.NET\Framework64\csc.exe /s/b

![Compilando el archivo .cs][2]

Ahora debería haber un archivo ejecutable llamado `HelloWorld.exe` en el mismo directorio. Para ejecutar el programa desde el símbolo del sistema, simplemente escriba el nombre del ejecutable y presione <kbd>Enter</kbd> de la siguiente manera:

<!-- idioma: lang-none -->
    HelloWorld.exe

Esto producirá:

> ¡Hola, mundo!

![Ejecutando el archivo exe en la consola][3]

También puede hacer doble clic en el ejecutable y abrir una nueva ventana de consola con el mensaje "**¡Hola, mundo!**"

![Ejecutando el ejecutable y haciendo doble clic][4]

[1]: https://ideone.com/3OhmnG
[2]: http://i.stack.imgur.com/xT8kk.png
[3]: http://i.stack.imgur.com/x0Fek.png
[4]: http://i.stack.imgur.com/qstu1.png

## Crear un nuevo proyecto en Visual Studio (aplicación de consola) y ejecutarlo en modo de depuración
1. **Descargue e instale [Visual Studio][1]**. Visual Studio se puede descargar desde [VisualStudio.com][2]. Se sugiere la edición comunitaria, primero porque es gratuita y segundo porque incluye todas las características generales y puede extenderse más.

2. **Abrir Visual Studio.**
3. **Bienvenido.** Vaya a **Archivo → **Nuevo** → Proyecto**.
    [![Microsoft Visual Studio - File Menu][3]][3]

4. Haga clic en **Plantillas** → **Visual C#** → **Aplicación de consola**

    [![Microsoft Visual Studio - New Project window][4]][4]

5. **Después de seleccionar Aplicación de consola,** Ingrese un nombre para su proyecto y una ubicación para guardar y presione <kbd>OK</kbd>. No se preocupe por el nombre de la solución.

6. **Proyecto creado**. El proyecto recién creado se verá similar a:

    [![Microsoft Visual Studio - c# Default Project][5]][5]

    _(Always use descriptive names for projects so that they can easily be distinguished from other projects.  It is recommended not to use spaces in project or class name.)_

7. **Escribe código.** Ahora puedes actualizar tu `Program.cs` para presentar "¡Hola mundo!" al usuario

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                }
            }
        }

    Add the following two lines to the `public static void Main(string[] args)` object in `Program.cs`: (make sure it's inside the braces)

        Console.WriteLine("Hello world!");
        Console.Read();

    **Why** `Console.Read()`__?__ The first line prints out the text "Hello world!" to the console, and the second line waits for a single character to be entered; in effect, this causes the program to pause execution so that you're able to see the output while debugging.  Without `Console.Read();`, when you start debugging the application it will just print "Hello world!" to the console and then immediately close.  Your code window should now look like the following:

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                    Console.WriteLine("Hello world!");
                    Console.Read();
                }
            }
        }

8. **Depure su programa.** Presione el botón Inicio en la barra de herramientas cerca de la parte superior de la ventana [![Botón Iniciar depuración][6]][6] o presione <kbd>F5</kbd> en su teclado para ejecutar su aplicación. Si el botón no está presente, puede ejecutar el programa desde el menú superior: **Depurar → Iniciar depuración**. El programa compilará y luego abrirá una ventana de consola. Debería verse similar a la siguiente captura de pantalla:

[![Consola que ejecuta la aplicación Hello World][7]][7]

9. **Detener el programa.** Para cerrar el programa, simplemente presione cualquier tecla en su teclado. El `Console.Read()` que agregamos fue para este mismo propósito. Otra forma de cerrar el programa es ir al menú donde estaba el botón <kbd>Inicio</kbd> y hacer clic en el botón <kbd>Detener</kbd>.

     


[1]: https://www.visualstudio.com/products/vs-2015-product-editions
[2]: http://www.visualstudio.com
[3]: http://i.stack.imgur.com/fpvTX.png
[4]: http://i.stack.imgur.com/kKGls.png
[5]: http://i.stack.imgur.com/WVkeF.png
[6]: https://i.stack.imgur.com/odDu6.png
[7]: http://i.stack.imgur.com/ZD5MF.png

## Creando un nuevo programa usando Mono
Primero instale [Mono][1] siguiendo las instrucciones de instalación para la plataforma de su elección como se describe en su [sección de instalación][2].

Mono está disponible para Mac OS X, Windows y Linux.

Una vez finalizada la instalación, cree un archivo de texto, asígnele el nombre `HelloWorld.cs` y copie el siguiente contenido en él:

    public class Program
    {
        public static void Main()
        {
            System.Console.WriteLine("Hello, world!");
            System.Console.WriteLine("Press any key to exit..");
            System.Console.Read();
        }
    }


Si está utilizando Windows, ejecute el símbolo del sistema de Mono que se incluye en la instalación de Mono y se asegura de que se establezcan las variables de entorno necesarias. Si está en Mac o Linux, abra una nueva terminal.

Para compilar el archivo recién creado, ejecute el siguiente comando en el directorio que contiene `HelloWorld.cs`:

<!-- idioma: lang-ninguno -->
    mcs -out:HelloWorld.exe HelloWorld.cs
 

El 'HelloWorld.exe' resultante se puede ejecutar con:
 
<!-- idioma: lang-none -->
    mono HelloWorld.exe
 
que producirá la salida:
 
 
<!-- idioma: lang-none -->
    Hello, world!   
    Press any key to exit..

 
[1]: http://www.mono-project.com/
[2]: http://www.mono-project.com/docs/getting-started/install/

## Creando un nuevo programa usando .NET Core
Primero instale el [**.NET Core SDK**][1] siguiendo las instrucciones de instalación para la plataforma de su elección:

- [Ventanas][2]
- [OSX][3]
- [Linux][4]
- [Docker][5]

Una vez completada la instalación, abra un símbolo del sistema o una ventana de terminal.

1. Cree un nuevo directorio con `mkdir hello_world` y cambie al directorio recién creado con `cd hello_world`.

2. Cree una nueva aplicación de consola con `dotnet new console`.
Esto producirá dos archivos:

    - **hello_world.csproj**

          <Project Sdk="Microsoft.NET.Sdk">

            <PropertyGroup>
              <OutputType>Exe</OutputType>
              <TargetFramework>netcoreapp1.1</TargetFramework>
            </PropertyGroup>

          </Project>
          
    - **Program.cs**

          using System;
        
          namespace hello_world
          {
              class Program
              {
                  static void Main(string[] args)
                  {
                      Console.WriteLine("Hello World!");
                  }
              }
          }

3. Restaure los paquetes necesarios con `dotnet restore`.

4. *Opcional* Cree la aplicación con `dotnet build` para Depurar o `dotnet build -c Release` para Release. `dotnet run` también ejecutará el compilador y generará errores de compilación, si se encuentra alguno.

5. Ejecute la aplicación con `dotnet run` para depuración o `dotnet run .\bin\Release\netcoreapp1.1\hello_world.dll` para versión.

-------------------------------------------------- ---------------------------------

Salida del símbolo del sistema
---------------------
[![ingrese la descripción de la imagen aquí][6]][6]


[1]: https://docs.microsoft.com/en-us/dotnet/articles/core/
[2]: https://www.microsoft.com/net/core#windows
[3]: https://www.microsoft.com/net/core#macos
[4]: https://www.microsoft.com/net/core#linuxubuntu
[5]: https://www.microsoft.com/net/core#dockercmd
[6]: https://i.stack.imgur.com/arqCl.png


## Creando una nueva consulta usando LinqPad
LinqPad es una gran herramienta que le permite aprender y probar las características de los lenguajes .Net (C#, F# y VB.Net).

1. Instale [LinqPad][1]
2. Cree una nueva Consulta (<kbd>Ctrl</kbd> + <kbd>N</kbd>)
[![ingrese la descripción de la imagen aquí][2]][2]
3. En idioma, seleccione "Declaraciones C#"
[![ingrese la descripción de la imagen aquí][3]][3]
4. Escriba el siguiente código y presione ejecutar (<kbd>F5</kbd>)

        string hw = "Hello World";

        hw.Dump(); //or Console.WriteLine(hw);
[![ingrese la descripción de la imagen aquí][4]][4]

5. Debería ver "Hello World" impreso en la pantalla de resultados.
[![ingrese la descripción de la imagen aquí][5]][5]
6. Ahora que ha creado su primer programa .Net, vaya y compruebe los ejemplos incluidos en LinqPad a través del navegador "Muestras". Hay muchos buenos ejemplos que le mostrarán muchas características diferentes de los lenguajes .Net.
[![ingrese la descripción de la imagen aquí][6]][6]

**Notas:**
1. Si hace clic en "IL", puede inspeccionar el código IL que genera su código .net. Esta es una gran herramienta de aprendizaje.
[![ingrese la descripción de la imagen aquí][7]][7]
2. Al usar `LINQ to SQL` o `Linq to Entities`, puede inspeccionar el SQL que se está generando, lo cual es otra excelente manera de aprender sobre LINQ.


[1]: http://www.linqpad.net/
[2]: http://i.stack.imgur.com/D0tSi.png
[3]: http://i.stack.imgur.com/kC5Ur.jpg
[4]: http://i.stack.imgur.com/LO4kD.jpg
[5]: http://i.stack.imgur.com/GzsrS.jpg
[6]: http://i.stack.imgur.com/yucuf.jpg
[7]: http://i.stack.imgur.com/XPumO.jpg

## Creando un nuevo proyecto usando Xamarin Studio
1. Descargue e instale [Comunidad de Xamarin Studio][1].
2. Abra Xamarin Studio.
3. Haga clic en **Archivo** → **Nuevo** → **Solución**.

[![Creando un nuevo proyecto en Xamarin Studio][2]][2]

4. Haga clic en **.NET** → **Proyecto de consola** y elija **C#**.
5. Haga clic en <kbd>Siguiente</kbd> para continuar.

[![Elegir plantilla para nuevo proyecto][3]][3]
 
6. Ingrese el **Nombre del proyecto** y <kbd>Examinar...</kbd> para una **Ubicación** para guardar y luego haga clic en <kbd>Crear</kbd>.

[![Nombre y ubicación del proyecto][4]][4]

7. El proyecto recién creado tendrá un aspecto similar a:

[![ingrese la descripción de la imagen aquí][5]][5]

8. Este es el código en el Editor de texto:


    using System;
    
    namespace FirstCsharp
    {
        public class MainClass
        {
            public static void Main(string[] args)
            {
                Console.WriteLine("Hello World!");
                Console.ReadLine();
            }
        }
    }

9. Para ejecutar el código, presione <kbd>F5</kbd> o haga clic en **Botón de reproducción** como se muestra a continuación:

[![Ejecutar el código][6]][6]

10. La siguiente es la salida:

[![salida][7]][7]


[1]: https://store.xamarin.com/
[2]: http://i.stack.imgur.com/hHjMM.png
[3]: http://i.stack.imgur.com/s58Ju.png
[4]: http://i.stack.imgur.com/lrK8L.png
[5]: http://i.stack.imgur.com/vva82.png
[6]: http://i.stack.imgur.com/6q4ZN.png
[7]: http://i.stack.imgur.com/cqBsK.png

