---
title: "Creación de una aplicación de consola con un editor de texto sin formato y el compilador de C# (csc.exe)"
slug: "creacion-de-una-aplicacion-de-consola-con-un-editor-de-texto-sin-formato-y-el-compilador-de-c-cscexe"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Creación de una aplicación de consola con un editor de texto sin formato y el compilador de C#
Para usar un editor de texto sin formato para crear una aplicación de consola escrita en C#, necesitará el compilador de C#. El compilador de C# (csc.exe) se puede encontrar en la siguiente ubicación:
`%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe`

**N.B.** Dependiendo de la versión de .NET Framework que esté instalada en su sistema, es posible que deba cambiar la ruta anterior, según corresponda.


----------

<h1>Guardar el código</h1>
El propósito de este tema no es enseñarle <i>cómo</i> escribir una aplicación de Consola, sino enseñarle cómo <i>compilar</i> una [para producir un solo archivo ejecutable], sin nada que no sea el compilador de C# y cualquier editor de texto sin formato (como el Bloc de notas).
<br/><br/>

1. Abra el cuadro de diálogo Ejecutar usando el método abreviado de teclado <kbd>Tecla de Windows</kbd> + <kbd>R</kbd>
2. Escriba `notepad`, luego presione <kbd>Enter</kbd>
3. Pegue el código de ejemplo a continuación, en el Bloc de notas
4. Guarde el archivo como `ConsoleApp.cs`, yendo a **Archivo** → **Guardar como...**, luego ingrese `ConsoleApp.cs` en el campo de texto 'Nombre de archivo', luego seleccione ` Todos los archivos` como el tipo de archivo.
5. Haga clic en `Guardar`

<h1>Compilación del código fuente</h1>
1. Abra el cuadro de diálogo Ejecutar, usando <kbd>Tecla de Windows</kbd> + <kbd>R</kbd><br/>
2. Introduzca:

    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe /t:exe /out:"C:\Users\yourUserName\Documents\ConsoleApp.exe" "C:\Users\yourUserName\Documents\ConsoleApp.cs"

Ahora, regrese a donde guardó originalmente su archivo `ConsoleApp.cs`. Ahora debería ver un archivo ejecutable (`ConsoleApp.exe`). Haga doble clic en `ConsoleApp.exe` para abrirlo.

¡Eso es todo! Su aplicación de consola ha sido compilada. Se ha creado un archivo ejecutable y ahora tiene una aplicación de consola en funcionamiento.


    using System;
    
    namespace ConsoleApp
    {
        class Program
        {
            private static string input = String.Empty;
    
            static void Main(string[] args)
            {
                goto DisplayGreeting;
    
                DisplayGreeting:
                {
                    Console.WriteLine("Hello! What is your name?");
    
                    input = Console.ReadLine();
    
                    if (input.Length >= 1)
                    {
                        Console.WriteLine(
                            "Hello, " + 
                            input + 
                            ", enter 'Exit' at any time to exit this app.");
    
                        goto AwaitFurtherInstruction;
                    }
                    else
                    {
                        goto DisplayGreeting;
                    }
                }
    
                AwaitFurtherInstruction:
                {
                    input = Console.ReadLine();
    
                    if(input.ToLower() == "exit")
                    {
                        input = String.Empty;
    
                        Environment.Exit(0);
                    }
                    else
                    {
                        goto AwaitFurtherInstruction;
                    }
                }
            }
        }
    }

