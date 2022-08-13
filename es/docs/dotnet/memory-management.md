---
title: "Gestión de la memoria"
slug: "gestion-de-la-memoria"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

Las aplicaciones críticas para el rendimiento en aplicaciones .NET administradas pueden verse gravemente afectadas por el GC. Cuando se ejecuta el GC, todos los demás subprocesos se suspenden hasta que se completa. Por esta razón, se recomienda evaluar cuidadosamente los procesos de GC y determinar cómo minimizar cuando se ejecuta.

## Use SafeHandle cuando empaquete recursos no administrados
Al escribir envoltorios para recursos no administrados, debe subclasificar `SafeHandle` en lugar de intentar implementar `IDisposable` y un finalizador usted mismo. Su subclase `SafeHandle` debe ser lo más pequeña y simple posible para minimizar la posibilidad de una fuga de identificadores. Esto probablemente significa que su implementación de SafeHandle sería un detalle de implementación interna de una clase que la envuelve para proporcionar una API utilizable. Esta clase asegura que, incluso si un programa pierde su instancia `SafeHandle`, se libera su identificador no administrado.

    using System.Runtime.InteropServices;
    
    class MyHandle : SafeHandle
    {
        public override bool IsInvalid => handle == IntPtr.Zero;
        public MyHandle() : base(IntPtr.Zero, true)
        { }
    
        public MyHandle(int length) : this()
        {
            SetHandle(Marshal.AllocHGlobal(length));
        }

        protected override bool ReleaseHandle()
        {
            Marshal.FreeHGlobal(handle);
            return true;
        }
    }

Descargo de responsabilidad: este ejemplo es un intento de mostrar cómo proteger un recurso administrado con `SafeHandle` que implementa `IDisposable` para usted y configura los finalizadores de manera adecuada. Es muy artificial y probablemente no tenga sentido asignar una porción de memoria de esta manera.

## Recursos no administrados
Cuando hablamos del GC y el "montón", en realidad estamos hablando de lo que se llama *montón administrado*. Los objetos en el *montón administrado* pueden acceder a recursos que no están en el montón administrado, por ejemplo, al escribir o leer de un archivo. Puede ocurrir un comportamiento inesperado cuando se abre un archivo para lectura y luego ocurre una excepción, lo que impide que el identificador del archivo se cierre como lo haría normalmente. Por esta razón, .NET requiere que los recursos no administrados implementen la interfaz `IDisposable`. Esta interfaz tiene un único método llamado `Dispose` sin parámetros:

    public interface IDisposable
    {
        Dispose();
    } 

Al manejar recursos no administrados, debe asegurarse de que se eliminen correctamente. Puedes hacer esto llamando explícitamente a `Dispose()` en un bloque `finally`, o con una declaración `using`.

    StreamReader sr; 
    string textFromFile;
    string filename = "SomeFile.txt";
    try 
    {
        sr = new StreamReader(filename);
        textFromFile = sr.ReadToEnd();
    }
    finally
    {
        if (sr != null) sr.Dispose();
    }

o

    string textFromFile;
    string filename = "SomeFile.txt";
    
    using (StreamReader sr = new Streamreader(filename))
    {
        textFromFile = sr.ReadToEnd();
    }

Este último es el método preferido y se expande automáticamente al primero durante la compilación.

