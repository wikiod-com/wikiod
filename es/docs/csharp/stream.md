---
title: "Corriente"
slug: "corriente"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Uso de flujos
Una secuencia es un objeto que proporciona un medio de bajo nivel para transferir datos. Ellos mismos no actúan como contenedores de datos.

Los datos con los que tratamos están en forma de matriz de bytes (`byte []`). Las funciones de lectura y escritura están todas orientadas a bytes, p. `EscribirByte()`.

No hay funciones para tratar con números enteros, cadenas, etc. Esto hace que la secuencia sea de uso muy general, pero menos fácil de usar si, por ejemplo, solo desea transferir texto. Los flujos pueden ser especialmente útiles cuando se trata de una gran cantidad de datos.

Tendremos que usar diferentes tipos de Stream en función de dónde se debe escribir/leer (es decir, la tienda de respaldo). Por ejemplo, si la fuente es un archivo, necesitamos usar `FileStream`:

    string filePath = @"c:\Users\exampleuser\Documents\userinputlog.txt";
    using (FileStream fs = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
    {
        // do stuff here...
    
        fs.Close();
    }

De manera similar, `MemoryStream` se usa si el almacén de respaldo es la memoria:

    // Read all bytes in from a file on the disk.
    byte[] file = File.ReadAllBytes(“C:\\file.txt”);

    // Create a memory stream from those bytes.
    using (MemoryStream memory = new MemoryStream(file))
    {
       // do stuff here...
    }

De manera similar, `System.Net.Sockets.NetworkStream` se usa para el acceso a la red.

Todos los flujos se derivan de la clase genérica `System.IO.Stream`. Los datos no se pueden leer ni escribir directamente desde flujos. .NET Framework proporciona clases auxiliares como `StreamReader`, `StreamWriter`, `BinaryReader` y `BinaryWriter` que convierten entre tipos nativos y la interfaz de flujo de bajo nivel, y transfieren los datos hacia o desde el flujo por usted.

La lectura y escritura de flujos se puede realizar a través de `StreamReader` y `StreamWriter`. Se debe tener cuidado al cerrar estos. De forma predeterminada, el cierre también cerrará el flujo contenido y lo dejará inutilizable para otros usos. Este comportamiento predeterminado se puede cambiar usando un [constructor][1] que tiene el parámetro `bool LeaveOpen` y configurando su valor como `true`.


`StreamWriter`:

    FileStream fs = new FileStream("sample.txt", FileMode.Create);
    StreamWriter sw = new StreamWriter(fs);
    string NextLine = "This is the appended line.";
    sw.Write(NextLine);
    sw.Close();
    //fs.Close(); There is no need to close fs. Closing sw will also close the stream it contains.

`StreamReader`:

    using (var ms = new MemoryStream())
    {
        StreamWriter sw = new StreamWriter(ms);
        sw.Write(123);
        //sw.Close();     This will close ms and when we try to use ms later it will cause an exception
        sw.Flush();     //You can send the remaining data to stream. Closing will do this automatically
        // We need to set the position to 0 in order to read 
        // from the beginning.
        ms.Position = 0;
        StreamReader sr = new StreamReader(ms);
        var myStr = sr.ReadToEnd();
        sr.Close();
        ms.Close();
    }

Dado que las clases `Stream`, `StreamReader`, `StreamWriter`, etc. implementan la interfaz `IDisposable`, podemos llamar al método `Dispose()` en los objetos de estas clases.


[1]: https://msdn.microsoft.com/en-us/library/gg712952(v=vs.110).aspx

