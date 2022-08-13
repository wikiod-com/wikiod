---
title: "Sistema.IO"
slug: "sistemaio"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Leer un archivo de texto usando StreamReader


## Puertos serie usando System.IO.SerialPorts
Iterando sobre puertos seriales conectados
-------------------------------------

    using System.IO.Ports;
    string[] ports = SerialPort.GetPortNames();
    for (int i = 0; i < ports.Length; i++)
    {
        Console.WriteLine(ports[i]);
    }

----------


Instanciando un objeto System.IO.SerialPort
-------------------------------------------

    using System.IO.Ports;
    SerialPort port = new SerialPort();
    SerialPort port = new SerialPort("COM 1"); ;
    SerialPort port = new SerialPort("COM 1", 9600);
**NOTA**: Esas son solo tres de las siete sobrecargas del constructor para el tipo SerialPort.

----------

Lectura/escritura de datos a través de SerialPort
----------------------------------------

La forma más sencilla es usar los métodos `SerialPort.Read` y `SerialPort.Write`.
Sin embargo, también puede recuperar un objeto `System.IO.Stream` que puede usar para transmitir datos a través de SerialPort. Para hacer esto, use `SerialPort.BaseStream`.

**Lectura**

    int length = port.BytesToRead;
    //Note that you can swap out a byte-array for a char-array if you prefer.
    byte[] buffer = new byte[length];
    port.Read(buffer, 0, length);

También puede leer todos los datos disponibles:

    string curData = port.ReadExisting();

O simplemente lea hasta la primera línea nueva que encuentre en los datos entrantes:

    string line = port.ReadLine();

**Escritura**

La forma más fácil de escribir datos en SerialPort es:

    port.Write("here is some text to be sent over the serial port.");

Sin embargo, también puede enviar datos de esta manera cuando sea necesario:

    //Note that you can swap out the byte-array with a char-array if you so choose.
    byte[] data = new byte[1] { 255 };
    port.Write(data, 0, data.Length);

## Leer/Escribir datos usando System.IO.File
Primero, veamos tres formas diferentes de extraer datos de un archivo.

    string fileText = File.ReadAllText(file);
    string[] fileLines = File.ReadAllLines(file);
    byte[] fileBytes = File.ReadAllBytes(file);

- En la primera línea, leemos todos los datos del archivo como una cadena.
- En la segunda línea, leemos los datos del archivo en una matriz de cadenas.
Cada línea del archivo se convierte en un elemento de la matriz.
- En el tercero leemos los bytes del archivo.

----------

A continuación, veamos tres métodos diferentes para **agregar** datos a un archivo.
Si el archivo que especifica no existe, cada método creará automáticamente el archivo antes de intentar agregarle los datos.

     File.AppendAllText(file, "Here is some data that is\nappended to the file.");
     File.AppendAllLines(file, new string[2] { "Here is some data that is", "appended to the file." });
     using (StreamWriter stream = File.AppendText(file))
     {
         stream.WriteLine("Here is some data that is");
         stream.Write("appended to the file.");
     }

- En la primera línea, simplemente agregamos una cadena al final del archivo especificado.
- En la segunda línea agregamos cada elemento de la matriz en una nueva línea en el archivo.
- Finalmente, en la tercera línea, usamos `File.AppendText` para abrir un streamwriter que agregará cualquier dato que se escriba en él.
----------
Y, por último, veamos tres métodos diferentes para **escribir** datos en un archivo.
La diferencia entre *agregar* y *escribir* es que escribir **sobrescribe** los datos en el archivo mientras que agregar **agrega** a los datos en el archivo.
Si el archivo que especifica no existe, cada método creará automáticamente el archivo antes de intentar escribir los datos en él.

    File.WriteAllText(file, "here is some data\nin this file.");
    File.WriteAllLines(file, new string[2] { "here is some data", "in this file" });
    File.WriteAllBytes(file, new byte[2] { 0, 255 });
- La primera línea escribe una cadena en el archivo.
- La segunda línea escribe cada cadena en la matriz en su propia línea en el archivo.
- Y la tercera línea le permite escribir una matriz de bytes en el archivo.

