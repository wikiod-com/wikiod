---
title: "System.IO"
slug: "systemio"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Lendo um arquivo de texto usando StreamReader


## Portas seriais usando System.IO.SerialPorts
Iterando em portas seriais conectadas
--------------------------

    using System.IO.Ports;
    string[] ports = SerialPort.GetPortNames();
    for (int i = 0; i < ports.Length; i++)
    {
        Console.WriteLine(ports[i]);
    }

----------


Instanciando um objeto System.IO.SerialPort
--------------------------------------------

    using System.IO.Ports;
    SerialPort port = new SerialPort();
    SerialPort port = new SerialPort("COM 1"); ;
    SerialPort port = new SerialPort("COM 1", 9600);
**NOTA**: Essas são apenas três das sete sobrecargas do construtor para o tipo SerialPort.

----------

Lendo/gravando dados sobre o SerialPort
----------------------------------------

A maneira mais simples é usar os métodos `SerialPort.Read` e `SerialPort.Write`.
No entanto, você também pode recuperar um objeto `System.IO.Stream` que pode ser usado para transmitir dados pelo SerialPort. Para fazer isso, use `SerialPort.BaseStream`.

**Leitura**

    int length = port.BytesToRead;
    //Note that you can swap out a byte-array for a char-array if you prefer.
    byte[] buffer = new byte[length];
    port.Read(buffer, 0, length);

Você também pode ler todos os dados disponíveis:

    string curData = port.ReadExisting();

Ou simplesmente leia a primeira nova linha encontrada nos dados de entrada:

    string line = port.ReadLine();

**Escrita**

A maneira mais fácil de gravar dados no SerialPort é:

    port.Write("here is some text to be sent over the serial port.");

No entanto, você também pode enviar dados assim quando necessário:

    //Note that you can swap out the byte-array with a char-array if you so choose.
    byte[] data = new byte[1] { 255 };
    port.Write(data, 0, data.Length);

## Lendo/Gravando Dados Usando System.IO.File
Primeiro, vamos ver três maneiras diferentes de extrair dados de um arquivo.

    string fileText = File.ReadAllText(file);
    string[] fileLines = File.ReadAllLines(file);
    byte[] fileBytes = File.ReadAllBytes(file);

- Na primeira linha, lemos todos os dados do arquivo como uma string.
- Na segunda linha, lemos os dados do arquivo em um array de strings.
Cada linha no arquivo se torna um elemento na matriz.
- Na terceira lemos os bytes do arquivo.

----------

A seguir, vamos ver três métodos diferentes de **anexar** dados a um arquivo.
Se o arquivo especificado não existir, cada método criará automaticamente o arquivo antes de tentar anexar os dados a ele.

     File.AppendAllText(file, "Here is some data that is\nappended to the file.");
     File.AppendAllLines(file, new string[2] { "Here is some data that is", "appended to the file." });
     using (StreamWriter stream = File.AppendText(file))
     {
         stream.WriteLine("Here is some data that is");
         stream.Write("appended to the file.");
     }

- Na primeira linha, simplesmente adicionamos uma string ao final do arquivo especificado.
- Na segunda linha adicionamos cada elemento do array em uma nova linha no arquivo.
- Finalmente na terceira linha nós usamos `File.AppendText` para abrir um streamwriter que irá anexar quaisquer dados escritos nele.
----------
E por último, vamos ver três métodos diferentes de **gravar** dados em um arquivo.
A diferença entre *anexar* e *escrever* é que a gravação **sobrescreve** os dados no arquivo enquanto acrescenta **adiciona** aos dados no arquivo.
Se o arquivo especificado não existir, cada método criará automaticamente o arquivo antes de tentar gravar os dados nele.

    File.WriteAllText(file, "here is some data\nin this file.");
    File.WriteAllLines(file, new string[2] { "here is some data", "in this file" });
    File.WriteAllBytes(file, new byte[2] { 0, 255 });
- A primeira linha grava uma string no arquivo.
- A segunda linha escreve cada string no array em sua própria linha no arquivo.
- E a terceira linha permite que você escreva uma matriz de bytes no arquivo.

