---
title: "Cómo usar estructuras de C# para crear un tipo de unión (similar a las uniones de C)"
slug: "como-usar-estructuras-de-c-para-crear-un-tipo-de-union-similar-a-las-uniones-de-c"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

Los tipos de unión se utilizan en varios lenguajes, en particular el lenguaje C, para contener varios tipos diferentes que pueden "superponerse" en el mismo espacio de memoria. En otras palabras, pueden contener diferentes campos, todos los cuales comienzan en el mismo desplazamiento de memoria, incluso cuando pueden tener diferentes longitudes y tipos. Esto tiene la ventaja de ahorrar memoria y realizar una conversión automática.

Por favor, tenga en cuenta los comentarios en el constructor de Struct. El orden en que se inicializan los campos es extremadamente importante. Primero desea inicializar todos los demás campos y luego establecer el valor que pretende cambiar como la última declaración. Debido a que los campos se superponen, la configuración del último valor es la que cuenta.

## Uniones estilo C en C#
Los tipos de unión se usan en varios lenguajes, como el lenguaje C, para contener varios tipos diferentes que pueden "superponerse". En otras palabras, pueden contener diferentes campos, todos los cuales comienzan en el mismo desplazamiento de memoria, incluso cuando pueden tener diferentes longitudes y tipos. Esto tiene la ventaja de ahorrar memoria y realizar una conversión automática. Piense en una dirección IP, como ejemplo. Internamente, una dirección IP se representa como un número entero, pero a veces queremos acceder al componente Byte diferente, como en Byte1.Byte2.Byte3.Byte4. Esto funciona para cualquier tipo de valor, ya sean primitivos como Int32 o long, o para otras estructuras que defina usted mismo.

Podemos lograr el mismo efecto en C# mediante el uso de estructuras de diseño explícito.

    using System;
    using System.Runtime.InteropServices;

    // The struct needs to be annotated as "Explicit Layout"
    [StructLayout(LayoutKind.Explicit)]
    struct IpAddress
    {
        // The "FieldOffset" means that this Integer starts, an offset in bytes.
        // sizeof(int) 4, sizeof(byte) = 1
        [FieldOffset(0)] public int Address;
        [FieldOffset(0)] public byte Byte1;
        [FieldOffset(1)] public byte Byte2;
        [FieldOffset(2)] public byte Byte3;
        [FieldOffset(3)] public byte Byte4;

        public IpAddress(int address) : this()
        {
            // When we init the Int, the Bytes will change too.
            Address = address;
        }

        // Now we can use the explicit layout to access the 
        // bytes separately, without doing any conversion.
        public override string ToString() => $"{Byte1}.{Byte2}.{Byte3}.{Byte4}";
    }
    
Habiendo definido Struct de esta manera, podemos usarlo como lo haríamos
use una Unión en C. Por ejemplo, vamos a crear una dirección IP como
Entero aleatorio y luego modifique el primer token en la dirección
a '100', cambiándolo de 'A.B.C.D' a '100.B.C.D':
                     
    var ip = new IpAddress(new Random().Next());
    Console.WriteLine($"{ip} = {ip.Address}");
    ip.Byte1 = 100;
    Console.WriteLine($"{ip} = {ip.Address}");

Producción:

    75.49.5.32 = 537211211
    100.49.5.32 = 537211236

[Ver demostración][1]


[1]: https://dotnetfiddle.net/CnrgBi

## Los tipos de unión en C# también pueden contener campos de estructura
Además de las primitivas, las estructuras de diseño explícito (uniones) en C# también pueden contener otras estructuras. Siempre que un campo sea un tipo de valor y no una referencia, puede estar contenido en una unión:

    using System;
    using System.Runtime.InteropServices;

    // The struct needs to be annotated as "Explicit Layout"
    [StructLayout(LayoutKind.Explicit)]
    struct IpAddress
    {
        // Same definition of IpAddress, from the example above
    }

    // Now let's see if we can fit a whole URL into a long

    // Let's define a short enum to hold protocols
    enum Protocol : short { Http, Https, Ftp, Sftp, Tcp }

    // The Service struct will hold the Address, the Port and the Protocol
    [StructLayout(LayoutKind.Explicit)]
    struct Service
    {
        [FieldOffset(0)] public IpAddress Address;
        [FieldOffset(4)] public ushort Port;
        [FieldOffset(6)] public Protocol AppProtocol;
        [FieldOffset(0)] public long Payload;

        public Service(IpAddress address, ushort port, Protocol protocol)
        {
            Payload = 0;
            Address = address;
            Port  = port;
            AppProtocol = protocol;
        }

        public Service(long payload)
        {
            Address = new IpAddress(0);
            Port = 80;
            AppProtocol = Protocol.Http;
            Payload = payload;
        }

        public Service Copy() => new Service(Payload);

        public override string ToString() => $"{AppProtocol}//{Address}:{Port}/";
    }
    
Ahora podemos verificar que todo el Service Union cabe en el tamaño de un largo (8 bytes).

    var ip = new IpAddress(new Random().Next());
    Console.WriteLine($"Size: {Marshal.SizeOf(ip)} bytes. Value: {ip.Address} = {ip}.");

    var s1 = new Service(ip, 8080, Protocol.Https);
    var s2 = new Service(s1.Payload);
    s2.Address.Byte1 = 100;
    s2.AppProtocol = Protocol.Ftp;

    Console.WriteLine($"Size: {Marshal.SizeOf(s1)} bytes. Value: {s1.Address} = {s1}.");
    Console.WriteLine($"Size: {Marshal.SizeOf(s2)} bytes. Value: {s2.Address} = {s2}.");

[Ver demostración][1]


[1]: https://dotnetfiddle.net/cROlki

