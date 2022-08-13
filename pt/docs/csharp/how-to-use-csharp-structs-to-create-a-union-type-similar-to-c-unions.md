---
title: "Como usar C# Structs para criar um tipo de união (semelhante a C Unions)"
slug: "como-usar-c-structs-para-criar-um-tipo-de-uniao-semelhante-a-c-unions"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

Os tipos de união são usados ​​em várias linguagens, principalmente na linguagem C, para conter vários tipos diferentes que podem "sobrepor-se" no mesmo espaço de memória. Em outras palavras, eles podem conter campos diferentes que começam no mesmo deslocamento de memória, mesmo quando podem ter comprimentos e tipos diferentes. Isso tem o benefício de economizar memória e fazer conversão automática.

Por favor, observe os comentários no construtor do Struct. A ordem em que os campos são inicializados é extremamente importante. Você deseja inicializar primeiro todos os outros campos e, em seguida, definir o valor que pretende alterar como a última instrução. Como os campos se sobrepõem, a configuração do último valor é a que conta.

## Uniões de estilo C em C#
Os tipos de união são usados ​​em várias linguagens, como a linguagem C, para conter vários tipos diferentes que podem se "sobrepor". Em outras palavras, eles podem conter campos diferentes que começam no mesmo deslocamento de memória, mesmo quando podem ter comprimentos e tipos diferentes. Isso tem o benefício de economizar memória e fazer conversão automática. Pense em um endereço IP, como exemplo. Internamente, um endereço IP é representado como um inteiro, mas às vezes queremos acessar o componente Byte diferente, como em Byte1.Byte2.Byte3.Byte4. Isso funciona para qualquer tipo de valor, sejam primitivos como Int32 ou long, ou para outras estruturas que você mesmo define.

Podemos obter o mesmo efeito em C# usando Explicit Layout Structs.

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
    
Tendo definido o Struct dessa maneira, podemos usá-lo como faríamos
use uma União em C. Por exemplo, vamos criar um endereço IP como um
Random Integer e, em seguida, modifique o primeiro token no endereço
para '100', alterando-o de 'A.B.C.D' para '100.B.C.D':
                     
    var ip = new IpAddress(new Random().Next());
    Console.WriteLine($"{ip} = {ip.Address}");
    ip.Byte1 = 100;
    Console.WriteLine($"{ip} = {ip.Address}");

Resultado:

    75.49.5.32 = 537211211
    100.49.5.32 = 537211236

[Ver demonstração][1]


[1]: https://dotnetfiddle.net/CnrgBi

## Tipos de união em C# também podem conter campos Struct
Além das primitivas, as estruturas de layout explícito (Unions) em C# também podem conter outras estruturas. Desde que um campo seja um tipo de valor e não uma referência, ele pode estar contido em uma união:

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
    
Podemos agora verificar se toda a União de Serviço cabe no tamanho de um long (8 bytes).

    var ip = new IpAddress(new Random().Next());
    Console.WriteLine($"Size: {Marshal.SizeOf(ip)} bytes. Value: {ip.Address} = {ip}.");

    var s1 = new Service(ip, 8080, Protocol.Https);
    var s2 = new Service(s1.Payload);
    s2.Address.Byte1 = 100;
    s2.AppProtocol = Protocol.Ftp;

    Console.WriteLine($"Size: {Marshal.SizeOf(s1)} bytes. Value: {s1.Address} = {s1}.");
    Console.WriteLine($"Size: {Marshal.SizeOf(s2)} bytes. Value: {s2.Address} = {s2}.");

[Ver demonstração][1]


[1]: https://dotnetfiddle.net/cROlki

