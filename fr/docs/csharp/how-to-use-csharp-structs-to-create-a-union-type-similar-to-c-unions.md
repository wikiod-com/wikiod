---
title: "Comment utiliser C # Structs pour créer un type d'union (similaire aux unions C)"
slug: "comment-utiliser-c--structs-pour-creer-un-type-dunion-similaire-aux-unions-c"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

Les types d'union sont utilisés dans plusieurs langages, notamment le langage C, pour contenir plusieurs types différents qui peuvent "se chevaucher" dans le même espace mémoire. En d'autres termes, ils peuvent contenir différents champs qui commencent tous au même décalage de mémoire, même s'ils peuvent avoir des longueurs et des types différents. Cela a l'avantage d'économiser de la mémoire et d'effectuer une conversion automatique.

Veuillez noter les commentaires dans le constructeur de Struct. L'ordre dans lequel les champs sont initialisés est extrêmement important. Vous souhaitez d'abord initialiser tous les autres champs, puis définir la valeur que vous avez l'intention de modifier comme dernière instruction. Étant donné que les champs se chevauchent, la dernière configuration de valeur est celle qui compte.

## Unions de style C en C#
Les types d'union sont utilisés dans plusieurs langages, comme le langage C, pour contenir plusieurs types différents qui peuvent "se chevaucher". En d'autres termes, ils peuvent contenir différents champs qui commencent tous au même décalage de mémoire, même s'ils peuvent avoir des longueurs et des types différents. Cela a l'avantage d'économiser de la mémoire et d'effectuer une conversion automatique. Pensez à une adresse IP, par exemple. En interne, une adresse IP est représentée sous la forme d'un entier, mais parfois nous souhaitons accéder aux différents composants Byte, comme dans Byte1.Byte2.Byte3.Byte4. Cela fonctionne pour tous les types de valeur, qu'il s'agisse de primitives comme Int32 ou long, ou pour d'autres structures que vous définissez vous-même.

Nous pouvons obtenir le même effet en C # en utilisant des structures de mise en page explicites.

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
    
Après avoir défini Struct de cette manière, nous pouvons l'utiliser comme nous le ferions
utilisez une Union en C. Par exemple, créons une adresse IP en tant que
Entier aléatoire puis modifier le premier jeton de l'adresse
à '100', en le changeant de 'A.B.C.D' à '100.B.C.D' :
                     
    var ip = new IpAddress(new Random().Next());
    Console.WriteLine($"{ip} = {ip.Address}");
    ip.Byte1 = 100;
    Console.WriteLine($"{ip} = {ip.Address}");

Production:

    75.49.5.32 = 537211211
    100.49.5.32 = 537211236

[Voir la démo][1]


[1] : https://dotnetfiddle.net/CnrgBi

## Les types Union en C# peuvent également contenir des champs Struct
Outre les primitives, les structures de mise en page explicites (Unions) en C# peuvent également contenir d'autres structures. Tant qu'un champ est de type Valeur et non de Référence, il peut être contenu dans une Union :

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
    
Nous pouvons maintenant vérifier que l'ensemble de Service Union tient dans la taille d'un long (8 octets).

    var ip = new IpAddress(new Random().Next());
    Console.WriteLine($"Size: {Marshal.SizeOf(ip)} bytes. Value: {ip.Address} = {ip}.");

    var s1 = new Service(ip, 8080, Protocol.Https);
    var s2 = new Service(s1.Payload);
    s2.Address.Byte1 = 100;
    s2.AppProtocol = Protocol.Ftp;

    Console.WriteLine($"Size: {Marshal.SizeOf(s1)} bytes. Value: {s1.Address} = {s1}.");
    Console.WriteLine($"Size: {Marshal.SizeOf(s2)} bytes. Value: {s2.Address} = {s2}.");

[Voir la démo][1]


[1] : https://dotnetfiddle.net/cROlki

