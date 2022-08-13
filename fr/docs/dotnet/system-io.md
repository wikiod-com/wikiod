---
title: "System.IO"
slug: "systemio"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Lecture d'un fichier texte avec StreamReader


## Ports série utilisant System.IO.SerialPorts
Itération sur les ports série connectés
-------------------------------------

    using System.IO.Ports;
    string[] ports = SerialPort.GetPortNames();
    for (int i = 0; i < ports.Length; i++)
    {
        Console.WriteLine(ports[i]);
    }

----------


Instanciation d'un objet System.IO.SerialPort
------------------------------------------------

    using System.IO.Ports;
    SerialPort port = new SerialPort();
    SerialPort port = new SerialPort("COM 1"); ;
    SerialPort port = new SerialPort("COM 1", 9600);
**REMARQUE** : Ce ne sont que trois des sept surcharges du constructeur pour le type SerialPort.

----------

Lecture/écriture de données sur le port série
----------------------------------------

Le plus simple est d'utiliser les méthodes `SerialPort.Read` et `SerialPort.Write`.
Cependant, vous pouvez également récupérer un objet `System.IO.Stream` que vous pouvez utiliser pour diffuser des données sur le SerialPort. Pour ce faire, utilisez `SerialPort.BaseStream`.

**En lisant**

    int length = port.BytesToRead;
    //Note that you can swap out a byte-array for a char-array if you prefer.
    byte[] buffer = new byte[length];
    port.Read(buffer, 0, length);

Vous pouvez également lire toutes les données disponibles :

    string curData = port.ReadExisting();

Ou lisez simplement jusqu'à la première nouvelle ligne rencontrée dans les données entrantes :

    string line = port.ReadLine();

**L'écriture**

Le moyen le plus simple d'écrire des données sur le port série est :

    port.Write("here is some text to be sent over the serial port.");

Cependant, vous pouvez également envoyer des données comme ceci si nécessaire :

    //Note that you can swap out the byte-array with a char-array if you so choose.
    byte[] data = new byte[1] { 255 };
    port.Write(data, 0, data.Length);

## Lecture/écriture de données à l'aide de System.IO.File
Voyons d'abord trois manières différentes d'extraire des données d'un fichier.

    string fileText = File.ReadAllText(file);
    string[] fileLines = File.ReadAllLines(file);
    byte[] fileBytes = File.ReadAllBytes(file);

- Sur la première ligne, nous lisons toutes les données du fichier sous forme de chaîne.
- Sur la deuxième ligne, nous lisons les données du fichier dans un tableau de chaînes.
Chaque ligne du fichier devient un élément du tableau.
- Le troisième, nous lisons les octets du fichier.

----------

Voyons maintenant trois méthodes différentes pour **ajouter** des données à un fichier.
Si le fichier que vous spécifiez n'existe pas, chaque méthode créera automatiquement le fichier avant de tenter d'y ajouter les données.

     File.AppendAllText(file, "Here is some data that is\nappended to the file.");
     File.AppendAllLines(file, new string[2] { "Here is some data that is", "appended to the file." });
     using (StreamWriter stream = File.AppendText(file))
     {
         stream.WriteLine("Here is some data that is");
         stream.Write("appended to the file.");
     }

- Sur la première ligne, nous ajoutons simplement une chaîne à la fin du fichier spécifié.
- Sur la deuxième ligne, nous ajoutons chaque élément du tableau sur une nouvelle ligne du fichier.
- Enfin, sur la troisième ligne, nous utilisons `File.AppendText` pour ouvrir un streamwriter qui ajoutera toutes les données qui y sont écrites.
----------
Et enfin, voyons trois méthodes différentes d'**écriture** de données dans un fichier.
La différence entre *ajouter* et *écrire* étant que l'écriture **écrase** les données du fichier tandis que l'ajout **ajoute** aux données du fichier.
Si le fichier que vous spécifiez n'existe pas, chaque méthode créera automatiquement le fichier avant de tenter d'y écrire les données.

    File.WriteAllText(file, "here is some data\nin this file.");
    File.WriteAllLines(file, new string[2] { "here is some data", "in this file" });
    File.WriteAllBytes(file, new byte[2] { 0, 255 });
- La première ligne écrit une chaîne dans le fichier.
- La deuxième ligne écrit chaque chaîne du tableau sur sa propre ligne dans le fichier.
- Et la troisième ligne vous permet d'écrire un tableau d'octets dans le fichier.

