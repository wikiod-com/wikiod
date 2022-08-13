---
title: "Lire et écrire des fichiers Zip"
slug: "lire-et-ecrire-des-fichiers-zip"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

La classe **ZipFile** réside dans l'espace de noms **System.IO.Compression**. Il peut être utilisé pour lire et écrire dans des fichiers Zip.

* Vous pouvez également utiliser un MemoryStream au lieu d'un FileStream.

* Exceptions

| exception | Condition
| --------------------------- | ------ 
| ArgumentException | Le flux a déjà été fermé, ou les capacités du flux ne correspondent pas au mode (par exemple : tentative d'écriture dans un flux en lecture seule)
| ArgumentNullException | l'entrée *stream* est nulle
| ArgumentOutOfRangeException | *mode* a une valeur invalide
| InvalidDataException | Voir la liste ci-dessous

Lorsqu'une **InvalidDataException** est levée, elle peut avoir 3 causes :

* Le contenu du flux ne peut pas être interprété comme une archive zip
* *mode* est Mise à jour et une entrée manque dans l'archive ou est corrompue et ne peut pas être lue
* *mode* est mise à jour et une entrée est trop grande pour tenir dans la mémoire

Toutes les informations proviennent de [cette page MSDN](https://msdn.microsoft.com/en-us/library/system.io.compression.ziparchive(v=vs.110).aspx)

## Liste du contenu ZIP
Cet extrait répertorie tous les noms de fichiers d'une archive zip. Les noms de fichiers sont relatifs à la racine zip.

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        for (int i = 0; i < archive.Entries.Count; i++)
        {
            Console.WriteLine($"{i}: {archive.Entries[i]}");
        }
    }

## Extraction de fichiers à partir de fichiers ZIP
Extraire tous les fichiers dans un répertoire est très simple :

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        archive.ExtractToDirectory(AppDomain.CurrentDomain.BaseDirectory);
    }

Lorsque le fichier existe déjà, une **System.IO.IOException** sera levée.

Extraction de fichiers spécifiques :




    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        // Get a root entry file
        archive.GetEntry("test.txt").ExtractToFile("test_extracted_getentries.txt", true);

        // Enter a path if you want to extract files from a subdirectory
        archive.GetEntry("sub/subtest.txt").ExtractToFile("test_sub.txt", true);

        // You can also use the Entries property to find files
        archive.Entries.FirstOrDefault(f => f.Name == "test.txt")?.ExtractToFile("test_extracted_linq.txt", true);

        // This will throw a System.ArgumentNullException because the file cannot be found
        archive.GetEntry("nonexistingfile.txt").ExtractToFile("fail.txt", true);
    }

Chacune de ces méthodes produira le même résultat.

## Mise à jour d'un fichier ZIP
Pour mettre à jour un fichier ZIP, le fichier doit être ouvert avec ZipArchiveMode.Update à la place.

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Update))
    {
        // Add file to root
        archive.CreateEntryFromFile("test.txt", "test.txt");

        // Add file to subfolder
        archive.CreateEntryFromFile("test.txt", "symbols/test.txt");
    }

Il existe également la possibilité d'écrire directement dans un fichier de l'archive :

    var entry = archive.CreateEntry("createentry.txt");
    using(var writer = new StreamWriter(entry.Open()))
    {
        writer.WriteLine("Test line");
    }

