---
title: "Leer y escribir archivos Zip"
slug: "leer-y-escribir-archivos-zip"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

La clase **ZipFile** vive en el espacio de nombres **System.IO.Compression**. Se puede utilizar para leer y escribir en archivos Zip.

* También puede usar un MemoryStream en lugar de un FileStream.

* Excepciones

| Excepción | Condición
| --------------------------- | ------ 
| ArgumentoExcepción | La secuencia ya se ha cerrado o las capacidades de la secuencia no coinciden con el modo (por ejemplo, tratando de escribir en una secuencia de solo lectura)
| ArgumentNullException | la entrada *stream* es nula
| ArgumentOutOfRangeException | *modo* tiene un valor inválido
| Excepción de datos no válidos | Ver la lista a continuación

Cuando se lanza una **InvalidDataException**, puede tener 3 causas:

* El contenido de la transmisión no se pudo interpretar como un archivo zip
* *modo* es Actualizar y falta una entrada en el archivo o está dañada y no se puede leer
* *modo* es Actualizar y una entrada es demasiado grande para caber en la memoria

Toda la información se ha extraído de [esta página de MSDN](https://msdn.microsoft.com/en-us/library/system.io.compression.ziparchive(v=vs.110).aspx)

## Listado de contenidos ZIP
Este fragmento enumerará todos los nombres de archivo de un archivo zip. Los nombres de archivo son relativos a la raíz zip.

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        for (int i = 0; i < archive.Entries.Count; i++)
        {
            Console.WriteLine($"{i}: {archive.Entries[i]}");
        }
    }

## Extraer archivos de archivos ZIP
Extraer todos los archivos a un directorio es muy fácil:

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        archive.ExtractToDirectory(AppDomain.CurrentDomain.BaseDirectory);
    }

Cuando el archivo ya existe, se lanzará una **System.IO.IOException**.

Extracción de archivos específicos:




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

Cualquiera de estos métodos producirá el mismo resultado.

## Actualizar un archivo ZIP
Para actualizar un archivo ZIP, el archivo debe abrirse con ZipArchiveMode.Update en su lugar.

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Update))
    {
        // Add file to root
        archive.CreateEntryFromFile("test.txt", "test.txt");

        // Add file to subfolder
        archive.CreateEntryFromFile("test.txt", "symbols/test.txt");
    }

También existe la opción de escribir directamente en un archivo dentro del archivo:

    var entry = archive.CreateEntry("createentry.txt");
    using(var writer = new StreamWriter(entry.Open()))
    {
        writer.WriteLine("Test line");
    }

