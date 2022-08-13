---
title: "Lendo e gravando arquivos Zip"
slug: "lendo-e-gravando-arquivos-zip"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

A classe **ZipFile** reside no namespace **System.IO.Compression**. Ele pode ser usado para ler e gravar em arquivos Zip.

* Você também pode usar um MemoryStream em vez de um FileStream.

* Exceções

| Exceção | Doença
| --------------------------- | ------ 
| ArgumentException | O fluxo já foi fechado ou os recursos do fluxo não correspondem ao modo (por exemplo: tentando gravar em um fluxo somente leitura)
| ArgumentNullException | entrada *stream* é nula
| ArgumentOutOfRangeException | *modo* tem um valor inválido
| InvalidDataException | Veja lista abaixo

Quando uma **InvalidDataException** é lançada, ela pode ter 3 causas:

* O conteúdo do stream não pôde ser interpretado como um arquivo zip
* *modo* é Atualização e uma entrada está faltando no arquivo ou está corrompida e não pode ser lida
* *modo* é Atualização e uma entrada é muito grande para caber na memória

Todas as informações foram retiradas de [esta página do MSDN](https://msdn.microsoft.com/en-us/library/system.io.compression.ziparchive(v=vs.110).aspx)

## Listando o conteúdo ZIP
Este snippet listará todos os nomes de arquivo de um arquivo zip. Os nomes dos arquivos são relativos à raiz zip.

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        for (int i = 0; i < archive.Entries.Count; i++)
        {
            Console.WriteLine($"{i}: {archive.Entries[i]}");
        }
    }

## Extraindo arquivos de arquivos ZIP
Extrair todos os arquivos em um diretório é muito fácil:

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        archive.ExtractToDirectory(AppDomain.CurrentDomain.BaseDirectory);
    }

Quando o arquivo já existir, uma **System.IO.IOException** será lançada.

Extraindo arquivos específicos:




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

Qualquer um desses métodos produzirá o mesmo resultado.

## Atualizando um arquivo ZIP
Para atualizar um arquivo ZIP, o arquivo deve ser aberto com ZipArchiveMode.Update.

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Update))
    {
        // Add file to root
        archive.CreateEntryFromFile("test.txt", "test.txt");

        // Add file to subfolder
        archive.CreateEntryFromFile("test.txt", "symbols/test.txt");
    }

Há também a opção de gravar diretamente em um arquivo dentro do arquivo:

    var entry = archive.CreateEntry("createentry.txt");
    using(var writer = new StreamWriter(entry.Open()))
    {
        writer.WriteLine("Test line");
    }

