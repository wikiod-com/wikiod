---
title: "FileSystemWatcher"
slug: "filesystemwatcher"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Sintaxe
- public FileSystemWatcher()
- público FileSystemWatcher (caminho da string)
- public FileSystemWatcher(caminho de string, filtro de string)

## Parâmetros
|caminho |filtro|
|---|---|
|O diretório a ser monitorado, em notação padrão ou Universal Naming Convention (UNC).||O tipo de arquivos a ser observado. Por exemplo, "*.txt" observa as alterações em todos os arquivos de texto.|


##FileReady
Um erro comum que muitas pessoas que estão começando com o FileSystemWatcher cometem é não levar em conta que o evento FileWatcher é gerado assim que o arquivo é criado.
No entanto, pode levar algum tempo para que o arquivo seja concluído.

*Exemplo*:

Pegue um tamanho de arquivo de 1 GB, por exemplo. O arquivo apr ask criado por outro programa (Explorer.exe copiando-o de algum lugar), mas levará alguns minutos para concluir esse processo. O evento é gerado nesse momento de criação e você precisa aguardar o arquivo estar pronto para ser copiado.

Este é um método para verificar se o arquivo está pronto.



     public static bool IsFileReady(String sFilename)
    {
        // If the file can be opened for exclusive access it means that the file
        // is no longer locked by another process.
        try
        {
            using (FileStream inputStream = File.Open(sFilename, FileMode.Open, FileAccess.Read, FileShare.None))
            {
                if (inputStream.Length > 0)
                {
                    return true;
                }
                else
                {
                    return false;
                }

            }
        }
        catch (Exception)
        {
            return false;
        }
    }

## Observador de Arquivos Básico
O exemplo a seguir cria um `FileSystemWatcher` para observar o diretório especificado em tempo de execução. O componente está configurado para observar as alterações no tempo de **LastWrite** e **LastAccess**, a criação, exclusão ou renomeação de arquivos de texto no diretório. Se um arquivo for alterado, criado ou excluído, o caminho para o arquivo será impresso no console. Quando um arquivo é renomeado, os caminhos antigo e novo são impressos no console.

Use os namespaces System.Diagnostics e System.IO para este exemplo.

    FileSystemWatcher watcher;

    private void watch()
    {
      // Create a new FileSystemWatcher and set its properties.
      watcher = new FileSystemWatcher();
      watcher.Path = path;

     /* Watch for changes in LastAccess and LastWrite times, and
           the renaming of files or directories. */
      watcher.NotifyFilter = NotifyFilters.LastAccess | NotifyFilters.LastWrite
                             | NotifyFilters.FileName | NotifyFilters.DirectoryName;

      // Only watch text files.      
      watcher.Filter = "*.txt*";

      // Add event handler.
      watcher.Changed += new FileSystemEventHandler(OnChanged);
      // Begin watching.      
      watcher.EnableRaisingEvents = true;
    }

    // Define the event handler.
    private void OnChanged(object source, FileSystemEventArgs e)
    {
      //Copies file to another directory or another action.
      Console.WriteLine("File: " +  e.FullPath + " " + e.ChangeType);
    }

