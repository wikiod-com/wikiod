---
title: "Observador del sistema de archivos"
slug: "observador-del-sistema-de-archivos"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Sintaxis
- público FileSystemWatcher ()
- público FileSystemWatcher (ruta de la cadena)
- FileSystemWatcher público (ruta de cadena, filtro de cadena)

## Parámetros
|ruta |filtro|
|---|---|
|El directorio a monitorear, en notación estándar o Universal Naming Convention (UNC).||El tipo de archivos a monitorear. Por ejemplo, "*.txt" busca cambios en todos los archivos de texto.|


## EstáElArchivoListo
Un error común que cometen muchas personas que comienzan con FileSystemWatcher es no tener en cuenta que el evento FileWatcher se genera tan pronto como se crea el archivo.
Sin embargo, es posible que el archivo tarde algún tiempo en completarse.

*Ejemplo*:

Tome un tamaño de archivo de 1 GB, por ejemplo. El archivo apr ask creado por otro programa (Explorer.exe copiándolo desde algún lugar) pero tomará minutos terminar ese proceso. El evento se genera en el momento de la creación y debe esperar a que el archivo esté listo para ser copiado.

Este es un método para verificar si el archivo está listo.



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

## Observador de archivos básico
El siguiente ejemplo crea un `FileSystemWatcher` para observar el directorio especificado en tiempo de ejecución. El componente está configurado para observar cambios en la hora **LastWrite** y **LastAccess**, la creación, eliminación o cambio de nombre de archivos de texto en el directorio. Si se cambia, crea o elimina un archivo, la ruta al archivo se imprime en la consola. Cuando se cambia el nombre de un archivo, las rutas antiguas y nuevas se imprimen en la consola.

Utilice los espacios de nombres System.Diagnostics y System.IO para este ejemplo.

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

