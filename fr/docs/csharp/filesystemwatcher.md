---
title: "FileSystemWatcher"
slug: "filesystemwatcher"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Syntaxe
- public FileSystemWatcher()
- public FileSystemWatcher (chemin de chaîne)
- public FileSystemWatcher (chemin de chaîne, filtre de chaîne)

## Paramètres
|chemin |filtre|
|---|---|
|Le répertoire à surveiller, en notation standard ou UNC (Universal Naming Convention).||Le type de fichiers à surveiller. Par exemple, "*.txt" surveille les modifications apportées à tous les fichiers texte.|


## EstFichierPrêt
Une erreur courante que beaucoup de gens commencent avec FileSystemWatcher ne tient pas compte du fait que l'événement FileWatcher est déclenché dès que le fichier est créé.
Cependant, la finalisation du fichier peut prendre un certain temps.

*Exemple*:

Prenez une taille de fichier de 1 Go par exemple. Le fichier apr ask a été créé par un autre programme (Explorer.exe le copiant de quelque part) mais cela prendra quelques minutes pour terminer ce processus. L'événement est déclenché au moment de la création et vous devez attendre que le fichier soit prêt à être copié.

C'est une méthode pour vérifier si le fichier est prêt.



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

## FileWatcher de base
L'exemple suivant crée un `FileSystemWatcher` pour surveiller le répertoire spécifié au moment de l'exécution. Le composant est configuré pour surveiller les modifications de l'heure **LastWrite** et **LastAccess**, la création, la suppression ou le changement de nom des fichiers texte dans le répertoire. Si un fichier est modifié, créé ou supprimé, le chemin d'accès au fichier s'imprime sur la console. Lorsqu'un fichier est renommé, l'ancien et le nouveau chemin s'impriment sur la console.

Utilisez les espaces de noms System.Diagnostics et System.IO pour cet exemple.

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

