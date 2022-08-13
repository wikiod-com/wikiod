---
title: "Classe System.IO.File"
slug: "classe-systemiofile"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Syntaxe
- source de chaîne ;
- destination de la chaîne ;

## Paramètres
| Paramètre | Détails |
| ---------------- | ------- |
| `source` | Le fichier qui doit être déplacé vers un autre emplacement. |
| `destination` | Le répertoire dans lequel vous souhaitez déplacer `source` (cette variable doit également contenir le nom (et l'extension de fichier) du fichier. |

## Supprimer un fichier
Pour supprimer un fichier (si vous disposez des autorisations requises), c'est aussi simple que :

    File.Delete(path);

Cependant, beaucoup de choses peuvent mal tourner :

* Vous ne disposez pas des autorisations requises (`UnauthorizedAccessException` est levé).
* Le fichier peut être utilisé par quelqu'un d'autre (`IOException` est lancé).
* Le fichier ne peut pas être supprimé en raison d'une erreur de bas niveau ou du support en lecture seule (`IOException` est lancé).
* Le fichier n'existe plus (`IOException` est lancé).

Notez que le dernier point (le fichier n'existe pas) est généralement _contourné_ avec un extrait de code comme celui-ci :

    if (File.Exists(path))
        File.Delete(path);

Cependant, ce n'est pas une opération atomique et le fichier peut être supprimé par quelqu'un d'autre entre l'appel à `File.Exists()` et avant `File.Delete()`. La bonne approche pour gérer l'opération d'E/S nécessite la gestion des exceptions (en supposant qu'une autre ligne de conduite peut être prise en cas d'échec de l'opération) :

    if (File.Exists(path))
    {
        try
        {
            File.Delete(path);
        }
        catch (IOException exception)
        {
            if (!File.Exists(path))
                return; // Someone else deleted this file
    
            // Something went wrong...
        }
        catch (UnauthorizedAccessException exception)
        {
            // I do not have required permissions
        }
    }

Notez que ces erreurs d'E / S sont parfois transitoires (fichier en cours d'utilisation, par exemple) et si une connexion réseau est impliquée, elle peut se rétablir automatiquement sans aucune action de notre part. Il est alors courant de _réessayer_ une opération d'E/S plusieurs fois avec un petit délai entre chaque tentative :

    public static void Delete(string path)
    {
        if (!File.Exists(path))
            return;
    
        for (int i=1; ; ++i)
        {
            try
            {
                File.Delete(path);
                return;
            }
            catch (IOException e)
            {
                if (!File.Exists(path))
                    return;
    
                if (i == NumberOfAttempts)
                    throw;
    
                Thread.Sleep(DelayBetweenEachAttempt);
            }
    
            // You may handle UnauthorizedAccessException but this issue
            // will probably won't be fixed in few seconds...
        }
    }
    
    private const int NumberOfAttempts = 3;
    private const int DelayBetweenEachAttempt = 1000; // ms

Remarque : dans l'environnement Windows, le fichier ne sera pas vraiment supprimé lorsque vous appelez cette fonction. Si quelqu'un d'autre ouvre le fichier à l'aide de "FileShare.Delete", le fichier peut être supprimé, mais cela ne se produira effectivement que lorsque le propriétaire fermera le fichier.

## Supprimer les lignes indésirables d'un fichier texte
Modifier un fichier texte n'est pas facile car son contenu doit être déplacé. Pour les _petits_ fichiers, la méthode la plus simple consiste à lire son contenu en mémoire, puis à réécrire le texte modifié.

Dans cet exemple, nous lisons toutes les lignes d'un fichier et supprimons toutes les lignes vides, puis nous réécrivons dans le chemin d'origine :

    File.WriteAllLines(path,
        File.ReadAllLines(path).Where(x => !String.IsNullOrWhiteSpace(x)));

Si le fichier est trop volumineux pour être chargé en mémoire et que le chemin de sortie est différent du chemin d'entrée :

    File.WriteAllLines(outputPath,
        File.ReadLines(inputPath).Where(x => !String.IsNullOrWhiteSpace(x)));
    

## Convertir l'encodage du fichier texte
Le texte est enregistré encodé (voir aussi le sujet [Strings][1]) alors parfois vous devrez peut-être changer son encodage, cet exemple suppose (pour simplifier) ​​que le fichier n'est pas trop gros et qu'il peut être entièrement lu en mémoire :

    public static void ConvertEncoding(string path, Encoding from, Encoding to)
    {
        File.WriteAllText(path, File.ReadAllText(path, from), to);
    }

Lorsque vous effectuez des conversions, n'oubliez pas que le fichier peut contenir un BOM (Byte Order Mark), pour mieux comprendre comment il est géré, reportez-vous à [Encoding.UTF8.GetString ne prend pas en compte le Préambule/BOM][2].


[1] : https://www.wikiod.com/fr/dotnet/cordes
[2] : http://stackoverflow.com/q/11701341/1207195

## Enumérer les fichiers plus anciens qu'un nombre spécifié
Cet extrait est une fonction d'assistance pour énumérer tous les fichiers plus anciens qu'un âge spécifié, il est utile - par exemple - lorsque vous devez supprimer d'anciens fichiers journaux ou d'anciennes données mises en cache.

    static IEnumerable<string> EnumerateAllFilesOlderThan(
                                   TimeSpan maximumAge,
                                   string path,
                                   string searchPattern = "*.*",
                                   SearchOption options = SearchOption.TopDirectoryOnly)
    {
        DateTime oldestWriteTime = DateTime.Now - maximumAge;

        return Directory.EnumerateFiles(path, searchPattern, options)
            .Where(x => Directory.GetLastWriteTime(x) < oldestWriteTime);
    }

Utilisé comme ceci :

    var oldFiles = EnumerateAllFilesOlderThan(TimeSpan.FromDays(7), @"c:\log", "*.log");

Peu de choses à noter :

* La recherche est effectuée en utilisant `Directory.EnumerateFiles()` au lieu de `Directory.GetFiles()`. L'énumération est _alive_ alors vous n'aurez pas besoin d'attendre que toutes les entrées du système de fichiers aient été récupérées.
* Nous vérifions l'heure de la dernière écriture, mais vous pouvez utiliser l'heure de création ou l'heure du dernier accès (par exemple, pour supprimer les fichiers en cache _inutilisés_, notez que l'heure d'accès peut être désactivée).
* La granularité n'est pas uniforme pour toutes ces propriétés (heure d'écriture, heure d'accès, heure de création), consultez MSDN pour plus de détails à ce sujet.


## Déplacer un fichier d'un emplacement à un autre
<h1>Fichier.Déplacer</h1>
Pour déplacer un fichier d'un emplacement à un autre, une simple ligne de code peut y parvenir :

`Fichier.Déplacer(@"C:\FichierTemporaire.txt", @"C:\FichiersTemporaire\FichierTemporaire.txt");`

Cependant, il y a beaucoup de choses qui pourraient mal tourner avec cette opération simple. Par exemple, que se passe-t-il si l'utilisateur exécutant votre programme n'a pas de lecteur portant la mention « C » ? Et s'ils le faisaient - mais qu'ils décidaient de le renommer en "B" ou "M" ?

Que se passe-t-il si le fichier source (le fichier dans lequel vous souhaitez vous déplacer) a été déplacé à votre insu - ou s'il n'existe tout simplement pas.

Ceci peut être contourné en vérifiant d'abord si le fichier source existe :

    string source = @"C:\TemporaryFile.txt", destination = @"C:\TemporaryFiles\TemporaryFile.txt";
    if(File.Exists("C:\TemporaryFile.txt"))
    {
        File.Move(source, destination);
    }

Cela garantira qu'à ce moment précis, le fichier existe et peut être déplacé vers un autre emplacement. Il peut y avoir des moments où un simple appel à `File.Exists` ne suffira pas. Si ce n'est pas le cas, vérifiez à nouveau, indiquez à l'utilisateur que l'opération a échoué - ou gérez l'exception.

Une `FileNotFoundException` n'est pas la seule exception que vous êtes susceptible de rencontrer.

Voir ci-dessous pour les exceptions possibles :

| Type d'exception | Descriptif |
| ------ | ------ |
| `IOException` | Le fichier existe déjà ou le fichier source est introuvable.
| `ArgumentNullException`| La valeur des paramètres Source et/ou Destination est nulle. |
| `ArgumentException` | La valeur des paramètres Source et/ou Destination est vide ou contient des caractères non valides. |
| `Exception d'accès non autorisé` | Vous ne disposez pas des autorisations requises pour effectuer cette action. |
| `PathTooLongException` | La source, la destination ou le ou les chemins spécifiés dépassent la longueur maximale. Sous Windows, la longueur d'un chemin doit être inférieure à 248 caractères, tandis que les noms de fichiers doivent être inférieurs à 260 caractères.
| `DirectoryNotFoundException` | Le répertoire spécifié est introuvable. |
| `Exception non prise en charge` | Les chemins d'accès ou les noms de fichiers source ou de destination sont dans un format non valide.

## "Touchez" une grande quantité de fichiers (pour mettre à jour la dernière heure d'écriture)
Cet exemple met à jour l'heure de la dernière écriture d'un grand nombre de fichiers (en utilisant `System.IO.Directory.EnumerateFiles` au lieu de `System.IO.Directory.GetFiles()`). Vous pouvez éventuellement spécifier un modèle de recherche (la valeur par défaut est `"*.*"` et éventuellement rechercher dans une arborescence de répertoires (pas seulement dans le répertoire spécifié) :

    public static void Touch(string path,
                             string searchPattern = "*.*",
                             SearchOptions options = SearchOptions.None)
    {
        var now = DateTime.Now;
    
        foreach (var filePath in Directory.EnumerateFiles(path, searchPattern, options))
        {
            File.SetLastWriteTime(filePath, now);
        }
    }

