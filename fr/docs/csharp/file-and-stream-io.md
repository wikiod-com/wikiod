---
title: "ES de fichiers et de flux"
slug: "es-de-fichiers-et-de-flux"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

Gère les fichiers.

## Syntaxe
- `nouveau System.IO.StreamWriter (chemin de chaîne)`
- `nouveau System.IO.StreamWriter (chemin de chaîne, ajout booléen)`
- `System.IO.StreamWriter.WriteLine (chaîne de texte)`
- `System.IO.StreamWriter.WriteAsync (chaîne de texte)`
- `System.IO.Stream.Close()`
- `System.IO.File.ReadAllText (chemin de la chaîne)`
- `System.IO.File.ReadAllLines (chemin de la chaîne)`
- `System.IO.File.ReadLines (chemin de la chaîne)`
- `System.IO.File.WriteAllText (chemin de chaîne, texte de chaîne)`
- `System.IO.File.WriteAllLines(chemin de la chaîne, contenu IEnumerable<chaîne>)`
- `System.IO.File.Copy(chaîne source, chaîne destination)`
- `System.IO.File.Create (chemin de chaîne)`
- `System.IO.File.Delete (chemin de la chaîne)`
- `System.IO.File.Move(chaîne source, chaîne destination)`
- `System.IO.Directory.GetFiles (chemin de la chaîne)`

## Paramètres
| Paramètre | Détails |
| ------ | ------ |
| chemin | L'emplacement du fichier. |
| ajouter | Si le fichier existe, true ajoutera des données à la fin du fichier (append), false écrasera le fichier. |
| texte | Texte à écrire ou à stocker. |
|contenu | Une collection de chaînes à écrire. |
| sources | L'emplacement du fichier que vous souhaitez utiliser. |
| destination | L'emplacement où vous voulez qu'un fichier aille. |

- Assurez-vous toujours de fermer les objets `Stream`. Cela peut être fait avec un bloc `using` comme indiqué ci-dessus ou en appelant manuellement `myStream.Close()`.
- Assurez-vous que l'utilisateur actuel dispose des autorisations nécessaires sur le chemin que vous essayez de créer le fichier.
- [chaînes Verbatim](https://www.wikiod.com/fr/docs/c%23/16/verbatim-strings) doivent être utilisées lors de la déclaration d'une chaîne de chemin qui inclut des barres obliques inverses, comme ceci : `@"C:\MyFolder\MyFile .txt"`

## Lecture d'un fichier à l'aide de la classe System.IO.File
Vous pouvez utiliser le **[System.IO.File.ReadAllText](https://msdn.microsoft.com/en-us/library/system.io.file.readalltext(v=vs.110).aspx)* * fonction pour lire tout le contenu d'un fichier dans une chaîne.

    string text = System.IO.File.ReadAllText(@"C:\MyFolder\MyTextFile.txt");

Vous pouvez également lire un fichier sous forme de tableau de lignes à l'aide de **[System.IO.File.ReadAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.readlines(v =vs.110).aspx)** fonction :
    
    string[] lines = System.IO.File.ReadAllLines(@"C:\MyFolder\MyTextFile.txt");

## Lecture paresseuse d'un fichier ligne par ligne via un IEnumerable
Lorsque vous travaillez avec des fichiers volumineux, vous pouvez utiliser la méthode `System.IO.File.ReadLines` pour lire toutes les lignes d'un fichier dans un `IEnumerable<string>`. Ceci est similaire à `System.IO.File.ReadAllLines`, sauf qu'il ne charge pas le fichier entier en mémoire à la fois, ce qui le rend plus efficace lorsque vous travaillez avec des fichiers volumineux.

    IEnumerable<string> AllLines = File.ReadLines("file_name.txt", Encoding.Default);

_Le deuxième paramètre de File.ReadLines est facultatif. Vous pouvez l'utiliser lorsqu'il est nécessaire de spécifier l'encodage._

Il est important de noter que l'appel de `ToArray`, `ToList` ou une autre fonction similaire forcera toutes les lignes à être chargées en même temps, ce qui signifie que l'avantage d'utiliser `ReadLines` est annulé. Il est préférable d'énumérer sur le `IEnumerable` en utilisant une boucle `foreach` ou LINQ si vous utilisez cette méthode.


## Écrire des lignes dans un fichier à l'aide de la classe System.IO.StreamWriter
La classe **[System.IO.StreamWriter](https://msdn.microsoft.com/en-us/library/system.io.streamwriter(v=vs.110).aspx)** :
> Implémente un TextWriter pour écrire des caractères dans un flux dans un encodage particulier.

En utilisant la méthode `WriteLine`, vous pouvez écrire du contenu ligne par ligne dans un fichier.

Notez l'utilisation du mot clé `using` qui garantit que l'objet StreamWriter est supprimé dès qu'il sort de la portée et que le fichier est donc fermé.

    string[] lines = { "My first string", "My second string", "and even a third string" };
    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt"))
    {
        foreach (string line in lines)
        {
            sw.WriteLine(line);
        }
    }

Notez que le StreamWriter peut recevoir un deuxième paramètre `bool` dans son constructeur, permettant de `Append` à un fichier au lieu d'écraser le fichier :

    bool appendExistingFile = true;
    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt", appendExistingFile ))
    {
        sw.WriteLine("This line will be appended to the existing file");
    }

## Écrire dans un fichier à l'aide de la classe System.IO.File
Vous pouvez utiliser le **[System.IO.File.WriteAllText](https://msdn.microsoft.com/en-us/library/system.io.file.writealltext(v=vs.110).aspx)* * fonction pour écrire une chaîne dans un fichier.

    string text = "String that will be stored in the file";
    System.IO.File.WriteAllText(@"C:\MyFolder\OutputFile.txt", text);

Vous pouvez également utiliser **[System.IO.File.WriteAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.writealllines(v=vs.110).aspx) ** fonction qui reçoit un `IEnumerable<String>` comme deuxième paramètre (par opposition à une seule chaîne dans l'exemple précédent). Cela vous permet d'écrire du contenu à partir d'un tableau de lignes.

    string[] lines = { "My first string", "My second string", "and even a third string" };
    System.IO.File.WriteAllLines(@"C:\MyFolder\OutputFile.txt", lines);

## Copier un fichier
**Classe statique de fichier**

La classe statique `File` peut être facilement utilisée à cette fin.

    File.Copy(@"sourcePath\abc.txt", @"destinationPath\abc.txt");
    File.Copy(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

**Remarque :** Par cette méthode, le fichier est copié, ce qui signifie qu'il sera lu à partir de la source, puis écrit dans le chemin de destination. Il s'agit d'un processus consommateur de ressources, cela prendrait du temps par rapport à la taille du fichier et peut entraîner le blocage de votre programme si vous n'utilisez pas de threads.

## Écrire du texte asynchrone dans un fichier à l'aide de StreamWriter


## Créer un fichier
**Classe statique de fichier**

En utilisant la méthode `Create` de la classe statique `File`, nous pouvons créer des fichiers. La méthode crée le fichier au chemin donné, en même temps elle ouvre le fichier et nous donne le `FileStream` du fichier. Assurez-vous de fermer le fichier une fois que vous en avez terminé.

ex1 :

    var fileStream1 = File.Create("samplePath");
    /// you can write to the fileStream1
    fileStream1.Close();

ex2 :

    using(var fileStream1 = File.Create("samplePath"))
    {
        /// you can write to the fileStream1
    }

ex3 :

    File.Create("samplePath").Close();

**Classe FileStream**

Il existe de nombreuses surcharges de ce constructeur de classes qui est en fait bien documenté [ici][1]. L'exemple ci-dessous est pour celui qui couvre les fonctionnalités les plus utilisées de cette classe.

    var fileStream2 = new FileStream("samplePath", FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None);

Vous pouvez vérifier les énumérations pour [FileMode][2], [FileAccess][3] et [FileShare][4] à partir de ces liens. Ce qu'ils signifient essentiellement sont les suivants:

*FileMode :* Réponses "Le fichier doit-il être créé ? Ouvert ? Créer s'il n'existe pas, puis l'ouvrir ?" un peu des questions.

*FileAccess :* Réponses "Dois-je être capable de lire le fichier, d'écrire dans le fichier ou les deux ?" un peu des questions.

*FileShare :* Réponses "Les autres utilisateurs devraient-ils pouvoir lire, écrire, etc. dans le fichier pendant que je l'utilise simultanément ?" un peu des questions.


[1] : https://msdn.microsoft.com/en-us/library/system.io.filestream(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.io.filemode(v=vs.110).aspx
[3] : https://msdn.microsoft.com/en-us/library/4z36sx0f(v=vs.110).aspx
[4] : https://msdn.microsoft.com/en-us/library/system.io.fileshare(v=vs.110).aspx

## Déplacer le fichier
**Classe statique de fichier**

La classe statique de fichier peut facilement être utilisée à cette fin.

    File.Move(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

**Remarque1 :** Change uniquement l'index du fichier (si le fichier est déplacé dans le même volume). Cette opération ne prend pas de temps relatif à la taille du fichier.

**Remarque2 :** Impossible de remplacer un fichier existant sur le chemin de destination.

## Supprimer le fichier
    string path = @"c:\path\to\file.txt";
    File.Delete(path);

Bien que `Delete` ne lève pas d'exception si le fichier n'existe pas, il lèvera une exception, par exemple. si le chemin spécifié n'est pas valide ou si l'appelant ne dispose pas des autorisations requises. Vous devez toujours envelopper les appels à `Delete` dans [try-catch block][1] et gérer toutes les exceptions attendues. En cas de conditions de concurrence possibles, encapsulez la logique dans [instruction de verrouillage] [2].


[1] : https://www.wikiod.com/fr/docs/c%23/26/keywords/148/try-catch-finally-throw#t=201608021340222162938
[2] : https://www.wikiod.com/fr/docs/c%23/1495/lock-statement/4865/simple-usage#t=201608021343504970522

## Fichiers et répertoires
**Obtenir tous les fichiers du répertoire**
    
     var FileSearchRes = Directory.GetFiles(@Path, "*.*", SearchOption.AllDirectories);

Renvoie un tableau de `FileInfo`, représentant tous les fichiers du répertoire spécifié.

**Obtenir des fichiers avec une extension spécifique**

     var FileSearchRes = Directory.GetFiles(@Path, "*.pdf", SearchOption.AllDirectories);

Renvoie un tableau de `FileInfo`, représentant tous les fichiers du répertoire spécifié avec l'extension spécifiée.

