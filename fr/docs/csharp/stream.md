---
title: "Flux"
slug: "flux"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Utilisation de flux
Un flux est un objet qui fournit un moyen de bas niveau pour transférer des données. Ils n'agissent pas eux-mêmes comme des conteneurs de données.

Les données que nous traitons sont sous la forme d'un tableau d'octets (`byte []`). Les fonctions de lecture et d'écriture sont toutes orientées octets, par ex. `WriteByte()`.

Il n'y a pas de fonctions pour traiter les entiers, les chaînes, etc. Cela rend le flux très polyvalent, mais moins simple à utiliser si, par exemple, vous souhaitez simplement transférer du texte. Les flux peuvent être particulièrement utiles lorsque vous traitez une grande quantité de données.

Nous devrons utiliser différents types de flux en fonction de l'endroit où il doit être écrit/lu (c'est-à-dire le magasin de sauvegarde). Par exemple, si la source est un fichier, nous devons utiliser `FileStream` :

    string filePath = @"c:\Users\exampleuser\Documents\userinputlog.txt";
    using (FileStream fs = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
    {
        // do stuff here...
    
        fs.Close();
    }

De même, `MemoryStream` est utilisé si le magasin de sauvegarde est la mémoire :

    // Read all bytes in from a file on the disk.
    byte[] file = File.ReadAllBytes(“C:\\file.txt”);

    // Create a memory stream from those bytes.
    using (MemoryStream memory = new MemoryStream(file))
    {
       // do stuff here...
    }

De même, `System.Net.Sockets.NetworkStream` est utilisé pour l'accès au réseau.

Tous les flux sont dérivés de la classe générique "System.IO.Stream". Les données ne peuvent pas être directement lues ou écrites à partir de flux. Le .NET Framework fournit des classes d'assistance telles que `StreamReader`, `StreamWriter`, `BinaryReader` et `BinaryWriter` qui convertissent entre les types natifs et l'interface de flux de bas niveau, et transfèrent les données vers ou depuis le flux pour vous.

La lecture et l'écriture dans les flux peuvent être effectuées via `StreamReader` et `StreamWriter`. Il faut être prudent lors de la fermeture de ceux-ci. Par défaut, la fermeture fermera également le flux contenu et le rendra inutilisable pour d'autres utilisations. Ce comportement par défaut peut être modifié en utilisant un [constructeur][1] qui a le paramètre `bool leaveOpen` et en définissant sa valeur sur `true`.


`StreamWriter` :

    FileStream fs = new FileStream("sample.txt", FileMode.Create);
    StreamWriter sw = new StreamWriter(fs);
    string NextLine = "This is the appended line.";
    sw.Write(NextLine);
    sw.Close();
    //fs.Close(); There is no need to close fs. Closing sw will also close the stream it contains.

`StreamReader` :

    using (var ms = new MemoryStream())
    {
        StreamWriter sw = new StreamWriter(ms);
        sw.Write(123);
        //sw.Close();     This will close ms and when we try to use ms later it will cause an exception
        sw.Flush();     //You can send the remaining data to stream. Closing will do this automatically
        // We need to set the position to 0 in order to read 
        // from the beginning.
        ms.Position = 0;
        StreamReader sr = new StreamReader(ms);
        var myStr = sr.ReadToEnd();
        sr.Close();
        ms.Close();
    }

Puisque les classes `Stream`, `StreamReader`, `StreamWriter`, etc. implémentent l'interface `IDisposable`, nous pouvons appeler la méthode `Dispose()` sur les objets de ces classes.


[1] : https://msdn.microsoft.com/en-us/library/gg712952(v=vs.110).aspx

