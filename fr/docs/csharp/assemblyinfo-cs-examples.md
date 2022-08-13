---
title: "AssemblyInfo.cs Exemples"
slug: "assemblyinfocs-exemples"
draft: false
images: []
weight: 9764
type: docs
toc: true
---

Le nom de fichier "AssemblyInfo.cs" est utilisé par convention comme fichier source où les développeurs placent des attributs de métadonnées qui décrivent l'assemblage complet qu'ils construisent.

## AssemblyInfo global et local


## [Version d'assemblage]
Cet attribut applique une version à l'assembly.

    [assembly: AssemblyVersion("1.0.*")]

Le caractère `*` est utilisé pour auto-incrémenter automatiquement une partie de la version à chaque fois que vous compilez (souvent utilisé pour le numéro de "build")

## [Titre de l'assemblage]
Cet attribut est utilisé pour donner un nom à cet assemblage particulier.

    [assembly: AssemblyTitle("MyProduct")]



## [Produit d'assemblage]
Cet attribut est utilisé pour décrire le produit auquel cet assemblage particulier est destiné. Plusieurs assemblages peuvent être des composants du même produit, auquel cas ils peuvent tous partager la même valeur pour cet attribut.

    [assembly: AssemblyProduct("MyProduct")]


## Versionnement automatisé


## Champs communs


## [InternesVisiblesÀ]
Si vous souhaitez rendre les classes ou les fonctions "internes" d'un assembly accessibles à partir d'un autre assembly, vous le déclarez par "InternalsVisibleTo" et le nom de l'assembly auquel l'accès est autorisé.


Dans cet exemple, le code de l'assembly `MyAssembly.UnitTests` est autorisé à appeler des éléments `internal` à partir de `MyAssembly`.

    [assembly: InternalsVisibleTo("MyAssembly.UnitTests")]

Ceci est particulièrement utile pour les tests unitaires afin d'éviter les déclarations "publiques" inutiles.

## Lecture des attributs d'assemblage
À l'aide des API de réflexion enrichies de .NET, vous pouvez accéder aux métadonnées d'un assembly. Par exemple, vous pouvez obtenir l'attribut title de cet assembly avec le code suivant

    using System.Linq;
    using System.Reflection;
    
    ...
    
    Assembly assembly = typeof(this).Assembly;
    var titleAttribute = assembly.GetCustomAttributes<AssemblyTitleAttribute>().FirstOrDefault();
    
    Console.WriteLine($"This assembly title is {titleAttribute?.Title}");


## [Configuration de l'assemblage]
AssemblyConfiguration : l'attribut AssemblyConfiguration doit avoir la configuration qui a été utilisée pour générer l'assembly.
Utilisez la compilation conditionnelle pour inclure correctement différentes configurations d'assembly.
Utilisez le bloc similaire à l'exemple ci-dessous. Ajoutez autant de configurations différentes que vous en utilisez couramment.


    #if (DEBUG)
    
    [assembly: AssemblyConfiguration("Debug")]

    #else

    [assembly: AssemblyConfiguration("Release")]
    
    #endif


## [AssemblyKeyFile]
Chaque fois que nous voulons que notre assemblage s'installe dans GAC, il doit avoir un nom fort. Pour un assemblage de nommage fort, nous devons créer une clé publique.
Pour générer le fichier `.snk`.

Pour créer un fichier de clé de nom fort

> 1. Invite de commande des développeurs pour VS2015 (avec accès administrateur)
> 2. À l'invite de commande, saisissez cd C:\Nom_Répertoire et appuyez sur ENTREE.
> 3. À l'invite de commande, tapez sn -k KeyFileName.snk, puis appuyez sur ENTRÉE.

une fois que le keyFileName.snk est créé dans le répertoire spécifié, donnez une référence dans votre projet. donnez à l'attribut `AssemblyKeyFileAttribute` le chemin d'accès au fichier `snk` pour générer la clé lorsque nous construisons notre bibliothèque de classes.
    
> Propriétés -> AssemblyInfo.cs
    
    [assembly: AssemblyKeyFile(@"c:\Directory_Name\KeyFileName.snk")]

Cela créera un assembly de nom fort après la construction. Après avoir créé votre assemblage de nom fort, vous pouvez ensuite l'installer dans GAC

Bon codage :)

