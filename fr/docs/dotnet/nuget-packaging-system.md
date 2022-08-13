---
title: "Système de conditionnement NuGet"
slug: "systeme-de-conditionnement-nuget"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

[NuGet.org](https://www.nuget.org/) :

> NuGet est le gestionnaire de packages de la plate-forme de développement Microsoft incluant .NET. Les outils clients NuGet permettent de produire et de consommer des packages. La galerie NuGet est le référentiel de packages central utilisé par tous les auteurs et consommateurs de packages.

Images dans des exemples avec l'aimable autorisation de [NuGet.org](https://www.nuget.org/).

## Désinstallation d'un package d'un projet dans une solution
    PM> Uninstall-Package -ProjectName MyProjectB EntityFramework

## Installer une version spécifique d'un package
    PM> Install-Package EntityFramework -Version 6.1.2  


## Installation du gestionnaire de packages NuGet
Pour pouvoir gérer les packages de vos projets, vous avez besoin du gestionnaire de packages NuGet. Il s'agit d'une extension Visual Studio, expliquée dans la documentation officielle : [Installation et mise à jour du client NuGet](https://docs.nuget.org/consume/installing-nuget).

À partir de Visual Studio 2012, NuGet est inclus dans chaque édition et peut être utilisé à partir de : Outils -> Gestionnaire de packages NuGet -> Console du gestionnaire de packages.

Pour ce faire, accédez au menu Outils de Visual Studio, en cliquant sur Extensions et mises à jour :

[![entrez la description de l'image ici][1]][1]

[1] : http://i.stack.imgur.com/zTzgp.png

Cela installe à la fois l'interface graphique :

* Disponible en cliquant sur "Gérer les packages NuGet..." sur un projet ou son dossier de références

Et la console du gestionnaire de packages :

* Outils -> Gestionnaire de packages NuGet -> Console du gestionnaire de packages.

## Ajout d'un flux source de package (MyGet, Klondike, ect)
    nuget sources add -name feedname -source http://sourcefeedurl

## Gestion des packages via l'interface utilisateur
Lorsque vous cliquez avec le bouton droit sur un projet (ou son dossier Références), vous pouvez cliquer sur l'option "Gérer les packages NuGet...". Cela montre le [Package Manager Dialog] (https://docs.nuget.org/consume/package-manager-dialog).

[![entrez la description de l'image ici][1]][1]

[1] : http://i.stack.imgur.com/Fi0Uq.png

## Gestion des packages via la console
Cliquez sur les menus Outils -> NuGet Package Manager -> Package Manager Console pour afficher la console dans votre IDE. [Documentation officielle ici](https://docs.nuget.org/consume/package-manager-console-powershell-reference).

Ici, vous pouvez émettre, entre autres, des commandes `install-package` qui installe le package saisi dans le "projet par défaut" actuellement sélectionné :

    Install-Package Elmah

Vous pouvez également fournir le projet sur lequel installer le package, en remplaçant le projet sélectionné dans la liste déroulante "Projet par défaut" :

    Install-Package Elmah -ProjectName MyFirstWebsite

## Mettre à jour un paquet
Pour mettre à jour un package, utilisez la commande suivante :

    PM> Update-Package EntityFramework
où EntityFramework est le nom du package à mettre à jour. Notez que la mise à jour s'exécutera pour tous les projets et qu'elle est donc différente de `Install-Package EntityFramework` qui s'installerait uniquement sur le "Projet par défaut".

Vous pouvez également spécifier explicitement un seul projet :

    PM> Update-Package EntityFramework -ProjectName MyFirstWebsite



## Désinstaller un paquet
    PM> Uninstall-Package EntityFramework  

## désinstaller une version spécifique du package
    
    PM> uninstall-Package EntityFramework -Version 6.1.2

## Utilisation de différentes sources de packages Nuget (locales) à l'aide de l'interface utilisateur
Il est courant pour une entreprise de configurer son propre serveur nuget pour la distribution de packages entre différentes équipes.

1. Accédez à l'explorateur de solutions et cliquez sur le bouton <kbd>droit de la souris</kbd>, puis choisissez "Gérer les packages NuGet pour la solution".

[![entrez la description de l'image ici][1]][1]

2. Dans la fenêtre qui s'ouvre, cliquez sur "Paramètres"

[![entrez la description de l'image ici][2]][2]

3. Cliquez sur `+` dans le coin supérieur droit, puis ajoutez un nom et une URL pointant vers votre serveur nuget local.

[![entrez la description de l'image ici][3]][3]


[1] : http://i.stack.imgur.com/PhB3d.png
[2] : http://i.stack.imgur.com/8vKM6.png
[3] : http://i.stack.imgur.com/h85QG.png

