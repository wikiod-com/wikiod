---
title: "ObservableCollection<T>"
slug: "observablecollectiont"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Initialiser ObservableCollection<T>
`ObservableCollection` est une collection de type `T` comme `List<T>` ce qui signifie qu'elle contient des objets de type `T`.

De la documentation, nous lisons que:

> `ObservableCollection`représente une collection de données dynamique qui
> fournit des notifications lorsque des éléments sont ajoutés, supprimés ou lorsque le
> toute la liste est actualisée.

La principale différence avec les autres collections est que `ObservableCollection` implémente les interfaces `INotifyCollectionChanged` et `INotifyPropertyChanged` et déclenche immédiatement un événement de notification lorsqu'un nouvel objet est ajouté ou supprimé et lorsque la collection est effacée.

Ceci est particulièrement utile pour connecter l'interface utilisateur et le backend d'une application sans avoir à écrire de code supplémentaire, car lorsqu'un objet est ajouté ou supprimé d'une collection observable, l'interface utilisateur est automatiquement mise à jour.

La première étape pour l'utiliser est d'inclure

    using System.Collections.ObjectModel

Vous pouvez soit créer une instance vide d'une collection par exemple de type `string`

    ObservableCollection<string> collection = new ObservableCollection<string>();

ou une instance remplie de données

     ObservableCollection<string> collection = new ObservableCollection<string>()
     {
      "First_String", "Second_String"
     };

N'oubliez pas que dans toutes les collections IList, l'index commence à 0 ([IList<T>.Item Property][1]).


[1] : https://msdn.microsoft.com/en-us/library/ewthkb10(v=vs.110).aspx "IList&lt;T&gt;.Item Property"

