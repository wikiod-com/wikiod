---
title: "Colección Observable<T>"
slug: "coleccion-observablet"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Inicializar ObservableCollection<T>
`ObservableCollection` es una colección de tipo `T` como `List<T>`, lo que significa que contiene objetos de tipo `T`.

De la documentación leemos que:

> `ObservableCollection`representa una colección de datos dinámica que
> proporciona notificaciones cuando se agregan o eliminan elementos, o cuando el
> toda la lista se actualiza.

La diferencia clave con otras colecciones es que 'ObservableCollection' implementa las interfaces 'INotifyCollectionChanged' e 'INotifyPropertyChanged' e inmediatamente genera un evento de notificación cuando se agrega o elimina un nuevo objeto y cuando se borra la colección.

Esto es especialmente útil para conectar la interfaz de usuario y el backend de una aplicación sin tener que escribir código adicional porque cuando se agrega o elimina un objeto de una colección observable, la interfaz de usuario se actualiza automáticamente.

El primer paso para usarlo es incluir

    using System.Collections.ObjectModel

Puede crear una instancia vacía de una colección, por ejemplo, de tipo `cadena`

    ObservableCollection<string> collection = new ObservableCollection<string>();

o una instancia que está llena de datos

     ObservableCollection<string> collection = new ObservableCollection<string>()
     {
      "First_String", "Second_String"
     };

Recuerde que, como en todas las colecciones de IList, el índice comienza desde 0 ([IList<T>.Item Property][1]).


[1]: https://msdn.microsoft.com/en-us/library/ewthkb10(v=vs.110).aspx "IList&lt;T&gt;.Propiedad del elemento"

