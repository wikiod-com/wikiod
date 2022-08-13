---
title: "Coleção Observable<T>"
slug: "colecao-observablet"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Inicialize ObservableCollection<T>
`ObservableCollection` é uma coleção do tipo `T` como `List<T>` o que significa que contém objetos do tipo `T`.

Da documentação, lemos que:

> `ObservableCollection` representa uma coleção de dados dinâmica que
> fornece notificações quando os itens são adicionados, removidos ou quando o
> toda a lista é atualizada.

A principal diferença de outras coleções é que `ObservableCollection` implementa as interfaces `INotifyCollectionChanged` e `INotifyPropertyChanged` e imediatamente gera um evento de notificação quando um novo objeto é adicionado ou removido e quando a coleção é limpa.

Isso é especialmente útil para conectar a interface do usuário e o back-end de um aplicativo sem ter que escrever código extra porque quando um objeto é adicionado ou removido de uma coleção observável, a interface do usuário é atualizada automaticamente.

O primeiro passo para usá-lo é incluir

    using System.Collections.ObjectModel

Você pode criar uma instância vazia de uma coleção, por exemplo, do tipo `string`

    ObservableCollection<string> collection = new ObservableCollection<string>();

ou uma instância que é preenchida com dados

     ObservableCollection<string> collection = new ObservableCollection<string>()
     {
      "First_String", "Second_String"
     };

Lembre-se, como em toda coleção IList, o índice começa em 0 ([IList<T>.Item Property][1]).


[1]: https://msdn.microsoft.com/en-us/library/ewthkb10(v=vs.110).aspx "IList&lt;T&gt;.Item Property"

