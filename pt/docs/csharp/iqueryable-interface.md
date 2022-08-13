---
title: "Interface que pode ser consultada"
slug: "interface-que-pode-ser-consultada"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Traduzindo uma consulta LINQ para uma consulta SQL
As interfaces `IQueryable` e `IQueryable<T>` permitem que os desenvolvedores traduzam uma consulta LINQ (uma consulta 'integrada à linguagem') para uma fonte de dados específica, por exemplo, um banco de dados relacional. Pegue esta consulta LINQ escrita em C#:

    var query = from book in books
                where book.Author == "Stephen King" 
                select book;

Se a variável `books` for de um tipo que implementa `IQueryable<Book>`, a consulta acima será passada para o provedor (definido na propriedade `IQueryable.Provider`) na forma de uma árvore de expressão, uma estrutura de dados que reflete a estrutura do código.

O provedor pode inspecionar a árvore de expressão em tempo de execução para determinar:

- que existe um predicado para a propriedade `Author` da classe `Book`;
- que o método de comparação usado é 'igual' (`==`);
- que o valor que deve ser igual é `"Stephen King"`.

Com essas informações, o provedor pode traduzir a consulta C# para uma consulta SQL em tempo de execução e passar essa consulta para um banco de dados relacional para buscar apenas os livros que correspondem ao predicado:

    select *
    from Books
    where Author = 'Stephen King'

O provedor é chamado quando a variável `query` é iterada (`IQueryable` implementa `IEnumerable`).

(O provedor usado neste exemplo exigiria alguns metadados extras para saber qual tabela consultar e saber como corresponder as propriedades da classe C# às colunas da tabela, mas esses metadados estão fora do escopo da interface `IQueryable`. )

