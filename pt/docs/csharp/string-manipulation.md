---
title: "Manipulação de String"
slug: "manipulacao-de-string"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Substituindo uma string dentro de uma string
Usando o método [`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx), você pode substituir parte de uma string por outra corda.

    string s = "Hello World";
    s = s.Replace("World", "Universe"); // s = "Hello Universe"

Todas as ocorrências da string de pesquisa são substituídas:

    string s = "Hello World";
    s = s.Replace("l", "L"); // s = "HeLLo WorLD"

`String.Replace` também pode ser usado para *remover* parte de uma string, especificando uma string vazia como valor de substituição:

    string s = "Hello World";
    s = s.Replace("ell", String.Empty); // s = "Ho World"

## Mudando o caso de caracteres dentro de uma String
A classe [`System.String`](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) oferece suporte a vários métodos para converter entre maiúsculas e minúsculas caracteres em uma string.

- [`System.String.ToLowerInvariant`](https://msdn.microsoft.com/en-us/library/system.string.tolowerinvariant(v=vs.110).aspx) é usado para retornar um objeto String convertido para minúsculas.


- [`System.String.ToUpperInvariant`](https://msdn.microsoft.com/en-us/library/system.string.toupperinvariant(v=vs.110).aspx) é usado para retornar um objeto String convertido para maiúsculas.

**Observação:** o motivo para usar as versões *invariantes* desses métodos é evitar a produção de letras inesperadas específicas da cultura. Isso é explicado [aqui em detalhes](http://stackoverflow.com/a/19778131/1379664).

Exemplo:

    string s = "My String";
    s = s.ToLowerInvariant(); // "my string"
    s = s.ToUpperInvariant(); // "MY STRING"


Observe que você *pode* escolher especificar uma **[Cultura](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo(v=vs.110).aspx)** específica ao converter para letras minúsculas e maiúsculas usando [String.ToLower(CultureInfo)](https://msdn.microsoft.com/en-us/library/s8z5yt00(v=vs.110).aspx) e [String.ToUpper (CultureInfo)](https://msdn.microsoft.com/en-us/library/24kc78ka(v=vs.110).aspx) métodos de acordo.



## Encontrando uma string dentro de uma string
Usando o
[`System.String.Contains`][1] você pode descobrir se uma determinada string existe dentro de uma string. O método retorna um booleano, true se a string existir, senão false.

    string s = "Hello World";
    bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 

Usando o método [`System.String.IndexOf`](https://msdn.microsoft.com/en-us/library/k8b1470s(v=vs.110).aspx), você pode localizar a posição inicial de uma substring dentro de uma string existente.
Observe que a posição retornada é baseada em zero, um valor de -1 é retornado se a substring não for encontrada.

    string s = "Hello World";
    int location = s.IndexOf("ello"); // location = 1

Para encontrar o primeiro local do ***end*** de uma string, use o [`System.String.LastIndexOf `](https://msdn.microsoft.com/en-us/library/system.string. método lastindexof(v=vs.110).aspx):

    string s = "Hello World";
    int location = s.LastIndexOf("l"); // location = 9


[1]: https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx

## Removendo (aparando) espaços em branco de uma string
O método [`System.String.Trim`](https://msdn.microsoft.com/en-us/library/t97s7bs3(v=vs.110).aspx) pode ser usado para remover caracteres de espaço de uma string:

    string s = "     String with spaces at both ends      ";
    s = s.Trim(); // s = "String with spaces at both ends"

Além disso:
- Para remover espaços em branco apenas do *início* de uma string, use: [`System.String.TrimStart`](https://msdn.microsoft.com/en-us/library/system.string.trimstart(v =vs.110).aspx)

- Para remover o espaço em branco apenas do *final* de uma string, use: [`System.String.TrimEnd`](https://msdn.microsoft.com/en-us/library/system.string.trimend(v =vs.110).aspx)

**Substring para extrair parte de uma string.**

O método [`System.String.Substring`][1] pode ser usado para extrair uma parte da string.

    string s ="A portion of word that is retained";
    s=str.Substring(26);  //s="retained"

    s1 = s.Substring(0,5);  //s="A por"
    

[1]: https://msdn.microsoft.com/en-us/library/hxthx5h6(v=vs.110).aspx

## Dividindo uma string usando um delimitador
Use o método [`System.String.Split`](https://msdn.microsoft.com/en-us/library/system.string.split(v=vs.110).aspx) para retornar uma matriz de string que contém substrings da string original, divididas com base em um delimitador especificado:

    string sentence = "One Two Three Four";
    string[] stringArray = sentence.Split(' ');

    foreach (string word in stringArray)
    {
        Console.WriteLine(word);    
    }

Resultado:

> Um
> Dois
> Três
> Quatro

## Concatenar um array de strings em uma única string
O método [`System.String.Join`](https://msdn.microsoft.com/en-us/library/57a79xd0(v=vs.110).aspx) permite concatenar todos os elementos em um array de strings, usando um separador especificado entre cada elemento:

    string[] words = {"One", "Two", "Three", "Four"};
    string singleString = String.Join(",", words); // singleString = "One,Two,Three,Four"


## Concatenação de Strings
A concatenação de strings pode ser feita usando o método [`System.String.Concat`](https://msdn.microsoft.com/en-us/library/system.string.concat(v=vs.110).aspx) , ou (muito mais fácil) usando o operador `+`:

    string first = "Hello ";
    string second = "World";

    string concat = first + second; // concat = "Hello World"
    concat = String.Concat(first, second); // concat = "Hello World"

