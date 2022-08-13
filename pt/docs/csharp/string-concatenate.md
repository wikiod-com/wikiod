---
title: "Concatenar String"
slug: "concatenar-string"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Se você está criando uma string dinâmica, é uma boa prática optar pela classe `StringBuilder` ao invés de juntar strings usando o método + ou `Concat` já que cada +/`Concat` cria um novo objeto string toda vez que é executado.

## + Operador
    string s1 = "string1";
    string s2 = "string2";
    
    string s3 = s1 + s2; // "string1string2"

## Concatenar strings usando System.Text.StringBuilder
A concatenação de strings usando um [StringBuilder][1] pode oferecer vantagens de desempenho sobre a concatenação de strings simples usando `+`. Isso se deve ao modo como a memória é alocada. Strings são realocadas a cada concatenação, StringBuilders alocam memória em blocos apenas realocando quando o bloco atual se esgota. Isso pode fazer uma enorme diferença ao fazer muitas pequenas concatenações.

    StringBuilder sb = new StringBuilder();
    for (int i = 1; i <= 5; i++)
    {
        sb.Append(i);
        sb.Append(" ");
    }
    Console.WriteLine(sb.ToString()); // "1 2 3 4 5 "

Chamadas para `Append()` podem ser encadeadas, porque retorna uma referência ao `StringBuilder`:

    StringBuilder sb = new StringBuilder();
    sb.Append("some string ")
      .Append("another string");


[1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

## Concatenar elementos de array de string usando String.Join
O método `String.Join` pode ser usado para concatenar vários elementos de um array de strings.

    string[] value = {"apple", "orange", "grape", "pear"};
    string separator = ", ";

    string result = String.Join(separator, value, 1, 2);
    Console.WriteLine(result);

> Produz a seguinte saída: "laranja, uva"

Este exemplo usa a sobrecarga `String.Join(String, String[], Int32, Int32)`, que especifica o índice inicial e a contagem sobre o separador e o valor.

Se você não deseja usar o startIndex e as sobrecargas de contagem, você pode unir todas as strings fornecidas. Assim:
    
    string[] value = {"apple", "orange", "grape", "pear"};
    string separator = ", ";
    string result = String.Join(separator, value);
    Console.WriteLine(result);

que irá produzir;

> maçã, laranja, uva, pêra

## Concatenação de duas strings usando $
$ fornece um método fácil e conciso para concatenar várias strings.

    var str1 = "text1";
    var str2 = " ";
    var str3 = "text3";
    string result2 = $"{str1}{str2}{str3}"; //"text1 text3"

