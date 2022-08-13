---
title: "Operações Comuns de String"
slug: "operacoes-comuns-de-string"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Formatando uma string
Use o método `String.Format()` para substituir um ou mais itens na string pela representação em string de um objeto especificado:

    String.Format("Hello {0} Foo {1}", "World", "Bar") //Hello World Foo Bar

## Preenchendo uma string com um comprimento fixo
    string s = "Foo";
    string paddedLeft = s.PadLeft(5);        // paddedLeft = "  Foo" (pads with spaces by default)
    string paddedRight = s.PadRight(6, '+'); // paddedRight = "Foo+++"
    string noPadded = s.PadLeft(2);          // noPadded = "Foo" (original string is never shortened)


## Invertendo corretamente uma string
Na maioria das vezes, quando as pessoas precisam inverter uma string, elas fazem mais ou menos assim:

    char[] a = s.ToCharArray();
    System.Array.Reverse(a);
    string r = new string(a);

No entanto, o que essas pessoas não percebem é que isso está realmente errado. <br />
E não quero dizer por causa da falta de verificação NULL.

Na verdade, está errado porque um Glyph/GraphemeCluster pode consistir em vários codepoints (também conhecidos como caracteres).

Para ver por que isso acontece, primeiro temos que estar cientes do que o termo "personagem" realmente significa.

[Referência:][1]
> Caráter é um termo sobrecarregado que pode significar muitas coisas.
> 
> Um ponto de código é a unidade atômica de informação. O texto é uma sequência de
> pontos de código. Cada ponto de código é um número que recebe significado pelo
> Padrão Unicode.
> 
> Um grafema é uma sequência de um ou mais pontos de código que são exibidos
> como uma unidade gráfica única que um leitor reconhece como um único
> elemento do sistema de escrita. Por exemplo, tanto a quanto ä são
> grafemas, mas eles podem consistir em vários pontos de código (por exemplo, ä pode ser
> dois pontos de código, um para o caractere base a seguido por um para o
> diárese; mas também há um ponto de código alternativo, legado e único
> representando este grafema). Alguns pontos de código nunca fazem parte de nenhum
> grafema (por exemplo, o não-marceneiro de largura zero ou substituições direcionais).
> 
> Um glifo é uma imagem, geralmente armazenada em uma fonte (que é uma coleção
> de glifos), usados ​​para representar grafemas ou partes deles. As fontes podem
> compor vários glifos em uma única representação, por exemplo, se
> o ä acima é um único ponto de código, uma fonte pode optar por renderizá-lo como
> dois glifos separados espacialmente sobrepostos. Para OTF, o GSUB da fonte e
> As tabelas GPOS contêm informações de substituição e posicionamento para
> este trabalho. Uma fonte pode conter vários glifos alternativos para o mesmo
> grafema também.

Então, em C#, um caractere é na verdade um CodePoint.

O que significa que, se você apenas reverter uma string válida como 'Les Misérables', que pode ficar assim

    string s = "Les Mise\u0301rables";

como uma sequência de caracteres, você obterá:

> selbaŕesiM seL

Como você pode ver, o acento está no caractere R, em vez do caractere e. <br />
Embora string.reverse.reverse produza a string original se você inverter o array char ambas as vezes, esse tipo de reversão definitivamente NÃO é o inverso da string original.


Você precisará reverter apenas cada GraphemeCluster. <br />
Então, se feito corretamente, você inverte uma string como esta:


        private static System.Collections.Generic.List<string> GraphemeClusters(string s)
        {
            System.Collections.Generic.List<string> ls = new System.Collections.Generic.List<string>();
    
            System.Globalization.TextElementEnumerator enumerator = System.Globalization.StringInfo.GetTextElementEnumerator(s);
            while (enumerator.MoveNext())
            {
                ls.Add((string)enumerator.Current);
            }
    
            return ls;
        }
    
    
        // this 
        private static string ReverseGraphemeClusters(string s)
        {
            if(string.IsNullOrEmpty(s) || s.Length == 1)
                 return s;
            
            System.Collections.Generic.List<string> ls = GraphemeClusters(s);
            ls.Reverse();
    
            return string.Join("", ls.ToArray());
        }
    
        public static void TestMe()
        {
            string s = "Les Mise\u0301rables";
            // s = "noël";
            string r = ReverseGraphemeClusters(s);
    
            // This would be wrong:
            // char[] a = s.ToCharArray();
            // System.Array.Reverse(a);
            // string r = new string(a);
    
            System.Console.WriteLine(r);
        }

E - oh alegria - você perceberá que se fizer isso corretamente assim, também funcionará para idiomas asiáticos/sul-asiáticos/leste-asiáticos (e francês/sueco/norueguês, etc.) ...


[1]: https://stackoverflow.com/questions/27331819/whats-the-difference-between-a-character-a-code-point-a-glyph-and-a-grapheme

## Obtendo x caracteres do lado direito de uma string
O Visual Basic tem funções Left, Right e Mid que retornam caracteres de Left, Right e Middle de uma string. Esses métodos não existem em C#, mas podem ser implementados com `Substring()`. Eles podem ser implementados como métodos de extensão como o seguinte:


       public static class StringExtensions
       {
          /// <summary>
          /// VB Left function
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="numchars"></param>
          /// <returns>Left-most numchars characters</returns>
          public static string Left( this string stringparam, int numchars )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (numchars > stringparam.Length)
                numchars = stringparam.Length;
        
             return stringparam.Substring( 0, numchars );
          }
        
          /// <summary>
          /// VB Right function
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="numchars"></param>
          /// <returns>Right-most numchars characters</returns>
          public static string Right( this string stringparam, int numchars )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (numchars > stringparam.Length)
                numchars = stringparam.Length;
        
             return stringparam.Substring( stringparam.Length - numchars );
          }
        
          /// <summary>
          /// VB Mid function - to end of string
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
          /// <returns>Balance of string beginning at startindex character</returns>
          public static string Mid( this string stringparam, int startindex )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative startindex being passed
             startindex = Math.Abs( startindex );
        
             // Validate numchars parameter        
             if (startindex > stringparam.Length)
                startindex = stringparam.Length;
             
             // C# strings are zero-based, convert passed startindex
             return stringparam.Substring( startindex - 1 );
          }
        
          /// <summary>
          /// VB Mid function - for number of characters
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
          /// <param name="numchars">number of characters to return</param>
          /// <returns>Balance of string beginning at startindex character</returns>
          public static string Mid( this string stringparam, int startindex, int numchars)
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative startindex being passed
             startindex = Math.Abs( startindex );
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (startindex > stringparam.Length)
                startindex = stringparam.Length;
        
             // C# strings are zero-based, convert passed startindex
             return stringparam.Substring( startindex - 1, numchars );
    
           }
        }
Este método de extensão pode ser usado da seguinte forma:

    string myLongString = "Hello World!";
    string myShortString = myLongString.Right(6);  // "World!"
    string myLeftString = myLongString.Left(5);    // "Hello"
    string myMidString1 = myLongString.Left(4);    // "lo World"
    string myMidString2 = myLongString.Left(2,3);    // "ell"








## Verificando a String vazia usando String.IsNullOrEmpty() e String.IsNullOrWhiteSpace()
    string nullString = null;
    string emptyString = "";
    string whitespaceString = "    ";
    string tabString = "\t";
    string newlineString = "\n";
    string nonEmptyString = "abc123";
    
    bool result;

    result = String.IsNullOrEmpty(nullString);            // true
    result = String.IsNullOrEmpty(emptyString);           // true
    result = String.IsNullOrEmpty(whitespaceString);      // false
    result = String.IsNullOrEmpty(tabString);             // false
    result = String.IsNullOrEmpty(newlineString);         // false
    result = String.IsNullOrEmpty(nonEmptyString);        // false

    result = String.IsNullOrWhiteSpace(nullString);       // true
    result = String.IsNullOrWhiteSpace(emptyString);      // true
    result = String.IsNullOrWhiteSpace(tabString);        // true
    result = String.IsNullOrWhiteSpace(newlineString);    // true
    result = String.IsNullOrWhiteSpace(whitespaceString); // true
    result = String.IsNullOrWhiteSpace(nonEmptyString);   // false

## Aparar caracteres indesejados no início e/ou no final das strings.
`String.Trim()`
--------

    string x = "   Hello World!    ";
    string y = x.Trim(); // "Hello World!"

    string q = "{(Hi!*";
    string r = q.Trim( '(', '*', '{' ); // "Hi!"


`String.TrimStart()` e `String.TrimEnd()`
--------------------------------------------

    string q = "{(Hi*";
    string r = q.TrimStart( '{' ); // "(Hi*"
    string s = q.TrimEnd( '*' );   // "{(Hi" 


## Construir uma string de Array
O método `String.Join` nos ajudará a construir uma string From array/list of characters ou string. Este método aceita dois parâmetros. O primeiro é o delimitador ou separador que o ajudará a separar cada elemento no array. E o segundo parâmetro é o próprio Array.

**String de `char array`:**

    string delimiter=",";
    char[] charArray = new[] { 'a', 'b', 'c' };
    string inputString = String.Join(delimiter, charArray);
**Saída** : `a,b,c` se alterarmos o `delimiter` como `""` então a saída se tornará `abc`.

**String de `Lista de caracteres`:**

    string delimiter = "|";
    List<char> charList = new List<char>() { 'a', 'b', 'c' };
    string inputString = String.Join(delimiter, charList);

**Saída**: `a|b|c`

**String da `Lista de Strings`:**

    string delimiter = " ";
    List<string> stringList = new List<string>() { "Ram", "is", "a","boy" };
    string inputString = String.Join(delimiter, stringList);

**Saída** : `Ram é um menino`

**String de `array of strings`:**

    string delimiter = "_";
    string[] stringArray = new [] { "Ram", "is", "a","boy" };
    string inputString = String.Join(delimiter, stringArray);

**Saída**: `Ram_is_a_boy`


## Formatação usando ToString
Normalmente estamos usando o método `String.Format` para fins de formatação, o `.ToString` é normalmente usado para converter outros tipos para string. Podemos especificar o formato junto com o método ToString enquanto a conversão está ocorrendo, para evitar uma formatação adicional. Deixe-me explicar como funciona com diferentes tipos;

**Inteiro para string formatada:**

    int intValue = 10;
    string zeroPaddedInteger = intValue.ToString("000"); // Output will be "010"
    string customFormat = intValue.ToString("Input value is 0"); // output will be   "Input value is 10" 
**duplo para string formatada:**

    double doubleValue = 10.456;
    string roundedDouble = doubleValue.ToString("0.00"); // output 10.46
    string integerPart = doubleValue.ToString("00");    // output 10
    string customFormat = doubleValue.ToString("Input value is 0.0");  // Input value is 10.5

**Formatando DateTime usando ToString**

    DateTime currentDate = DateTime.Now; //  {7/21/2016 7:23:15 PM}
    string dateTimeString = currentDate.ToString("dd-MM-yyyy HH:mm:ss"); // "21-07-2016 19:23:15"
    string dateOnlyString = currentDate.ToString("dd-MM-yyyy"); // "21-07-2016"
    string dateWithMonthInWords = currentDate.ToString("dd-MMMM-yyyy HH:mm:ss"); // "21-July-2016 19:23:15"




## Converter número decimal para formato binário, octal e hexadecimal
1. Para converter o número decimal para o formato binário, use **base 2**

        Int32 Number = 15;
        Console.WriteLine(Convert.ToString(Number, 2));  //OUTPUT : 1111

2. Para converter o número decimal para o formato octal, use **base 8**

        int Number = 15;
        Console.WriteLine(Convert.ToString(Number, 8));  //OUTPUT : 17

3. Para converter o número decimal para o formato hexadecimal, use **base 16**

        var Number = 15;
        Console.WriteLine(Convert.ToString(Number, 16));  //OUTPUT : f



## Dividindo uma String por caractere específico
    string helloWorld = "hello world, how is it going?";
    string[] parts1 = helloWorld.Split(',');

    //parts1: ["hello world", " how is it going?"]

    string[] parts2 = helloWorld.Split(' ');

    //parts2: ["hello", "world,", "how", "is", "it", "going?"]


## Obtendo Substrings de uma determinada string
    string helloWorld = "Hello World!";
    string world = helloWorld.Substring(6); //world = "World!"
    string hello = helloWorld.Substring(0,5); // hello = "Hello"

`Substring` retorna a string de um determinado índice, ou entre dois índices (ambos inclusive).

## Determina se uma string começa com uma determinada sequência
    string HelloWorld = "Hello World";
    HelloWorld.StartsWith("Hello"); // true
    HelloWorld.StartsWith("Foo"); // false


**Encontrando uma string dentro de uma string**

Usando o
[`System.String.Contains`][1] você pode descobrir se uma determinada string existe dentro de uma string. O método retorna um booleano, true se a string existir, senão false.

    string s = "Hello World";
    bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 


[1]: https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx

## Unindo um array de strings em um novo
    var parts = new[] { "Foo", "Bar", "Fizz", "Buzz"};
    var joined = string.Join(", ", parts);

    //joined = "Foo, Bar, Fizz, Buzz"

## Obtendo um char em um índice específico e enumerando a string
Você pode usar o método `Substring` para obter qualquer número de caracteres de uma string em qualquer local. No entanto, se você quiser apenas um único caractere, poderá usar o indexador de string para obter um único caractere em qualquer índice, como faz com um array:

    string s = "hello";
    char c = s[1]; //Returns 'e'

Observe que o tipo de retorno é `char`, diferente do método `Substring` que retorna um tipo `string`.

Você também pode usar o indexador para percorrer os caracteres da string:

    string s = "hello";
    foreach (char c in s)
        Console.WriteLine(c);
    /********* This will print each character on a new line:
    h
    e
    l
    l
    o
    **********/

## Dividindo uma String por outra string
    string str = "this--is--a--complete--sentence";
    string[] tokens = str.Split(new[] { "--" }, StringSplitOptions.None);

Resultado:

>[ "isto", "é", "um", "completo", "frase" ]

## Substituindo uma string dentro de uma string
Usando o método [`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx), você pode substituir parte de uma string por outra corda.

    string s = "Hello World";
     s = s.Replace("World", "Universe"); // s = "Hello Universe"
Todas as ocorrências da string de pesquisa são substituídas.

Este método também pode ser usado para remover parte de uma string, usando o [`String.Empty`](https://msdn.microsoft.com/en-us/library/system.string.empty(v=vs.110 ).aspx) campo:

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

Em C# 6 isso pode ser feito da seguinte forma:

    string concat = $"{first},{second}";



