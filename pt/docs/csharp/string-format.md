---
title: "String.Format"
slug: "stringformat"
draft: false
images: []
weight: 9519
type: docs
toc: true
---

Os métodos `Format` são um conjunto de [overloads][1] na classe [`System.String`][2] usados ​​para criar strings que combinam objetos em representações de strings específicas. Esta informação pode ser aplicada a [`String.Format`][1], vários métodos `WriteLine` assim como outros métodos no framework .NET.

[1]: https://msdn.microsoft.com/en-us/library/system.string.format(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx

## Sintaxe
- string.Format(formato de string, params object[] args)
- string.Format(provedor IFormatProvider, formato de string, params object[] args)
- $"string {text} blablabla" // Desde C#6

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| formato | Uma [string de formato composto][1], que define a forma como os *args* devem ser combinados em uma string. |
| argumentos | Uma sequência de objetos a serem combinados em uma string. Como isso usa um argumento [`params`][2], você pode usar uma lista de argumentos separados por vírgula ou um array de objetos real. |
| provedor | Uma coleção de maneiras de formatar objetos para strings. Os valores típicos incluem [CultureInfo.InvariantCulture][3] e [CultureInfo.CurrentCulture][4]. |


[1]: https://msdn.microsoft.com/en-us/library/txafckwd(v=vs.110).aspx
[2]: https://www.wikiod.com/pt/docs/c%23/26/keywords/2513/params#t=201607212143476676934
[3]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.invariantculture(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx

Notas:

- `String.Format()` trata argumentos `null` sem lançar uma exceção.
- Existem sobrecargas que substituem o parâmetro `args` por um, dois ou três parâmetros de objeto.



## Desde C# 6.0
<!-- if versão [gte 6.0] -->

Desde o C# 6.0 é possível usar interpolação de string no lugar de `String.Format`.

    string name = "John";
    string lastname = "Doe";
    Console.WriteLine($"Hello {name} {lastname}!");

> Olá John Doe!

<sup>Mais exemplos para isso no tópico Recursos do C# 6.0: https://www.wikiod.com/pt/docs/c%23/24/c-sharp-6-0-features/49/string-interpolation#t=201607220912379524818. </sup>

<!-- versão final if -->
 

## Locais onde String.Format é 'incorporado' no framework
Existem vários lugares onde você pode usar `String.Format` *indiretamente*: O segredo é procurar a sobrecarga com a assinatura `string format, params object[] args`, por exemplo:

    Console.WriteLine(String.Format("{0} - {1}", name, value));

Pode ser substituído por uma versão mais curta:

    Console.WriteLine("{0} - {1}", name, value);

Existem outros métodos que também usam `String.Format`, por exemplo:

    Debug.WriteLine(); // and Print()
    StringBuilder.AppendFormat();

## Crie um provedor de formato personalizado
    public class CustomFormat : IFormatProvider, ICustomFormatter
    {
        public string Format(string format, object arg, IFormatProvider formatProvider)
        {
            if (!this.Equals(formatProvider))
            {
                return null;
            }

            if (format == "Reverse")
            {
                return String.Join("", arg.ToString().Reverse());
            }

            return arg.ToString();
        }

        public object GetFormat(Type formatType)
        {
            return formatType==typeof(ICustomFormatter) ? this:null;
        }
    }

Uso:

    String.Format(new CustomFormat(), "-> {0:Reverse} <-", "Hello World");

Resultado:

    -> dlroW olleH <-

## Formatação de data
    DateTime date = new DateTime(2016, 07, 06, 18, 30, 14);
    // Format: year, month, day hours, minutes, seconds

    Console.Write(String.Format("{0:dd}",date)); 

    //Format by Culture info
    String.Format(new System.Globalization.CultureInfo("mn-MN"),"{0:dddd}",date);

<!-- if versão [gte 6.0] -->
    Console.Write($"{date:ddd}");
<!-- versão final if -->

resultado :

    06
    Лхагва
    06

| Especificador| Significado| Amostra| Resultado|
| ------ | ------ | ------ | ------ |
|d| Data |`{0:d}`|7/6/2016|
|dd| Dia, preenchido com zeros |`{0:dd}`|06|
|ddd|Nome do dia curto|`{0:ddd}`|Qua|
|dddd|Nome completo do dia|`{0:dddd}`|Quarta-feira|
|D|Data longa|`{0:D}`|Quarta-feira, 6 de julho de 2016|
|f|Data e hora completas, curtas|`{0:f}`|Quarta-feira, 6 de julho de 2016 18h30|
|ff|Segundas frações, 2 dígitos|`{0:ff}`|20|
|fff|Segundas frações, 3 dígitos|`{0:fff}`|201|
|ffff|Segundas frações, 4 dígitos|`{0:ffff}`|2016|
|F|Data e hora completas|`{0:F}`|Quarta-feira, 6 de julho de 2016 18:30:14|
|g|Data e hora padrão|`{0:g}`|7/6/2016 18h30|
|gg|Era|`{0:gg}`|A.D|
|hh|Hora (2 dígitos, 12H)|`{0:hh}`|06|
|HH|Hora (2 dígitos, 24H)|`{0:HH}`|18|
|M|Mês e dia|`{0:M}`|6 de julho|
|mm|Minutos, com zeros|`{0:mm}`|30|
|MM|Mês, preenchido com zeros|`{0:MM}`|07|
|MMM|nome do mês com 3 letras|`{0:MMM}`|Jul|
|MMMM|Nome do mês completo|`{0:MMMM}`|Julho|
|ss|Segundos|`{0:ss}`|14|
|r| RFC1123 data|`{0:r}`|Qua, 06 de julho de 2016 18:30:14 GMT|
|s| Sequência de data classificável|`{0:s}`|2016-07-06T18:30:14|
|t| Pouco tempo |`{0:t}`|18:30|
|T|Muito tempo|`{0:T}`|18:30:14|
|ht|AM/PM|`{0:ht}`|PM|
|u|Hora local universal classificável|`{0:u}`|2016-07-06 18:30:14Z|
|U| Universal GMT|`{0:U}`|Quarta-feira, 6 de julho de 2016 9:30:14 AM|
|S| Mês e ano|`{0:Y}`|Julho de 2016|
|aa|ano de 2 dígitos|`{0:aa}`|16|
|aaaa|ano de 4 dígitos|`{0:aaaa}`|2016|
|zz|Compensação de fuso horário de 2 dígitos|`{0:zz}`|+09|
|zzz|compensação de fuso horário completo|`{0:zzz}`|+09:00|

## Formatação de moeda
O especificador de formato "c" (ou moeda) converte um número em uma string que representa uma quantia em moeda.

    string.Format("{0:c}", 112.236677) // $112.23 - defaults to system

## Precisão ##
O padrão é 2. Use c1, c2, c3 e assim por diante para controlar a precisão.

    string.Format("{0:C1}", 112.236677) //$112.2
    string.Format("{0:C3}", 112.236677) //$112.237
    string.Format("{0:C4}", 112.236677) //$112.2367
    string.Format("{0:C9}", 112.236677) //$112.236677000

## Símbolo da moeda ##

1. Passe a instância `CultureInfo` para usar o símbolo de cultura personalizado.


    string.Format(new CultureInfo("en-US"), "{0:c}", 112.236677); //$112.24
    string.Format(new CultureInfo("de-DE"), "{0:c}", 112.236677); //112,24 €
    string.Format(new CultureInfo("hi-IN"), "{0:c}", 112.236677); //₹ 112.24


2. Use qualquer string como símbolo de moeda. Use `NumberFormatInfo` para personalizar o símbolo de moeda.


    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;
    nfi = (NumberFormatInfo) nfi.Clone();
    nfi.CurrencySymbol = "?";
    string.Format(nfi, "{0:C}", 112.236677); //?112.24
    nfi.CurrencySymbol = "?%^&";
    string.Format(nfi, "{0:C}", 112.236677); //?%^&112.24

## Posição do símbolo da moeda ##

Use [CurrencyPositivePattern][1] para valores positivos e [CurrencyNegativePattern][2] para valores negativos.

    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
    nfi.CurrencyPositivePattern = 0;
    string.Format(nfi, "{0:C}", 112.236677); //$112.24 - default
    nfi.CurrencyPositivePattern = 1;
    string.Format(nfi, "{0:C}", 112.236677); //112.24$
    nfi.CurrencyPositivePattern = 2;
    string.Format(nfi, "{0:C}", 112.236677); //$ 112.24
    nfi.CurrencyPositivePattern = 3; 
    string.Format(nfi, "{0:C}", 112.236677); //112.24 $

O uso de padrão negativo é o mesmo que o padrão positivo. Muito mais casos de uso, consulte o link original.

## Separador Decimal Personalizado ##

    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
    nfi.CurrencyPositivePattern = 0;
    nfi.CurrencyDecimalSeparator = "..";
    string.Format(nfi, "{0:C}", 112.236677); //$112..24

[1]: https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencypositivepattern(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencynegativepattern(v=vs.110).aspx

## Usando o formato de número personalizado
`NumberFormatInfo` pode ser usado para formatar números inteiros e flutuantes.

    // invariantResult is "1,234,567.89"
    var invarianResult = string.Format(CultureInfo.InvariantCulture, "{0:#,###,##}", 1234567.89);

    // NumberFormatInfo is one of classes that implement IFormatProvider
    var customProvider = new NumberFormatInfo
    {
        NumberDecimalSeparator = "_NS_", // will be used instead of ','
        NumberGroupSeparator = "_GS_", // will be used instead of '.'
    };

    // customResult is "1_GS_234_GS_567_NS_89"
    var customResult = string.Format(customProvider, "{0:#,###.##}", 1234567.89);



## Alinhar à esquerda/direita, preencher com espaços
O segundo valor entre chaves determina o comprimento da string de substituição.
Ajustando o segundo valor para positivo ou negativo, o alinhamento da string pode ser alterado.

    string.Format("LEFT:  string: ->{0,-5}<- int: ->{1,-5}<-", "abc", 123);
    string.Format("RIGHT: string: ->{0,5}<- int: ->{1,5}<-", "abc", 123);

Resultado:

    LEFT:  string: ->abc  <- int: ->123  <-
    RIGHT: string: ->  abc<- int: ->  123<-


## Formatos numéricos
    // Integral types as hex
    string.Format("Hexadecimal: byte2: {0:x2}; byte4: {0:X4}; char: {1:x2}", 123, (int)'A');

    // Integers with thousand separators
    string.Format("Integer, thousand sep.: {0:#,#}; fixed length: >{0,10:#,#}<", 1234567);

    // Integer with leading zeroes
    string.Format("Integer, leading zeroes: {0:00}; ", 1);

    // Decimals
    string.Format("Decimal, fixed precision: {0:0.000}; as percents: {0:0.00%}", 0.12);

Resultado:

    Hexadecimal: byte2: 7b; byte4: 007B; char: 41
    Integer, thousand sep.: 1,234,567; fixed length: > 1,234,567<
    Integer, leading zeroes: 01; 
    Decimal, fixed precision: 0.120; as percents: 12.00%


## Escapando chaves dentro de uma expressão String.Format()
    string outsidetext = "I am outside of bracket";
    string.Format("{{I am in brackets!}} {0}", outsidetext);

    //Outputs "{I am in brackets!} I am outside of bracket"

## Para sequenciar()
O método ToString() está presente em todos os tipos de objeto de referência. Isso ocorre porque todos os tipos de referência são derivados de Object que possui o método ToString() nele. O método ToString() na classe base do objeto retorna o nome do tipo. O fragmento abaixo imprimirá "Usuário" no console.

    public class User
    {
        public string Name { get; set; }
        public int Id { get; set; }
    }

    ...

    var user = new User {Name = "User1", Id = 5};
    Console.WriteLine(user.ToString());


No entanto, a classe User também pode substituir ToString() para alterar a string que ela retorna. O fragmento de código abaixo imprime "Id: 5, Name: User1" no console.

    public class User
    {
        public string Name { get; set; }
        public int Id { get; set; }
        public override ToString()
        {
            return string.Format("Id: {0}, Name: {1}", Id, Name);
        }
    }

    ...

    var user = new User {Name = "User1", Id = 5};
    Console.WriteLine(user.ToString());


## Relacionamento com ToString()
Embora o método `String.Format()` seja certamente útil na formatação de dados como strings, muitas vezes pode ser um pouco exagerado, especialmente ao lidar com um único objeto como visto abaixo:

    String.Format("{0:C}", money);  // yields "$42.00"

Uma abordagem mais fácil pode ser simplesmente usar o método `ToString()` disponível em todos os objetos em C#. Ele suporta todas as mesmas [strings de formatação padrão e personalizadas](https://msdn.microsoft.com/en-us/library/dwhawy9k(v=vs.110).aspx), mas não requer o parâmetro necessário mapeamento, pois haverá apenas um único argumento:

    money.ToString("C");  // yields "$42.00"

**Advertências e restrições de formatação**
---

Embora essa abordagem possa ser mais simples em alguns cenários, a abordagem `ToString()` é limitada no que diz respeito à adição de preenchimento esquerdo ou direito, como você pode fazer no método `String.Format()`:

    String.Format("{0,10:C}", money);  // yields "    $42.00"

Para realizar esse mesmo comportamento com o método `ToString()`, você precisaria usar outro método como `PadLeft()` ou `PadRight()` respectivamente:

    money.ToString("C").PadLeft(10);  // yields "    $42.00"

