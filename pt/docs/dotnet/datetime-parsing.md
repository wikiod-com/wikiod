---
title: "Análise de data e hora"
slug: "analise-de-data-e-hora"
draft: false
images: []
weight: 9816
type: docs
toc: true
---

## ParseExact
    var dateString = "2015-11-24";

    var date = DateTime.ParseExact(dateString, "yyyy-MM-dd", null);
    Console.WriteLine(date);

> 24/11/2015 12:00:00

Observe que passar `CultureInfo.CurrentCulture` como o terceiro parâmetro é idêntico a passar `null`. Ou você pode passar uma cultura específica.

**Formatar Strings**

*A string de entrada pode estar em qualquer formato que corresponda à string de formato*

    var date = DateTime.ParseExact("24|201511", "dd|yyyyMM", null);
    Console.WriteLine(date);

> 24/11/2015 12:00:00

*Quaisquer caracteres que não sejam especificadores de formato são tratados como literais*

    var date = DateTime.ParseExact("2015|11|24", "yyyy|MM|dd", null);
    Console.WriteLine(date);

> 24/11/2015 12:00:00

*Caso é importante para especificadores de formato*

    var date = DateTime.ParseExact("2015-01-24 11:11:30", "yyyy-mm-dd hh:MM:ss", null);
    Console.WriteLine(date);

> 24/11/2015 11:01:30

Observe que os valores de mês e minuto foram analisados ​​nos destinos errados.

*As strings de formato de caractere único devem ser um dos formatos padrão*

    var date = DateTime.ParseExact("11/24/2015", "d", new CultureInfo("en-US"));
    var date = DateTime.ParseExact("2015-11-24T10:15:45", "s", null);
    var date = DateTime.ParseExact("2015-11-24 10:15:45Z", "u", null);

**Exceções**

*ArgumentNullException*

    var date = DateTime.ParseExact(null, "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", null, null);

*FormatException*

    var date = DateTime.ParseExact("", "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", "", null);
    var date = DateTime.ParseExact("2015-0C-24", "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", "yyyy-QQ-dd", null);

    // Single-character format strings must be one of the standard formats
    var date = DateTime.ParseExact("2015-11-24", "q", null);

    // Format strings must match the input exactly* (see next section)
    var date = DateTime.ParseExact("2015-11-24", "d", null); // Expects 11/24/2015 or 24/11/2015 for most cultures

**Como lidar com vários formatos possíveis**

    var date = DateTime.ParseExact("2015-11-24T10:15:45", 
      new [] { "s", "t", "u", "yyyy-MM-dd" }, // Will succeed as long as input matches one of these
      CultureInfo.CurrentCulture, DateTimeStyles.None);

**Como lidar com diferenças culturais**

    var dateString = "10/11/2015";
    var date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-US"));
    Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

> Dia: 11; Mês: 10

    date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-GB"));
    Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

> Dia: 10; Mês: 11


## TryParse
Este método aceita uma string como entrada, tenta analisá-la em um `DateTime` e retorna um resultado booleano indicando sucesso ou falha. Se a chamada for bem-sucedida, a variável passada como o parâmetro `out` será preenchida com o resultado analisado.

Se a análise falhar, a variável passada como o parâmetro `out` é definida com o valor padrão, `DateTime.MinValue`.

**TryParse(string, out DateTime)**

    DateTime parsedValue;

    if (DateTime.TryParse("monkey", out parsedValue))
    {
       Console.WriteLine("Apparently, 'monkey' is a date/time value. Who knew?");
    }

Esse método tenta analisar a string de entrada com base nas configurações regionais do sistema e formatos conhecidos, como ISO 8601 e outros formatos comuns.

    DateTime.TryParse("11/24/2015 14:28:42", out parsedValue); // true
    DateTime.TryParse("2015-11-24 14:28:42", out parsedValue); // true
    DateTime.TryParse("2015-11-24T14:28:42", out parsedValue); // true
    DateTime.TryParse("Sat, 24 Nov 2015 14:28:42", out parsedValue); // true

Como esse método não aceita informações de cultura, ele usa a localidade do sistema. Isso pode levar a resultados inesperados.

    // System set to en-US culture
    bool result = DateTime.TryParse("24/11/2015", out parsedValue);
    Console.WriteLine(result);

> Falso

    // System set to en-GB culture
    bool result = DateTime.TryParse("11/24/2015", out parsedValue);
    Console.WriteLine(result);

> Falso

    // System set to en-GB culture
    bool result = DateTime.TryParse("10/11/2015", out parsedValue);
    Console.WriteLine(result);

> Verdadeiro

Observe que, se você estiver nos EUA, poderá se surpreender que o resultado analisado seja 10 de novembro, não 11 de outubro.

**TryParse(string, IFormatProvider, DateTimeStyles, out DateTime)**

    if (DateTime.TryParse(" monkey ", new CultureInfo("en-GB"),
        DateTimeStyles.AllowLeadingWhite | DateTimeStyles.AllowTrailingWhite, out parsedValue)
    {
        Console.WriteLine("Apparently, ' monkey ' is a date/time value. Who knew?");
    }

Ao contrário de seu método irmão, essa sobrecarga permite que uma cultura e estilos específicos sejam especificados. Passar `null` para o parâmetro `IFormatProvider` usa a cultura do sistema.

*Exceções*

Observe que é possível que esse método lance uma exceção sob certas condições. Eles estão relacionados aos parâmetros introduzidos para essa sobrecarga: `IFormatProvider` e `DateTimeStyles`.

* `NotSupportedException`: `IFormatProvider` especifica uma cultura neutra
* `ArgumentException`: `DateTimeStyles` não é uma opção válida ou contém sinalizadores incompatíveis como `AssumeLocal` e `AssumeUniversal`.

## TryParseExact
Este método se comporta como uma combinação de `TryParse` e ​​`ParseExact`: Ele permite que formatos personalizados sejam especificados e retorna um resultado booleano indicando sucesso ou falha em vez de lançar uma exceção se a análise falhar.

**TryParseExact(string, string, IFormatProvider, DateTimeStyles, fora DateTime)**

Essa sobrecarga tenta analisar a string de entrada em um formato específico. A string de entrada deve corresponder a esse formato para ser analisada.

    DateTime.TryParseExact("11242015", "MMddyyyy", null, DateTimeStyles.None, out parsedValue); // true

**TryParseExact(string, string[], IFormatProvider, DateTimeStyles, out DateTime)**

Essa sobrecarga tenta analisar a string de entrada em uma matriz de formatos. A string de entrada deve corresponder a pelo menos um formato para ser analisada.

    DateTime.TryParseExact("11242015", new [] { "yyyy-MM-dd", "MMddyyyy" }, null, DateTimeStyles.None, out parsedValue); // true


