---
title: "Enum"
slug: "enum"
draft: false
images: []
weight: 9420
type: docs
toc: true
---

Um enum pode derivar de qualquer um dos seguintes tipos: byte, sbyte, short, ushort, int, uint, long, ulong. O padrão é int e pode ser alterado especificando o tipo na definição de enum:

public enum Dia da semana : byte { segunda = 1, terça = 2, quarta = 3, quinta = 4, sexta = 5 }

Isso é útil ao P/Invocar código nativo, mapear para fontes de dados e circunstâncias semelhantes. Em geral, o int padrão deve ser usado, porque a maioria dos desenvolvedores espera que um enum seja um int.

## Sintaxe
- enum Colors { Red, Green, Blue } // Declaração de enum
- enum Colors : byte { Red, Green, Blue } // Declaração com tipo específico
- enum Colors { Red = 23, Green = 45, Blue = 12 } // Declaração com valores definidos
- Colors.Red // Acessa um elemento de um Enum
- int value = (int)Colors.Red // Obtém o valor int de um elemento enum
- Colors color = (Colors)intValue // Obtém um elemento enum de int

Um Enum (abreviação de "tipo enumerado") é um tipo que consiste em um conjunto de constantes nomeadas, representadas por um identificador específico do tipo.

Enums são mais úteis para representar conceitos que possuem um número (geralmente pequeno) de valores discretos possíveis. Por exemplo, eles podem ser usados ​​para representar um dia da semana ou um mês do ano. Eles também podem ser usados ​​como sinalizadores que podem ser combinados ou verificados, usando operações bit a bit.

## Enum como sinalizadores
O `FlagsAttribute` pode ser aplicado a um enum alterando o comportamento do `ToString()` para corresponder à natureza do enum:

    [Flags]
    enum MyEnum
    {
        //None = 0, can be used but not combined in bitwise operations
        FlagA = 1,
        FlagB = 2,
        FlagC = 4,
        FlagD = 8  
        //you must use powers of two or combinations of powers of two 
        //for bitwise operations to work
    }
    
    var twoFlags = MyEnum.FlagA | MyEnum.FlagB;
    
    // This will enumerate all the flags in the variable: "FlagA, FlagB".
    Console.WriteLine(twoFlags);

Como `FlagsAttribute` depende das constantes de enumeração para serem potências de dois (ou suas combinações) e os valores de enumeração são, em última análise, valores numéricos, você está limitado pelo tamanho do tipo numérico subjacente. O maior tipo numérico disponível que você pode usar é `UInt64`, que permite especificar 64 constantes de enumeração de sinalizador distintas (não combinadas). A palavra-chave `enum` padroniza para o tipo subjacente `int`, que é `Int32`. O compilador permitirá a declaração de valores maiores que 32 bits. Eles serão agrupados sem aviso e resultarão em dois ou mais membros enum com o mesmo valor. Portanto, se um enum destina-se a acomodar um bitset de mais de 32 sinalizadores, você precisa especificar um tipo maior explicitamente:

    public enum BigEnum : ulong
    {
        BigValue = 1 << 63
    }

Embora os sinalizadores geralmente sejam apenas um único bit, eles podem ser combinados em "conjuntos" nomeados para facilitar o uso.

    [Flags]
    enum FlagsEnum
    {
        None = 0,
        Option1 = 1,
        Option2 = 2,
        Option3 = 4,
           
        Default = Option1 | Option3,
        All = Option1 | Option2 | Option3,
    }

Para evitar soletrar os valores decimais de potências de dois, o [operador de deslocamento à esquerda (<<)](https://msdn.microsoft.com/en-gb/library/a1sway8w.aspx) também pode ser usado para declarar o mesmo enum

    [Flags]
    enum FlagsEnum
    {
        None = 0,
        Option1 = 1 << 0,
        Option2 = 1 << 1,
        Option3 = 1 << 2,
           
        Default = Option1 | Option3,
        All = Option1 | Option2 | Option3,
    }

A partir do C# 7.0, [literais binários](https://www.wikiod.com/pt/docs/c%23/1936/c-sharp-7-0-features/6327/binary-literals#t=201705181538117083427) também podem ser usados .

Para verificar se o valor da variável enum tem um determinado sinalizador definido, o método [`HasFlag`][1] pode ser usado. Digamos que temos

    [Flags]
    enum MyEnum
    {
        One = 1,
        Two = 2,
        Three = 4
    }

E um `valor`
    
    var value = MyEnum.One | MyEnum.Two;

Com `HasFlag` podemos verificar se algum dos sinalizadores está definido
    
    if(value.HasFlag(MyEnum.One))
        Console.WriteLine("Enum has One");

    if(value.HasFlag(MyEnum.Two))
        Console.WriteLine("Enum has Two");

    if(value.HasFlag(MyEnum.Three))
        Console.WriteLine("Enum has Three");

Também podemos percorrer todos os valores de enum para obter todos os sinalizadores definidos

    var type = typeof(MyEnum);
    var names = Enum.GetNames(type);

    foreach (var name in names)
    {
        var item = (MyEnum)Enum.Parse(type, name);

        if (value.HasFlag(item))
            Console.WriteLine("Enum has " + name);
    }
    
Ou

    foreach(MyEnum flagToCheck in Enum.GetValues(typeof(MyEnum)))
    {
        if(value.HasFlag(flagToCheck))
        {
             Console.WriteLine("Enum has " + flagToCheck);
        }
    }

Todos os três exemplos serão impressos:

    Enum has One
    Enum has Two


[1]: https://msdn.microsoft.com/en-us/library/system.enum.hasflag(v=vs.110).aspx

## Noções básicas de enumeração

De [MSDN][1]:
> Um tipo de enumeração (também chamado de enumeração ou enumeração) fornece uma maneira eficiente de definir um conjunto de **constantes integrais** nomeadas que podem ser **atribuídas a uma variável**.

Essencialmente, um enum é um tipo que permite apenas um conjunto de opções finitas, e cada opção corresponde a um número. Por padrão, esses números estão aumentando na ordem em que os valores são declarados, começando do zero. Por exemplo, pode-se declarar um enum para os dias da semana:

    public enum Day
    {
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
        Sunday
    }

Esse enum poderia ser usado assim:

    // Define variables with values corresponding to specific days
    Day myFavoriteDay = Day.Friday;
    Day myLeastFavoriteDay = Day.Monday;
    
    // Get the int that corresponds to myFavoriteDay
    // Friday is number 4
    int myFavoriteDayIndex = (int)myFavoriteDay;
    
    // Get the day that represents number 5
    Day dayFive = (Day)5;

Por padrão, o tipo subjacente de cada elemento no `enum` é `int`, mas `byte`, `sbyte`, `short`, `ushort`, `uint`, `long` e `ulong` podem ser usados ​​como Nós vamos. Se você usar um tipo diferente de `int`, você deve especificar o tipo usando dois pontos após o nome da enumeração:

    public enum Day : byte 
    {
        // same as before 
    }

Os números após o nome agora são bytes em vez de inteiros. Você pode obter o tipo subjacente da enumeração da seguinte maneira:

    Enum.GetUnderlyingType(typeof(Days)));

Resultado:

<!-- idioma: nenhum -->
    System.Byte

Demonstração: [.NET violino][2]

[1]: https://msdn.microsoft.com/en-us/library/cc138362.aspx

[2]: https://dotnetfiddle.net/EGi301

## Usando a notação << para sinalizadores
O operador left-shift (`<<`) pode ser usado em declarações de enumeração de sinalizadores para garantir que cada sinalizador tenha exatamente um `1` na representação binária, como os sinalizadores deveriam.

Isso também ajuda a melhorar a legibilidade de enums grandes com muitos sinalizadores.


    [Flags]
    public enum MyEnum 
    {
        None  = 0,
        Flag1 = 1 << 0,
        Flag2 = 1 << 1,
        Flag3 = 1 << 2,
        Flag4 = 1 << 3,
        Flag5 = 1 << 4,
        ...
        Flag31 = 1 << 30
    }

É óbvio agora que `MyEnum` contém apenas sinalizadores adequados e não qualquer coisa confusa como `Flag30 = 1073741822` (ou 11111111111111111111111111110 em binário), o que é inadequado.

## Teste valores de enum estilo sinalizadores com lógica bit a bit
Um valor enum estilo sinalizadores precisa ser testado com lógica bit a bit porque pode não corresponder a nenhum valor único.

    [Flags]
    enum FlagsEnum
    {
        Option1 = 1,
        Option2 = 2,
        Option3 = 4,
        Option2And3 = Option2 | Option3;
    
        Default = Option1 | Option3,
    }
    
O valor `Default` é na verdade uma combinação de dois outros _merged_ com um OR bit a bit. Portanto, para testar a presença de um sinalizador, precisamos usar um AND bit a bit.

    var value = FlagsEnum.Default;

    bool isOption2And3Set = (value & FlagsEnum.Option2And3) == FlagsEnum.Option2And3;

    Assert.True(isOption2And3Set);



## Enum para string e vice-versa
    public enum DayOfWeek
    {
        Sunday,
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday
    }
    
        
    // Enum to string
    string thursday = DayOfWeek.Thursday.ToString(); // "Thursday"
    
    string seventhDay = Enum.GetName(typeof(DayOfWeek), 6); // "Saturday"
    
    string monday = Enum.GetName(typeof(DayOfWeek), DayOfWeek.Monday); // "Monday"
    
    
    // String to enum (.NET 4.0+ only - see below for alternative syntax for earlier .NET versions)
    DayOfWeek tuesday;
    Enum.TryParse("Tuesday", out tuesday); // DayOfWeek.Tuesday
    
    DayOfWeek sunday;
    bool matchFound1 = Enum.TryParse("SUNDAY", out sunday); // Returns false (case-sensitive match)
    
    DayOfWeek wednesday;
    bool matchFound2 = Enum.TryParse("WEDNESDAY", true, out wednesday); // Returns true; DayOfWeek.Wednesday (case-insensitive match)
    
    
    // String to enum (all .NET versions)
    DayOfWeek friday = (DayOfWeek)Enum.Parse(typeof(DayOfWeek), "Friday"); // DayOfWeek.Friday

    DayOfWeek caturday = (DayOfWeek)Enum.Parse(typeof(DayOfWeek), "Caturady"); // Thows ArgumentException
    
    // All names of an enum type as strings
    string[] weekdays = Enum.GetNames(typeof(DayOfWeek));

## Adicionar e remover valores da enumeração sinalizada
Este código é para adicionar e remover um valor de uma instância enum sinalizada:

    [Flags]
    public enum MyEnum
    {
        Flag1 = 1 << 0,
        Flag2 = 1 << 1,
        Flag3 = 1 << 2
    }

    var value = MyEnum.Flag1;

    // set additional value
    value |= MyEnum.Flag2;  //value is now Flag1, Flag2
    value |= MyEnum.Flag3;  //value is now Flag1, Flag2, Flag3

    // remove flag
    value &= ~MyEnum.Flag2; //value is now Flag1, Flag3    


## Valor padrão para enum == ZERO
**O valor padrão para uma enumeração é zero**. Se um enum não definir um item com valor zero, seu valor padrão será zero.
    
    public class Program
    {        
        enum EnumExample
        {
            one = 1,
            two = 2
        }
        
        public void Main()
        {              
            var e = default(EnumExample);
            
            if (e == EnumExample.one)
                Console.WriteLine("defaults to one");
            else
                Console.WriteLine("Unknown");    
        }    
    }

Exemplo:
https://dotnetfiddle.net/l5Rwie

## Adicionando informações de descrição adicionais a um valor enum
Em alguns casos, você pode querer adicionar uma descrição adicional a um valor de enumeração, por exemplo, quando o próprio valor de enumeração for menos legível do que o que você deseja exibir para o usuário. Nesses casos, você pode usar a classe [`System.ComponentModel.DescriptionAttribute`](https://msdn.microsoft.com/en-us/library/system.componentmodel.descriptionattribute(v=vs.110).aspx).

Por exemplo:

    public enum PossibleResults
    {
        [Description("Success")]
        OK = 1,
        [Description("File not found")]
        FileNotFound = 2,
        [Description("Access denied")]
        AccessDenied = 3
    }

Agora, se você quiser retornar a descrição de um valor enum específico, faça o seguinte:

    public static string GetDescriptionAttribute(PossibleResults result)
    {
            return ((DescriptionAttribute)Attribute.GetCustomAttribute((result.GetType().GetField(result.ToString())), typeof(DescriptionAttribute))).Description;
    }

    static void Main(string[] args)
    {
        PossibleResults result = PossibleResults.FileNotFound;
        Console.WriteLine(result); // Prints "FileNotFound"
        Console.WriteLine(GetDescriptionAttribute(result)); // Prints "File not found"
    }

Isso também pode ser facilmente transformado em um método de extensão para todas as enumerações:

    static class EnumExtensions
    {
        public static string GetDescription(this Enum enumValue)
        {
            return ((DescriptionAttribute)Attribute.GetCustomAttribute((enumValue.GetType().GetField(enumValue.ToString())), typeof(DescriptionAttribute))).Description;
        }
    }

E então facilmente usado assim:
`Console.WriteLine(result.GetDescription());`


## Enums podem ter valores inesperados
Como uma enumeração pode ser convertida de e para seu tipo integral subjacente, o valor pode ficar fora do intervalo de valores fornecido na definição do tipo de enumeração.

Embora o tipo enum abaixo `DaysOfWeek` tenha apenas 7 valores definidos, ele ainda pode conter qualquer valor `int`.

    public enum DaysOfWeek
    {
        Monday = 1,
        Tuesday = 2,
        Wednesday = 3,
        Thursday = 4,
        Friday = 5,
        Saturday = 6,
        Sunday = 7
    }

    DaysOfWeek d = (DaysOfWeek)31;
    Console.WriteLine(d); // prints 31

    DaysOFWeek s = DaysOfWeek.Sunday;
    s++; // No error

Atualmente, não há como definir uma enumeração que não tenha esse comportamento.

No entanto, valores de enum indefinidos podem ser detectados usando o método `Enum.IsDefined`. Por exemplo,

    DaysOfWeek d = (DaysOfWeek)31;
    Console.WriteLine(Enum.IsDefined(typeof(DaysOfWeek),d)); // prints False

## Obtém todos os valores dos membros de um enum
    enum MyEnum
    {
        One,
        Two,
        Three
    }
    
    foreach(MyEnum e in Enum.GetValues(typeof(MyEnum)))
        Console.WriteLine(e);

Isso imprimirá:

    One
    Two
    Three

## Manipulação bit a bit usando enums
O [FlagsAttribute][1] deve ser usado sempre que o enumerable representar uma coleção de sinalizadores, em vez de um único valor.
O valor numérico atribuído a cada valor de enum ajuda ao manipular enums usando operadores bit a bit.


**Exemplo 1: Com [Sinalizadores]**

    [Flags]
    enum Colors
    {
        Red=1,
        Blue=2,
        Green=4,
        Yellow=8
    }

    var color = Colors.Red | Colors.Blue;
    Console.WriteLine(color.ToString());

> imprime Vermelho, Azul

    

****Exemplo 2: Sem [Sinalizadores]****

  
    enum Colors
    {
        Red=1,
        Blue=2,
        Green=4,
        Yellow=8
    }
    var color = Colors.Red | Colors.Blue;
    Console.WriteLine(color.ToString());

> imprime 3


[1]: https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx

