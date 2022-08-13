---
title: "Recursos do C# 7.0"
slug: "recursos-do-c-70"
draft: false
images: []
weight: 1717
type: docs
toc: true
---

C# 7.0 é a sétima versão do C#. Esta versão contém alguns novos recursos: suporte de linguagem para Tuplas, funções locais, declarações `out var`, separadores de dígitos, literais binários, correspondência de padrões, expressões throw, `ref return` e `ref local` e lista de membros corpo de expressão estendida.

Referência oficial: [O que há de novo no C# 7](https://docs.microsoft.com/en-us/dotnet/articles/csharp/csharp-7)

## Suporte a idiomas para Tuplas
# Fundamentos

Uma **tupla** é uma lista ordenada e finita de elementos. Tuplas são comumente usadas em programação como um meio de trabalhar com uma única entidade coletivamente em vez de trabalhar individualmente com cada um dos elementos da tupla e para representar linhas individuais (ou seja, "registros") em um banco de dados relacional.

No C# 7.0, os métodos podem ter vários valores de retorno. Nos bastidores, o compilador usará a nova estrutura [ValueTuple][1].

    public (int sum, int count) GetTallies() 
    {
        return (1, 2);
    }

_Observação_: para que isso funcione no Visual Studio 2017, você precisa obter o pacote ```System.ValueTuple```.

Se um resultado de método de retorno de tupla for atribuído a uma única variável, você poderá acessar os membros por seus nomes definidos na assinatura do método:

    var result = GetTallies();
    // > result.sum
    // 1
    // > result.count
    // 2

# Desconstrução de Tuplas

A desconstrução de tuplas separa uma tupla em suas partes.

Por exemplo, invocar `GetTallies` e atribuir o valor de retorno a duas variáveis ​​separadas desconstrói a tupla nessas duas variáveis:

    (int tallyOne, int tallyTwo) = GetTallies();

`var` também funciona:

    (var s, var c) = GetTallies();

Você também pode usar uma sintaxe mais curta, com `var` fora de `()`:

    var (s, c) = GetTallies();

Você também pode desconstruir em variáveis ​​existentes:

    int s, c;
    (s, c) = GetTallies();

A troca agora é muito mais simples (não é necessária nenhuma variável temporária):

    (b, a) = (a, b);

Curiosamente, qualquer objeto pode ser desconstruído definindo um método `Deconstruct` na classe:

    class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }

        public void Deconstruct(out string firstName, out string lastName)
        {
            firstName = FirstName;
            lastName = LastName;
        }
    }

    var person = new Person { FirstName = "John", LastName = "Smith" };
    var (localFirstName, localLastName) = person;

Neste caso, a sintaxe `(localFirstName, localLastName) = person` está invocando `Deconstruct` na `person`.

A desconstrução pode até ser definida em um método de extensão. Isso é equivalente ao acima:

    public static class PersonExtensions
    {
        public static void Deconstruct(this Person person, out string firstName, out string lastName)
        {
            firstName = person.FirstName;
            lastName = person.LastName;
        }
    }
    
    var (localFirstName, localLastName) = person;

Uma abordagem alternativa para a classe `Person` é definir o próprio `Name` como uma `Tupla`. Considere o seguinte:

    class Person
    {
        public (string First, string Last) Name { get; }

        public Person((string FirstName, string LastName) name)
        {
            Name = name;
        }
    }

Então você pode instanciar uma pessoa assim (onde podemos tomar uma tupla como argumento):

    var person = new Person(("Jane", "Smith"));

    var firstName = person.Name.First; // "Jane"
    var lastName = person.Name.Last;   // "Smith"

# Inicialização da Tupla
Você também pode criar tuplas arbitrariamente no código:

    var name = ("John", "Smith");
    Console.WriteLine(name.Item1);
    // Outputs John

    Console.WriteLine(name.Item2);
    // Outputs Smith

# 

Ao criar uma tupla, você pode atribuir nomes de itens ad hoc aos membros da tupla:

    var name = (first: "John", middle: "Q", last: "Smith");
    Console.WriteLine(name.first);
    // Outputs John

# Tipo de inferência

Múltiplas tuplas definidas com a mesma assinatura (tipos e contagem correspondentes) serão inferidas como tipos correspondentes. Por exemplo:

    public (int sum, double average) Measure(List<int> items)
    {
        var stats = (sum: 0, average: 0d);
        stats.sum = items.Sum();
        stats.average = items.Average();
        return stats;
    }

`stats` pode ser retornado desde que a declaração da variável `stats` e a assinatura de retorno do método sejam uma correspondência.

# Nomes de campo de reflexão e tupla
Os nomes dos membros não existem em tempo de execução. O Reflection considerará as tuplas com o mesmo número e tipos de membros iguais, mesmo que os nomes dos membros não correspondam. Converter uma tupla em um `objeto` e depois em uma tupla com os mesmos tipos de membros, mas nomes diferentes, também não causará uma exceção.

Embora a própria classe ValueTuple não preserve informações para nomes de membros, as informações estão disponíveis por meio de reflexão em um TupleElementNamesAttribute. Este atributo não é aplicado à tupla em si, mas aos parâmetros do método, valores de retorno, propriedades e campos. Isso permite que os nomes de itens de tupla sejam preservados em assemblies, ou seja, se um método retornar (nome da string, contagem int) os nomes name e count estarão disponíveis para os chamadores do método em outro assembly porque o valor de retorno será marcado com TupleElementNameAttribute contendo os valores "nome" e "conta".

# Use com genéricos e `async`

Os novos recursos de tupla (usando o tipo `ValueTuple` subjacente) suportam totalmente os genéricos e podem ser usados ​​como parâmetro de tipo genérico. Isso torna possível usá-los com o padrão `async`/`await`:

    public async Task<(string value, int count)> GetValueAsync()
    {
        string fooBar = await _stackoverflow.GetStringAsync();
        int num = await _stackoverflow.GetIntAsync();

        return (fooBar, num);
    }

# Use com coleções

Pode ser benéfico ter uma coleção de tuplas (como exemplo) em um cenário em que você está tentando encontrar uma tupla correspondente com base em condições para evitar a ramificação de código.

Exemplo:

    private readonly List<Tuple<string, string, string>> labels = new List<Tuple<string, string, string>>()
    {
        new Tuple<string, string, string>("test1", "test2", "Value"),
        new Tuple<string, string, string>("test1", "test1", "Value2"),
        new Tuple<string, string, string>("test2", "test2", "Value3"),
    };

    public string FindMatchingValue(string firstElement, string secondElement)
    {
        var result = labels
            .Where(w => w.Item1 == firstElement && w.Item2 == secondElement)
            .FirstOrDefault();

        if (result == null)
            throw new ArgumentException("combo not found");

        return result.Item3;
    }

Com as novas tuplas pode se tornar:

    private readonly List<(string firstThingy, string secondThingyLabel, string foundValue)> labels = new List<(string firstThingy, string secondThingyLabel, string foundValue)>()
    {
        ("test1", "test2", "Value"),
        ("test1", "test1", "Value2"),
        ("test2", "test2", "Value3"),
    }

    public string FindMatchingValue(string firstElement, string secondElement)
    {
        var result = labels
            .Where(w => w.firstThingy == firstElement && w.secondThingyLabel == secondElement)
            .FirstOrDefault();

        if (result == null)
            throw new ArgumentException("combo not found");

        return result.foundValue;
    }

Embora a nomenclatura na tupla de exemplo acima seja bastante genérica, a ideia de rótulos relevantes permite uma compreensão mais profunda do que está sendo tentado no código ao fazer referência a "item1", "item2" e "item3".

# Diferenças entre ValueTuple e Tuple

A principal razão para a introdução do `ValueTuple` é o desempenho.

| Nome do tipo | `ValueTuple` | `Tupla` |
|---|---|---|
| Classe ou estrutura | `estrutura` | `classe` |
| Mutabilidade (alteração de valores após a criação) | mutável | imutável |
| Nomeação de membros e suporte a outros idiomas | sim | não ([TBD][2]) |

# Referências

- [Proposta de recurso de linguagem Tuplas originais no GitHub][3]
- [Uma solução VS 15 executável para recursos do C# 7.0][4]
- [Pacote de Tupla NuGet][5]


[1]: https://github.com/dotnet/corefx/blob/master/src/System.ValueTuple/src/System/ValueTuple/ValueTuple.cs
[2]: https://github.com/dotnet/roslyn/issues/11031
[3]: https://github.com/dotnet/roslyn/issues/347
[4]: https://code.msdn.microsoft.com/Introduce-new-C-70-features-c639ed88
[5]: https://www.nuget.org/packages/System.ValueTuple/

## Funções locais
As funções locais são definidas dentro de um método e não estão disponíveis fora dele. Eles têm acesso a todas as variáveis ​​locais e suportam iteradores, `async`/`await` e sintaxe lambda. Dessa forma, repetições específicas de uma função podem ser funcionalizadas sem sobrecarregar a classe. Como efeito colateral, isso melhora o desempenho da sugestão do Intellisense.

# Exemplo

    double GetCylinderVolume(double radius, double height)
    {
        return getVolume();
  
        double getVolume()
        {
            // You can declare inner-local functions in a local function 
            double GetCircleArea(double r) => Math.PI * r * r;

            // ALL parents' variables are accessible even though parent doesn't have any input. 
            return GetCircleArea(radius) * height;
        }
    }

As funções locais simplificam consideravelmente o código para operadores LINQ, onde você geralmente precisa separar as verificações de argumento da lógica real para tornar as verificações de argumento instantâneas, não atrasadas até o início da iteração.

# Exemplo

    public static IEnumerable<TSource> Where<TSource>(
        this IEnumerable<TSource> source, 
        Func<TSource, bool> predicate)
    {
        if (source == null) throw new ArgumentNullException(nameof(source));
        if (predicate == null) throw new ArgumentNullException(nameof(predicate));
    
        return iterator();

        IEnumerable<TSource> iterator()
        {
            foreach (TSource element in source)
                if (predicate(element))
                    yield return element;
        }
    }

As funções locais também suportam as palavras-chave `async` e `await`.

# Exemplo

    async Task WriteEmailsAsync()
    {
        var emailRegex = new Regex(@"(?i)[a-z0-9_.+-]+@[a-z0-9-]+\.[a-z0-9-.]+");
        IEnumerable<string> emails1 = await getEmailsFromFileAsync("input1.txt");
        IEnumerable<string> emails2 = await getEmailsFromFileAsync("input2.txt");
        await writeLinesToFileAsync(emails1.Concat(emails2), "output.txt");

        async Task<IEnumerable<string>> getEmailsFromFileAsync(string fileName)
        {
            string text;

            using (StreamReader reader = File.OpenText(fileName))
            {
                text = await reader.ReadToEndAsync();
            }

            return from Match emailMatch in emailRegex.Matches(text) select emailMatch.Value;
        }

        async Task writeLinesToFileAsync(IEnumerable<string> lines, string fileName)
        {
            using (StreamWriter writer = File.CreateText(fileName))
            {
                foreach (string line in lines)
                {
                    await writer.WriteLineAsync(line);
                }
            }
        }
    }

Uma coisa importante que você deve ter notado é que as funções locais podem ser definidas sob a instrução `return`, elas **não** precisam ser definidas acima dela. Além disso, as funções locais normalmente seguem a convenção de nomenclatura "lowerCamelCase" para se diferenciar mais facilmente das funções de escopo de classe.

## declaração de var
Um padrão comum em C# é usar `bool TryParse(object input, out object value)` para analisar objetos com segurança.

A declaração `out var` é um recurso simples para melhorar a legibilidade. Permite que uma variável seja declarada ao mesmo tempo em que é passada como parâmetro de saída.

Uma variável declarada dessa maneira tem escopo para o restante do corpo no ponto em que é declarada.

# Exemplo

Usando `TryParse` antes do C# 7.0, você deve declarar uma variável para receber o valor antes de chamar a função:

<!-- if versão [lt 7.0] -->
    int value;
    if (int.TryParse(input, out value)) 
    {
        Foo(value); // ok
    }
    else
    {
        Foo(value); // value is zero
    }

    Foo(value); // ok
<!-- versão final if -->

No C# 7.0, você pode inline a declaração da variável passada para o parâmetro `out`, eliminando a necessidade de uma declaração de variável separada:

<!-- if versão [gte 7.0] -->
    if (int.TryParse(input, out var value)) 
    {
        Foo(value); // ok
    }
    else
    {
        Foo(value); // value is zero
    }

    Foo(value); // still ok, the value in scope within the remainder of the body
<!-- versão final if -->

Se alguns dos parâmetros que uma função retorna em `out` não são necessários, você pode usar o operador _discard_ `_`.

    p.GetCoordinates(out var x, out _); // I only care about x

Uma declaração `out var` pode ser usada com qualquer função existente que já tenha parâmetros `out`. A sintaxe da declaração da função permanece a mesma, e nenhum requisito adicional é necessário para tornar a função compatível com uma declaração `out var`. Esse recurso é simplesmente açúcar sintático.

Outra característica da declaração `out var` é que ela pode ser usada com tipos anônimos.

<!-- if versão [gte 7.0] -->
    var a = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    var groupedByMod2 = a.Select(x => new
                                      {
                                          Source = x,
                                          Mod2 = x % 2
                                      })
                         .GroupBy(x => x.Mod2)
                         .ToDictionary(g => g.Key, g => g.ToArray());
    if (groupedByMod2.TryGetValue(1, out var oddElements))
    {
        Console.WriteLine(oddElements.Length);
    }
<!-- versão final if -->
    
Neste código criamos um `Dicionário` com chave `int` e array de valor de tipo anônimo. Na versão anterior do C# era impossível usar o método `TryGetValue` aqui, pois exigia que você declarasse a variável `out` (que é do tipo anônimo!). No entanto, com `out var` não precisamos especificar explicitamente o tipo da variável `out`.

# Limitações

Observe que as declarações var são de uso limitado em consultas LINQ, pois as expressões são interpretadas como corpos lambda de expressão, portanto, o escopo das variáveis ​​introduzidas é limitado a esses lambdas. Por exemplo, o código a seguir não funcionará:

    var nums = 
        from item in seq
        let success = int.TryParse(item, out var tmp)
        select success ? tmp : 0; // Error: The name 'tmp' does not exist in the current context



# Referências

* [Proposta de declaração de var original no GitHub](https://github.com/dotnet/roslyn/issues/6183)

## Correspondência de padrões
As extensões de correspondência de padrões para C# permitem muitos dos benefícios da correspondência de padrões de linguagens funcionais, mas de uma maneira que se integra suavemente à sensação da linguagem subjacente

**Expressão `switch`**
-----
A correspondência de padrões estende a instrução `switch` para ativar os tipos:

    class Geometry {} 

    class Triangle : Geometry
    {
        public int Width { get; set; }
        public int Height { get; set; }
        public int Base { get; set; }
    }

    class Rectangle : Geometry
    {
        public int Width { get; set; }
        public int Height { get; set; }
    }

    class Square : Geometry
    {
        public int Width { get; set; }
    }

    public static void PatternMatching()
    {
        Geometry g = new Square { Width = 5 }; 
        
        switch (g)
        {
            case Triangle t:
                Console.WriteLine($"{t.Width} {t.Height} {t.Base}");
                break;
            case Rectangle sq when sq.Width == sq.Height:
                Console.WriteLine($"Square rectangle: {sq.Width} {sq.Height}");
                break;
            case Rectangle r:
                Console.WriteLine($"{r.Width} {r.Height}");
                break;
            case Square s:
                Console.WriteLine($"{s.Width}");
                break;
            default:
                Console.WriteLine("<other>");
                break;
        }
    }


**'é' expressão**
---- 

A correspondência de padrões estende o operador `is` para verificar um tipo e declarar uma nova variável ao mesmo tempo.

# Exemplo

<!-- if versão [lt 7.0] -->
    string s = o as string;
    if(s != null)
    {
        // do something with s
    }
<!-- versão final if -->

pode ser reescrita como:

<!-- if versão [gte 7.0] -->
    if(o is string s)
    {
        //Do something with s
    };
<!-- versão final if -->

Observe também que o escopo da variável padrão `s` é estendido para fora do bloco `if` atingindo o final do escopo delimitador, exemplo:

    if(someCondition)
    {
       if(o is string s)
       {
          //Do something with s
       }
       else
       {
         // s is unassigned here, but accessible 
       }
    
       // s is unassigned here, but accessible 
    }
    // s is not accessible here

## Separadores de dígitos
O sublinhado `_` pode ser usado como separador de dígitos. Ser capaz de agrupar dígitos em literais numéricos grandes tem um impacto significativo na legibilidade.

O sublinhado pode ocorrer em qualquer lugar em um literal numérico, exceto conforme indicado abaixo. Diferentes agrupamentos podem fazer sentido em diferentes cenários ou com diferentes bases numéricas.

Qualquer sequência de dígitos pode ser separada por um ou mais sublinhados. O `_` é permitido tanto em decimais quanto em expoentes. Os separadores não têm impacto semântico - eles são simplesmente ignorados.

    int bin = 0b1001_1010_0001_0100;
    int hex = 0x1b_a0_44_fe;
    int dec = 33_554_432;
    int weird = 1_2__3___4____5_____6______7_______8________9;
    double real = 1_000.111_1e-1_000;

**Onde o separador de dígitos `_` não pode ser usado:**
- no início do valor (`_121`)
- no final do valor (`121_` ou `121.05_`)
- ao lado do decimal (`10_.0`)
- ao lado do caractere expoente (`1.1e_1`)
- ao lado do especificador de tipo (`10_f`)
- imediatamente após o `0x` ou `0b` em literais binários e hexadecimais ([pode ser alterado para permitir, por exemplo, 0b_1001_1000][1])

[1]: https://github.com/dotnet/roslyn/issues/12680

## Literais binários
O prefixo **0b** pode ser usado para representar literais binários.

Os literais binários permitem construir números a partir de zeros e uns, o que torna muito mais fácil ver quais bits estão definidos na representação binária de um número. Isso pode ser útil para trabalhar com sinalizadores binários.

A seguir, maneiras equivalentes de especificar um `int` com valor `34` (=2<sup>5</sup> + 2<sup>1</sup>):

    // Using a binary literal:
    //   bits: 76543210
    int a1 = 0b00100010;          // binary: explicitly specify bits

    // Existing methods:
    int a2 = 0x22;                // hexadecimal: every digit corresponds to 4 bits
    int a3 = 34;                  // decimal: hard to visualise which bits are set
    int a4 = (1 << 5) | (1 << 1); // bitwise arithmetic: combining non-zero bits

# Sinaliza enumerações

Antes, especificar valores de sinalizador para um `enum` só podia ser feito usando um dos três métodos neste exemplo:

    [Flags]
    public enum DaysOfWeek
    {
        // Previously available methods:
        //          decimal        hex       bit shifting
        Monday    =  1,    //    = 0x01    = 1 << 0
        Tuesday   =  2,    //    = 0x02    = 1 << 1
        Wednesday =  4,    //    = 0x04    = 1 << 2
        Thursday  =  8,    //    = 0x08    = 1 << 3
        Friday    = 16,    //    = 0x10    = 1 << 4
        Saturday  = 32,    //    = 0x20    = 1 << 5
        Sunday    = 64,    //    = 0x40    = 1 << 6
    
        Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
        Weekends = Saturday | Sunday
    }

Com literais binários, é mais óbvio quais bits são definidos e usá-los não requer a compreensão de números hexadecimais e aritmética bit a bit:

    [Flags]
    public enum DaysOfWeek
    {
        Monday    = 0b00000001,
        Tuesday   = 0b00000010,
        Wednesday = 0b00000100,
        Thursday  = 0b00001000,
        Friday    = 0b00010000,
        Saturday  = 0b00100000,
        Sunday    = 0b01000000,
    
        Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
        Weekends = Saturday | Sunday
    }

## lançar expressões
O C# 7.0 permite lançar como uma expressão em determinados lugares:

    class Person
    {
        public string Name { get; }

        public Person(string name) => Name = name ?? throw new ArgumentNullException(nameof(name));

        public string GetFirstName()
        {
            var parts = Name.Split(' ');
            return (parts.Length > 0) ? parts[0] : throw new InvalidOperationException("No name!");
        }

        public string GetLastName() => throw new NotImplementedException();
    }


Antes do C# 7.0, se você quisesse lançar uma exceção de um corpo de expressão, você teria que:

    var spoons = "dinner,desert,soup".Split(',');

    var spoonsArray = spoons.Length > 0 ? spoons : null;

    if (spoonsArray == null) 
    {
        throw new Exception("There are no spoons");
    }

Ou

    var spoonsArray = spoons.Length > 0 
        ? spoons 
        : new Func<string[]>(() => 
          {
              throw new Exception("There are no spoons");
          })();

No C# 7.0, o acima agora é simplificado para:

    var spoonsArray = spoons.Length > 0 ? spoons : throw new Exception("There are no spoons");



## Lista de membros com corpo de expressão estendida
O C# 7.0 adiciona acessadores, construtores e finalizadores à lista de coisas que podem ter corpos de expressão:

    class Person
    {
        private static ConcurrentDictionary<int, string> names = new ConcurrentDictionary<int, string>();

        private int id = GetId();

        public Person(string name) => names.TryAdd(id, name); // constructors

        ~Person() => names.TryRemove(id, out _);              // finalizers

        public string Name
        {
            get => names[id];                                 // getters
            set => names[id] = value;                         // setters
        }
    }

Consulte também a seção [out var statement][1] para o operador de descarte.

[1]: https://www.wikiod.com/pt/docs/c%23/1936/c-sharp-7-0-features/6326/out-var-declaration

## ref return e ref local
Retornos de referência e locais de referência são úteis para manipular e retornar referências a blocos de memória em vez de copiar memória sem recorrer a ponteiros inseguros.

#Retorno de referência

    public static ref TValue Choose<TValue>(
        Func<bool> condition, ref TValue left, ref TValue right)
    {
        return condition() ? ref left : ref right;
    }

Com isso você pode passar dois valores por referência com um deles sendo retornado com base em alguma condição:

    Matrix3D left = …, right = …;
    Choose(chooser, ref left, ref right).M20 = 1.0;


# Referência Local

    public static ref int Max(ref int first, ref int second, ref int third)
    {
        ref int max = first > second ? ref first : ref second;
        return max > third ? ref max : ref third;
    }
    …
    int a = 1, b = 2, c = 3;
    Max(ref a, ref b, ref c) = 4;
    Debug.Assert(a == 1); // true
    Debug.Assert(b == 2); // true
    Debug.Assert(c == 4); // true

# Operações de referência inseguras
Em `System.Runtime.CompilerServices.Unsafe` foi definido um conjunto de operações inseguras que permitem que você manipule valores `ref` como se fossem ponteiros, basicamente.

Por exemplo, reinterpretando um endereço de memória (`ref`) como um tipo diferente:

    byte[] b = new byte[4] { 0x42, 0x42, 0x42, 0x42 };
    
    ref int r = ref Unsafe.As<byte, int>(ref b[0]);
    Assert.Equal(0x42424242, r);
    
    0x0EF00EF0;
    Assert.Equal(0xFE, b[0] | b[1] | b[2] | b[3]);

Cuidado com [endianness][1] ao fazer isso, por exemplo, verifique `BitConverter.IsLittleEndian` se necessário e trate de acordo.

Ou itere sobre uma matriz de maneira insegura:

    int[] a = new int[] { 0x123, 0x234, 0x345, 0x456 };
    
    ref int r1 = ref Unsafe.Add(ref a[0], 1);
    Assert.Equal(0x234, r1);

    ref int r2 = ref Unsafe.Add(ref r1, 2);
    Assert.Equal(0x456, r2);

    ref int r3 = ref Unsafe.Add(ref r2, -3);
    Assert.Equal(0x123, r3);

Ou o similar `Subtrair`:

    string[] a = new string[] { "abc", "def", "ghi", "jkl" };
    
    ref string r1 = ref Unsafe.Subtract(ref a[0], -2);
    Assert.Equal("ghi", r1);
    
    ref string r2 = ref Unsafe.Subtract(ref r1, -1);
    Assert.Equal("jkl", r2);
    
    ref string r3 = ref Unsafe.Subtract(ref r2, 3);
    Assert.Equal("abc", r3);

Além disso, pode-se verificar se dois valores `ref` são os mesmos, ou seja, mesmo endereço:

    long[] a = new long[2];
    
    Assert.True(Unsafe.AreSame(ref a[0], ref a[0]));
    Assert.False(Unsafe.AreSame(ref a[0], ref a[1]));

# Links
[Problema Roslyn Github][2]

[System.Runtime.CompilerServices.Unsafe no github][3]


[1]: https://en.wikipedia.org/wiki/Endianness
[2]: https://github.com/dotnet/roslyn/issues/118
[3]: https://github.com/dotnet/corefx/tree/master/src/System.Runtime.CompilerServices.Unsafe

## Tarefa de Valor<T>
`Task<T>` é uma **classe** e causa a sobrecarga desnecessária de sua alocação quando o resultado fica imediatamente disponível.

`ValueTask<T>` é uma **estrutura** e foi introduzida para evitar a alocação de um objeto `Task` caso o resultado da operação **async** já esteja disponível no momento da espera.

Portanto, `ValueTask<T>` oferece dois benefícios:

# 1. Aumento de desempenho

Aqui está um exemplo de `Task<T>`:
- Requer alocação de heap
- Leva 120ns com JIT


    async Task<int> TestTask(int d)
    {
        await Task.Delay(d);
        return 10;
    }

Aqui está o exemplo analógico `ValueTask<T>`:
- Nenhuma alocação de heap se o resultado for conhecido de forma síncrona (o que não é neste caso por causa do `Task.Delay`, mas geralmente é em muitos cenários `async`/`await` do mundo real)
- Leva 65ns com JIT


    async ValueTask<int> TestValueTask(int d)
    {
        await Task.Delay(d);
        return 10;
    }

# 2. Maior flexibilidade de implementação

Implementações de uma interface assíncrona que desejam ser síncronas seriam forçadas a usar `Task.Run` ou `Task.FromResult` (resultando na penalidade de desempenho discutida acima). Assim, há alguma pressão contra implementações síncronas.

Mas com `ValueTask<T>`, as implementações são mais livres para escolher entre serem síncronas ou assíncronas sem afetar os chamadores.

Por exemplo, aqui está uma interface com um método assíncrono:

    interface IFoo<T>
    {
        ValueTask<T> BarAsync();
    }

... e aqui está como esse método pode ser chamado:

    IFoo<T> thing = getThing();
    var x = await thing.BarAsync();

Com `ValueTask`, o código acima funcionará com **implementações síncronas ou assíncronas**:

## Implementação síncrona:

    class SynchronousFoo<T> : IFoo<T>
    {
        public ValueTask<T> BarAsync()
        {
            var value = default(T);
            return new ValueTask<T>(value);
        }
    }

## Implementação assíncrona

    class AsynchronousFoo<T> : IFoo<T>
    {
        public async ValueTask<T> BarAsync()
        {
            var value = default(T);
            await Task.Delay(1);
            return value;
        }
    }

# Notas

Embora a estrutura `ValueTask` estivesse sendo planejada para ser adicionada ao [C# 7.0][1], ela foi mantida como outra biblioteca por enquanto.
https://www.wikiod.com/pt/docs/c%23/1936/c-sharp-7-0-features/28612/valuetaskt#
O pacote `System.Threading.Tasks.Extensions` pode ser baixado da [Nuget Gallery](https://www.nuget.org/packages/System.Threading.Tasks.Extensions/)

[1]: https://blogs.msdn.microsoft.com/dotnet/2016/08/24/whats-new-in-csharp-7-0/

