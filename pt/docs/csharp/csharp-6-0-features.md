---
title: "Recursos do C# 6.0"
slug: "recursos-do-c-60"
draft: false
images: []
weight: 994
type: docs
toc: true
---

Esta sexta iteração da linguagem C# é fornecida pelo compilador Roslyn. Esse compilador foi lançado com a versão 4.6 do .NET Framework, mas pode gerar código de maneira compatível com versões anteriores para permitir o direcionamento de versões anteriores do framework. O código C# versão 6 pode ser compilado de maneira totalmente compatível com versões anteriores do .NET 4.0. Ele também pode ser usado para estruturas anteriores, no entanto, alguns recursos que exigem suporte de estrutura adicional podem não funcionar corretamente.

A sexta versão do C# foi lançada em julho de 2015 junto com o Visual Studio 2015 e o .NET 4.6.

Além de adicionar alguns novos recursos de linguagem, inclui uma reescrita completa do compilador. Anteriormente `csc.exe` era um aplicativo Win32 nativo escrito em C++, com C# 6 agora é um aplicativo gerenciado .NET escrito em C#. Essa reescrita ficou conhecida como projeto "Roslyn" e o código agora é de código aberto e está disponível no [GitHub][1].


[1]: https://github.com/dotnet/roslyn

## Filtros de exceção
<!-- language-all: lang-cs -->
[Filtros de exceção][1] dão aos desenvolvedores a capacidade de adicionar uma condição (na forma de uma expressão `booleana`) a um bloco [catch][2], permitindo que o `catch` seja executado somente se a condição for avaliada como ` verdadeiro`.

Filtros de exceção permitem a propagação de informações de depuração na exceção original, onde usar uma instrução `if` dentro de um bloco `catch` e relançar a exceção interrompe a propagação de informações de depuração na exceção original. Com filtros de exceção, a exceção continua a se propagar para cima na pilha de chamadas *a menos que* a condição seja atendida. Como resultado, os filtros de exceção tornam a experiência de depuração muito mais fácil. Em vez de parar na instrução `throw`, o depurador irá parar na instrução lançando a exceção, com o estado atual e todas as variáveis ​​locais preservadas. Os despejos de memória são afetados de maneira semelhante.

>Os filtros de exceção têm sido suportados pelo [**CLR**][3] desde o início e estão acessíveis a partir do VB.NET e F# há mais de uma década, expondo uma parte do modelo de tratamento de exceção do CLR. Somente após o lançamento do C# 6.0 a funcionalidade também ficou disponível para desenvolvedores de C#.
---

Usando filtros de exceção
-

Filtros de exceção são utilizados anexando uma cláusula `when` à expressão `catch`. É possível usar qualquer expressão que retorne um `bool` em uma cláusula `when` (exceto [await][4]). A variável Exception declarada `ex` é acessível a partir da cláusula `when`:

    var SqlErrorToIgnore = 123;
    try
    {
        DoSQLOperations();
    }
    catch (SqlException ex) when (ex.Number != SqlErrorToIgnore)
    {
        throw new Exception("An error occurred accessing the database", ex);
    }

Vários blocos `catch` com cláusulas `when` podem ser combinados. A primeira cláusula `when` retornando `true` fará com que a exceção seja capturada. Seu bloco `catch` será inserido, enquanto as outras cláusulas `catch` serão ignoradas (suas cláusulas `when` não serão avaliadas). Por exemplo:

    try
    { ... }
    catch (Exception ex) when (someCondition) //If someCondition evaluates to true,
                                              //the rest of the catches are ignored.
    { ... }
    catch (NotImplementedException ex) when (someMethod()) //someMethod() will only run if
                                                           //someCondition evaluates to false
    { ... }
    catch(Exception ex) // If both when clauses evaluate to false
    { ... }

---
Cláusula quando arriscada
-

>**Cuidado**
>
>Pode ser arriscado usar filtros de exceção: quando uma `Exception` é lançada de dentro da cláusula `when`, a `Exception` da cláusula `when` é ignorada e é tratada como `false`. Essa abordagem permite que os desenvolvedores escrevam a cláusula `when` sem cuidar de casos inválidos.

O exemplo a seguir ilustra esse cenário:

    public static void Main()
    {
        int a = 7;
        int b = 0;
        try
        {
            DoSomethingThatMightFail();
        }
        catch (Exception ex) when (a / b == 0)
        {
            // This block is never reached because a / b throws an ignored
            // DivideByZeroException which is treated as false.
        }
        catch (Exception ex)
        {
            // This block is reached since the DivideByZeroException in the 
            // previous when clause is ignored.
        }
    }

    public static void DoSomethingThatMightFail()
    {
        // This will always throw an ArgumentNullException.
        Type.GetType(null);
    }

[Ver demonstração][5]

Observe que os filtros de exceção evitam os problemas confusos de número de linha associados ao uso de `throw` quando o código com falha está dentro da mesma função. Por exemplo, neste caso, o número da linha é relatado como 6 em vez de 3:

    1. int a = 0, b = 0;
    2. try {
    3.     int c = a / b;
    4. }
    5. catch (DivideByZeroException) {
    6.     throw;
    7. }

O número da linha de exceção é relatado como 6 porque o erro foi detectado e relançado com a instrução `throw` na linha 6.

O mesmo não acontece com os filtros de exceção:

    1. int a = 0, b = 0;
    2. try {
    3.     int c = a / b;
    4. }
    5. catch (DivideByZeroException) when (a != 0) {
    6.     throw;
    7. }

Neste exemplo, `a` é 0, então a cláusula `catch` é ignorada, mas 3 é relatado como número de linha. Isso ocorre porque eles **não desenrolam a pilha**. Mais especificamente, a exceção *não é capturada* na linha 5 porque `a` de fato é igual a `0` e, portanto, não há oportunidade para a exceção ser relançada na linha 6 porque a linha 6 não é executada.

---

Registro como efeito colateral
-

Chamadas de método na condição podem causar efeitos colaterais, portanto, filtros de exceção podem ser usados ​​para executar código em exceções sem capturá-las. Um exemplo comum que tira vantagem disso é um método `Log` que sempre retorna `false`. Isso permite rastrear informações de log durante a depuração sem a necessidade de relançar a exceção.

>**Esteja ciente de que**, embora pareça ser uma maneira confortável de registrar, pode ser arriscado, especialmente se forem usados ​​conjuntos de registro de terceiros. Eles podem lançar exceções durante o login em situações não óbvias que podem não ser detectadas facilmente (consulte **cláusula `when(...)` de risco** acima).

<pre><code>tente
{
    DoSomethingThatMightFail(s);
}
catch (Exceção ex) <b>quando</b> (Log(ex, "Ocorreu um erro"))
{
    // This catch block will never be reached
}

// ...

static bool Log(Exception ex, string message, params object[] args)
{
    Debug.Print(message, args);
    return false;
}</code></pre>

[Ver demonstração][6]

A abordagem comum nas versões anteriores do C# era registrar e relançar a exceção.

<!-- if versão [lt 6.0] -->
    try
    {
        DoSomethingThatMightFail(s);
    }
    catch (Exception ex)
    {
         Log(ex, "An error occurred");
         throw;
    }

    // ...

    static void Log(Exception ex, string message, params object[] args)
    {
        Debug.Print(message, args);
    }

[Ver demonstração][7]
<!-- versão final if -->

---

O bloco `finalmente`
=

O bloco [`finally`][8] é executado sempre que a exceção é lançada ou não. Uma sutileza com expressões em `when` é que os filtros de exceção são executados mais acima na pilha *antes* de entrar nos blocos internos `finally`. Isso pode causar resultados e comportamentos inesperados quando o código tenta modificar o estado global (como o usuário ou a cultura do thread atual) e defini-lo de volta em um bloco `finally`.

Exemplo: bloco `finally`
-

    private static bool Flag = false;

    static void Main(string[] args)
    {
        Console.WriteLine("Start");
        try
        {
            SomeOperation();
        }
        catch (Exception) when (EvaluatesTo())
        {
            Console.WriteLine("Catch");
        }
        finally
        {
            Console.WriteLine("Outer Finally");
        }
    }

    private static bool EvaluatesTo()
    {
        Console.WriteLine($"EvaluatesTo: {Flag}");
        return true;
    }

    private static void SomeOperation()
    {
        try
        {
            Flag = true;
            throw new Exception("Boom");
        }
        finally
        {
            Flag = false;
            Console.WriteLine("Inner Finally");
        }
    }

Saída Produzida:

>Iniciar
Avalia para: Verdadeiro
Finalmente Interior
Truque
Finalmente exterior

[Ver demonstração][9]

No exemplo acima, se o método `SomeOperation` não deseja "vazar" as mudanças de estado global para as cláusulas `when` do chamador, ele também deve conter um bloco `catch` para modificar o estado. Por exemplo:

    private static void SomeOperation()
    {
        try
        {
            Flag = true;
            throw new Exception("Boom");
        }
        catch
        {
           Flag = false;
           throw;
        }
        finally
        {
            Flag = false;
            Console.WriteLine("Inner Finally");
        }
    }

Também é comum ver classes auxiliares [`IDisposable`][10] alavancando a semântica de [using][11] blocos para atingir o mesmo objetivo, já que `IDisposable.Dispose` sempre será chamado antes de uma exceção chamada dentro de um ` using` block começa a borbulhar na pilha.


[1]: https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6#exception-filters
[2]: https://www.wikiod.com/pt/docs/c%23/26/keywords/148/try-catch-finally-throw
[3]: https://msdn.microsoft.com/en-us/library/8bs2ecf4(v=vs.110).aspx
[4]: https://www.wikiod.com/pt/docs/c%23/26/keywords/5993/async-await
[5]: https://dotnetfiddle.net/Iex6DP
[6]: https://dotnetfiddle.net/pqPc7B
[7]: https://dotnetfiddle.net/kEWLue
[8]: https://www.wikiod.com/pt/docs/c%23/40/exception-handling/172/finally-block
[9]: https://ideone.com/gxfBA8
[10]: https://www.wikiod.com/pt/docs/c%23/1795/disposable-interface
[11]: https://www.wikiod.com/pt/docs/c%23/26/keywords/5062/using

## Interpolação de strings
A interpolação de strings permite que o desenvolvedor combine `variáveis` e texto para formar uma string.
___

# Exemplo básico

Duas variáveis ​​`int` são criadas: `foo` e `bar`.

    int foo = 34;
    int bar = 42;
    
    string resultString = $"The foo is {foo}, and the bar is {bar}.";

    Console.WriteLine(resultString);

**Resultado**:
>O foo é 34 e o bar é 42.

[Ver demonstração][3]

Chaves dentro de strings ainda podem ser usadas, assim:
<pre><code>var foo = 34;
onde barra = 42;

// Notação de interpolação de strings (novo estilo)
Console.WriteLine($"O foo é <b>{{foo}}</b> e a barra é <b>{{bar}}</b>.");</code></pre>

Isso produz a seguinte saída:

>O foo é {foo}, e a barra é {bar}.

___

# Usando interpolação com literais de string literais

Usar `@` antes da string fará com que a string seja interpretada literalmente. Assim, por exemplo Caracteres Unicode ou quebras de linha permanecerão exatamente como foram digitados. No entanto, isso não afetará as expressões em uma string interpolada, conforme mostrado no exemplo a seguir:<pre><code>Console.WriteLine($@"Caso não tenha ficado claro:
\u00B9
O foo
é <b>{foo}</b>,
e a barra
é <b>{bar}</b>.");</code></pre>
Resultado:
>Caso não tenha ficado claro:
\u00B9
O foo
é 34,
e a barra
é 42.

[Ver demonstração][4]

___

# Expressões
Com a interpolação de strings, *expressões* dentro de chaves `{}` também podem ser avaliadas. O resultado será inserido no local correspondente dentro da string. Por exemplo, para calcular o máximo de `foo` e `bar` e inseri-lo, use `Math.Max` dentro das chaves:<pre><code>Console.WriteLine($"E o maior é: <b >{ Math.Max(foo, bar) }</b>");</code></pre>

Resultado:

>E o maior é: 42

*Observação: qualquer espaço em branco à esquerda ou à direita (incluindo espaço, tabulação e CRLF/nova linha) entre a chave e a expressão é completamente ignorado e não incluído na saída*

[Ver demonstração][5]

Como outro exemplo, as variáveis ​​podem ser formatadas como moeda:<pre><code>Console.WriteLine($"Foo formatado como moeda com 4 casas decimais: <b>{foo:c4}</b>");< /code></pre>

Resultado:

>Foo formatado como moeda com 4 casas decimais: $34.0000

[Ver demonstração][6]

Ou podem ser formatados como datas:<pre><code>Console.WriteLine($"Today is: <b>{DateTime.Today:dddd, MMMM dd - yyyy}</b>");</code>< /pré>

Resultado:

>Hoje é: segunda-feira, julho, 20 - 2015

[Ver demonstração][7]

Declarações com um operador [Condicional (Ternário)][8] também podem ser avaliadas dentro da interpolação. No entanto, eles devem ser colocados entre parênteses, já que os dois pontos são usados ​​para indicar a formatação conforme mostrado acima:

<pre><code>Console.WriteLine($"{(foo > bar ? "Foo é maior que bar!" : "Bar é maior que foo!")}");</code></pre>

Resultado:
>Bar é maior que foo!

[Ver demonstração][9]

Expressões condicionais e especificadores de formato podem ser misturados:

    Console.WriteLine($"Environment: {(Environment.Is64BitProcess ? 64 : 32):00'-bit'} process");

Resultado:

> Ambiente: processo de 32 bits

___

# Sequências de escape
Escapar caracteres de barra invertida (`\`) e aspas (`"`) funciona exatamente da mesma forma em strings interpoladas como em strings não interpoladas, tanto para literais de string literais quanto não literais:
<pre><code>Console.WriteLine($"Foo is: <b>{foo}</b>. Em uma string não literal, precisamos escapar \" e \\ com barras invertidas.");
Console.WriteLine($@"Foo is: <b>{foo}</b>. Em uma string literal, precisamos escapar "" com uma aspa extra, mas não precisamos escapar \");
</code></pre>

Resultado:
>Foo é 34. Em uma string não literal, precisamos escapar " e \ com barras invertidas.
Foo é 34. Em uma string literal, precisamos escapar " com uma aspa extra, mas não precisamos escapar \

Para incluir uma chave `{` ou `}` em uma string interpolada, use duas chaves `{{` ou `}}`:<pre><code>$"{{foo}} is: <b>{ foo}</b>"</code></pre>

Resultado:
>{foo} é: 34

[Ver demonstração][10]
___

# Tipo FormattableString
O tipo de uma expressão de interpolação de string `$"..."` [nem sempre][11] uma string simples. O compilador decide qual tipo atribuir dependendo do contexto:<pre><code>string s = $"hello, <b>{name}</b>";
System.FormattableString s = $"Olá, <b>{name}</b>";
System.IFormattable s = $"Olá, <b>{name}</b>";</code></pre>

Essa também é a ordem de preferência de tipo quando o compilador precisa escolher qual método sobrecarregado será chamado.

Um [novo tipo][12], `System.FormattableString`, representa uma string de formato composto, juntamente com os argumentos a serem formatados. Use isso para escrever aplicativos que lidam especificamente com os argumentos de interpolação:

    public void AddLogItem(FormattableString formattableString)
    {
        foreach (var arg in formattableString.GetArguments())
        {
            // do something to interpolation argument 'arg'
        }

        // use the standard interpolation and the current culture info
        // to get an ordinary String:
        var formatted = formattableString.ToString();

        // ...
    }
Chame o método acima com:<pre><code>AddLogItem($"O foo é <b>{foo}</b> e a barra é <b>{bar}</b>.");</ código></pre>
Por exemplo, pode-se optar por não incorrer no custo de desempenho da formatação da string se o nível de log já for filtrar o item de log.<hr>
# Conversões implícitas
Existem conversões de tipo implícitas de uma string interpolada:<pre><code>var s = $"Foo: <b>{foo}</b>";
System.IFormattable s = $"Foo: <b>{foo}</b>";</code></pre>
Você também pode produzir uma variável `IFormattable` que permite converter a string com contexto invariável:<pre><code>var s = $"Bar: <b>{bar}</b>";
System.FormattableString s = $"Bar: <b>{bar}</b>";</code></pre><hr>
# Métodos de Cultura Atual e Invariante
Se a análise de código estiver ativada, todas as strings interpoladas produzirão aviso [CA1305][13] (Especifique `IFormatProvider`).
Um método estático pode ser usado para aplicar a cultura atual.

    public static class Culture
    {
        public static string Current(FormattableString formattableString)
        {
            return formattableString?.ToString(CultureInfo.CurrentCulture);
        }
        public static string Invariant(FormattableString formattableString)
        {
            return formattableString?.ToString(CultureInfo.InvariantCulture);
        }
    }
Então, para produzir uma string correta para a cultura atual, basta usar a expressão:<pre><code>Culture.Current($"interpolated <b>{typeof(string).Name}</b> string.");
Culture.Invariant($"interpolado <b>{typeof(string).Name}</b> string.")</code></pre>
**Observação**: `Current` e `Invariant` não podem ser criados como métodos de extensão porque, por padrão, o compilador atribui o tipo `String` a *expressão de string interpolada*, o que faz com que o código a seguir não seja compilado:

    $"interpolated {typeof(string).Name} string.".Current();
A classe `FormattableString` já contém o método `Invariant()`, então a maneira mais simples de mudar para a cultura invariável é confiar em `usando static`:<pre><code>usando static System.FormattableString;

string invariant = Invariant($"Agora = <b>{DateTime.Now}</b>");
string atual = $"Agora = <b>{DateTime.Now}</b>";</code></pre><hr>
# Por trás das cenas
Strings interpoladas são apenas um açúcar sintático para `String.Format()`. O compilador ([Roslyn][14]) irá transformá-lo em um `String.Format` nos bastidores:

    var text = $"Hello {name + lastName}";
    
O acima será convertido para algo assim:

    string text = string.Format("Hello {0}", new object[] {
        name + lastName
    });
<hr>

# Interpolação de String e Linq

É possível usar strings interpoladas em instruções Linq para aumentar ainda mais a legibilidade.

    var fooBar = (from DataRow x in fooBarTable.Rows
              select string.Format("{0}{1}", x["foo"], x["bar"])).ToList();

Pode ser reescrito como:

    var fooBar = (from DataRow x in fooBarTable.Rows
              select $"{x["foo"]}{x["bar"]}").ToList();

# Strings Interpoladas Reutilizáveis
Com `string.Format`, você pode criar strings de formato reutilizáveis:

    public const string ErrorFormat = "Exception caught:\r\n{0}";

    // ...

    Logger.Log(string.Format(ErrorFormat, ex));

Strings interpoladas, no entanto, não serão compiladas com espaços reservados referentes a variáveis ​​inexistentes. O seguinte não irá compilar:

    public const string ErrorFormat = $"Exception caught:\r\n{error}";
    // CS0103: The name 'error' does not exist in the current context

Em vez disso, crie um `Func<>` que consome variáveis ​​e retorna uma `String`:

    public static Func<Exception, string> FormatError =
        error => $"Exception caught:\r\n{error}";

    // ...

    Logger.Log(FormatError(ex));
<hr>

# Interpolação e localização de strings

Se você estiver localizando seu aplicativo, pode se perguntar se é possível usar a interpolação de string junto com a localização. De fato, seria bom ter a possibilidade de armazenar em arquivos de recursos `String`s como:<pre><code>"Meu nome é <b>{name} {middlename} {surname}</b>"</ código></pre>
em vez do muito menos legível:

    "My name is {0} {1} {2}"
O processo de interpolação `String` ocorre *em tempo de compilação*, diferentemente da formatação de string com `string.Format` que ocorre *em tempo de execução*. As expressões em uma string interpolada devem fazer referência a nomes no contexto atual e precisam ser armazenadas em arquivos de recursos. Isso significa que, se você quiser usar a localização, terá que fazer assim:

    var FirstName = "John";
    
    // method using different resource file "strings"
    // for French ("strings.fr.resx"), German ("strings.de.resx"), 
    // and English ("strings.en.resx")
    void ShowMyNameLocalized(string name, string middlename = "", string surname = "")
    {
        // get localized string
        var localizedMyNameIs = Properties.strings.Hello;
        // insert spaces where necessary
        name = (string.IsNullOrWhiteSpace(name) ? "" : name + " ");
        middlename = (string.IsNullOrWhiteSpace(middlename) ? "" : middlename + " ");
        surname = (string.IsNullOrWhiteSpace(surname) ? "" : surname + " ");
        // display it
        Console.WriteLine($"{localizedMyNameIs} {name}{middlename}{surname}".Trim());
    }

    // switch to French and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("fr-FR");
    ShowMyNameLocalized(FirstName);

    // switch to German and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("de-DE");
    ShowMyNameLocalized(FirstName);

    // switch to US English and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("en-US");
    ShowMyNameLocalized(FirstName);

Se as strings de recursos para os idiomas usados ​​acima estiverem armazenadas corretamente nos arquivos de recursos individuais, você deverá obter a seguinte saída:
> Olá, meu nome é João<br/>
> Olá, meu nome é João<br/>
> Olá, meu nome é João<br/>

**Observe** que isso implica que o nome segue a string localizada em todos os idiomas. Se esse não for o caso, você precisará adicionar espaços reservados às cadeias de recursos e modificar a função acima ou precisará consultar as informações de cultura na função e fornecer uma instrução switch case contendo os diferentes casos.
Para obter mais detalhes sobre arquivos de recursos, consulte [Como usar a localização em C#](https://stackoverflow.com/a/1142840/1016343).

É uma boa prática usar um idioma de fallback padrão que a maioria das pessoas entenderá, caso uma tradução não esteja disponível. Sugiro usar o inglês como idioma de fallback padrão.

# Interpolação recursiva

Embora não seja muito útil, é permitido usar uma `string` interpolada recursivamente dentro das chaves de outra:

    Console.WriteLine($"String has {$"My class is called {nameof(MyClass)}.".Length} chars:");
    Console.WriteLine($"My class is called {nameof(MyClass)}.");

Resultado:

> String tem 27 caracteres:

> Minha classe se chama MinhaClasse.

[1]: https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6#string-interpolation
[2]: https://dotnetfiddle.net/0JjwL5
[3]: https://ideone.com/bRFOaV
[4]: https://dotnetfiddle.net/FLs4Ae
[5]: https://ideone.com/qY1Y4B
[6]: https://ideone.com/CPB8UJ
[7]: https://ideone.com/PkjA6k
[8]: https://msdn.microsoft.com/en-us/library/ty67wk28.aspx
[9]: https://ideone.com/sX6tO3
[10]: https://dotnetfiddle.net/BuudHP
[11]: http://stackoverflow.com/questions/38119074
[12]: https://msdn.microsoft.com/en-us/library/system.formattablestring(v=vs.110).aspx
[13]: https://msdn.microsoft.com/en-us/library/ms182190.aspx
[14]: https://github.com/dotnet/roslyn


## Inicializadores de propriedade automática
# Introdução

As propriedades podem ser inicializadas com o operador `=` após o fechamento `}`. A classe `Coordinate` abaixo mostra as opções disponíveis para inicializar uma propriedade:


<!-- if versão [gte 6.0] -->
    public class Coordinate
    { 
        public int X { get; set; } = 34; // get or set auto-property with initializer
   
        public int Y { get; } = 89;      // read-only auto-property with initializer              
    }
<!-- versão final if -->

---

## Acessores com Visibilidade Diferente

Você pode inicializar propriedades automáticas que têm visibilidade diferente em seus acessadores. Aqui está um exemplo com um setter protegido:

        public string Name { get; protected set; } = "Cheeze";

O acessador também pode ser `internal`, `internal protected` ou `private`.

---

## Propriedades somente leitura

Além da flexibilidade com visibilidade, você também pode inicializar propriedades automáticas somente leitura. Aqui está um exemplo:

        public List<string> Ingredients { get; } = 
            new List<string> { "dough", "sauce", "cheese" };

Este exemplo também mostra como inicializar uma propriedade com um tipo complexo. Além disso, as propriedades automáticas não podem ser somente gravação, o que também impede a inicialização somente gravação.

---

# Estilo antigo (pré C# 6.0)

Antes do C# 6, isso exigia um código muito mais detalhado. Estávamos usando uma variável extra chamada backing property para a propriedade fornecer o valor padrão ou inicializar a propriedade pública como abaixo,

<!-- if versão [lt 6.0] -->
    public class Coordinate
    {
        private int _x = 34;
        public int X { get { return _x; } set { _x = value; } }
   
        private readonly int _y = 89;
        public int Y { get { return _y; } }
        
        private readonly int _z;
        public int Z { get { return _z; } }
    
        public Coordinate()
        {
            _z = 42;
        }
    }

***Observação:** antes do C# 6.0, você ainda podia inicializar [**propriedades implementadas automaticamente**][2] (propriedades com um getter e um setter) de leitura e gravação de dentro do construtor, mas não era possível inicializar o propriedade alinhada com sua declaração *

[Ver demonstração][3]
<!-- versão final if -->

---

# Uso

Inicializadores devem avaliar expressões estáticas, assim como inicializadores de campo. Se você precisar fazer referência a membros não estáticos, poderá inicializar propriedades em construtores como antes ou usar propriedades com corpo de expressão. Expressões não estáticas, como a abaixo (comentada), gerarão um erro do compilador:

    
    // public decimal X { get; set; } = InitMe();  // generates compiler error

    decimal InitMe() { return 4m; }

Mas métodos estáticos **podem** ser usados ​​para inicializar propriedades automáticas:

    public class Rectangle
    {
        public double Length { get; set; } = 1;
        public double Width { get; set; } = 1;
        public double Area { get; set; } = CalculateArea(1, 1);

        public static double CalculateArea(double length, double width)
        {
            return length * width;
        }
    }

Este método também pode ser aplicado a propriedades com diferentes níveis de acessadores:

    public short Type { get; private set; } = 15;

O inicializador de propriedade automática permite a atribuição de propriedades diretamente em sua declaração. Para propriedades somente leitura, ele cuida de todos os requisitos necessários para garantir que a propriedade seja imutável. Considere, por exemplo, a classe `FingerPrint` no exemplo a seguir:

    public class FingerPrint
    {
      public DateTime TimeStamp { get; } = DateTime.UtcNow;

      public string User { get; } =
        System.Security.Principal.WindowsPrincipal.Current.Identity.Name;

      public string Process { get; } =
        System.Diagnostics.Process.GetCurrentProcess().ProcessName;
    }

[Ver demonstração][4]

---

# Notas de advertência

Tome cuidado para não confundir inicializadores de propriedade automática ou de campo com [métodos de corpo de expressão][5] de aparência semelhante que fazem uso de `=>` em oposição a `=`, e campos que não incluem `{ get; }`.

Por exemplo, cada uma das seguintes declarações são diferentes.

    public class UserGroupDto
    {
        // Read-only auto-property with initializer:       
        public ICollection<UserDto> Users1 { get; } = new HashSet<UserDto>();
        
        // Read-write field with initializer:
        public ICollection<UserDto> Users2 = new HashSet<UserDto>();

        // Read-only auto-property with expression body:
        public ICollection<UserDto> Users3 => new HashSet<UserDto>();
    }

Falta `{ get; }` na declaração de propriedade resulta em um campo público. Tanto a propriedade automática de somente leitura `Users1` quanto o campo de leitura/gravação `Users2` são inicializados apenas uma vez, mas um campo público permite alterar a instância da coleção de fora da classe, o que geralmente é indesejável. Alterar uma propriedade automática somente leitura com corpo de expressão para propriedade somente leitura com inicializador requer não apenas remover `>` de `=>`, mas adicionar `{ get; }`.

O símbolo diferente (`=>` em vez de `=`) em `Users3` resulta em cada acesso à propriedade retornando uma nova instância do `HashSet<UserDto>` que, enquanto C# válido (do ponto de vista do compilador) é improvável que seja o comportamento desejado quando usado para um membro da coleção.

O código acima é equivalente a:

    public class UserGroupDto
    {
        // This is a property returning the same instance
        // which was created when the UserGroupDto was instantiated.
        private ICollection<UserDto> _users1 = new HashSet<UserDto>();
        public ICollection<UserDto> Users1 { get { return _users1; } }

        // This is a field returning the same instance
        // which was created when the UserGroupDto was instantiated.
        public virtual ICollection<UserDto> Users2 = new HashSet<UserDto>();

        // This is a property which returns a new HashSet<UserDto> as
        // an ICollection<UserDto> on each call to it.
        public ICollection<UserDto> Users3 { get { return new HashSet<UserDto>(); } }
    }


[2]: https://www.wikiod.com/pt/docs/c%23/49/properties/3365/auto-implemented-properties#t=201608062134378589394
[3]: http://ideone.com/2OgrPQ
[4]: http://ideone.com/qjDRmx
[5]: https://www.wikiod.com/pt/docs/c%23/24/c-sharp-6-0-features/44/expression-bodied-function-members

## Propagação nula
O operador `?.` e o operador `?[...]` são chamados de [operador condicional nulo][1]. Às vezes, também é referido por outros nomes, como [operador de navegação segura] [2].

Isso é útil, porque se o operador `.` (acessador de membro) for aplicado a uma expressão avaliada como `null`, o programa lançará uma `NullReferenceException`. Se o desenvolvedor usar o operador `?.` (condicional nulo), a expressão será avaliada como nula em vez de lançar uma exceção.

Observe que se o operador `?.` for usado e a expressão não for nula, `?.` e `.` serão equivalentes.

---

# Fundamentos

    var teacherName = classroom.GetTeacher().Name;
    // throws NullReferenceException if GetTeacher() returns null

[Ver demonstração][3]

Se a `classroom` não tiver um professor, `GetTeacher()` pode retornar `null`. Quando for `null` e a propriedade `Name` for acessada, uma `NullReferenceException` será lançada.

Se modificarmos esta declaração para usar a sintaxe `?.`, o resultado de toda a expressão será `null`:

    var teacherName = classroom.GetTeacher()?.Name;
    // teacherName is null if GetTeacher() returns null

[Ver demonstração][4]

Subsequentemente, se `classroom` também pudesse ser `null`, também poderíamos escrever esta declaração como:

    var teacherName = classroom?.GetTeacher()?.Name;
    // teacherName is null if GetTeacher() returns null OR classroom is null

[Ver demonstração][5]

Este é um exemplo de curto-circuito: quando qualquer operação de acesso condicional usando o operador condicional nulo é avaliada como nula, a expressão inteira é avaliada como nula imediatamente, sem processar o restante da cadeia.

Quando o membro terminal de uma expressão que contém o operador condicional nulo é de um tipo de valor, a expressão é avaliada como um `Nullable<T>` desse tipo e, portanto, não pode ser usada como uma substituição direta da expressão sem `?.` .

    bool hasCertification = classroom.GetTeacher().HasCertification;
    // compiles without error but may throw a NullReferenceException at runtime

    bool hasCertification = classroom?.GetTeacher()?.HasCertification;
    // compile time error: implicit conversion from bool? to bool not allowed

    bool? hasCertification = classroom?.GetTeacher()?.HasCertification;
    // works just fine, hasCertification will be null if any part of the chain is null

    bool hasCertification = classroom?.GetTeacher()?.HasCertification.GetValueOrDefault();
    // must extract value from nullable to assign to a value type variable

---

# Use com o operador de coalescência nula (??)

Você pode combinar o operador condicional nulo com o [Operador de coalescência nula][6] (`??`) para retornar um valor padrão se a expressão resolver para `null`. Usando nosso exemplo acima:

    var teacherName = classroom?.GetTeacher()?.Name ?? "No Name";
    // teacherName will be "No Name" when GetTeacher() 
    // returns null OR classroom is null OR Name is null

---

# Use com indexadores

O operador condicional nulo pode ser usado com [indexers][7]:

    var firstStudentName = classroom?.Students?[0]?.Name;

No exemplo acima:

* O primeiro `?.` garante que `classroom` não seja `null`.
* O segundo `?` garante que toda a coleção `Students` não seja `null`.
* O terceiro `?.` após o indexador garante que o indexador `[0]` não retornou um objeto `null`. Deve-se notar que esta operação pode **ainda** lançar uma `IndexOutOfRangeException`.

---

# Use com funções void

O operador nulo-condicional também pode ser usado com funções `void`. No entanto, neste caso, a instrução não será avaliada como `null`. Ele apenas impedirá um `NullReferenceException`.

    List<string> list = null;
    list?.Add("hi");          // Does not evaluate to null


---

# Use com Invocação de Evento

Assumindo a seguinte definição de evento:

    private event EventArgs OnCompleted;

Ao invocar um evento, tradicionalmente, é uma prática recomendada verificar se o evento é `null` caso nenhum assinante esteja presente:

    var handler = OnCompleted;
    if (handler != null)
    {
        handler(EventArgs.Empty);
    }

Como o operador condicional nulo foi introduzido, a invocação pode ser reduzida a uma única linha:

    OnCompleted?.Invoke(EventArgs.Empty);

---

# Limitações

O operador condicional nulo produz rvalue, não lvalue, ou seja, não pode ser usado para atribuição de propriedades, assinatura de eventos etc. Por exemplo, o código a seguir não funcionará:

    // Error: The left-hand side of an assignment must be a variable, property or indexer
    Process.GetProcessById(1337)?.EnableRaisingEvents = true;
    // Error: The event can only appear on the left hand side of += or -=
    Process.GetProcessById(1337)?.Exited += OnProcessExited;

---

# Pegadinhas

Observe que:

    int? nameLength = person?.Name.Length;    // safe if 'person' is null

é __não__ o mesmo que:

    int? nameLength = (person?.Name).Length;  // avoid this

porque o primeiro corresponde a:

    int? nameLength = person != null ? (int?)person.Name.Length : null;

e este último corresponde a:

    int? nameLength = (person != null ? person.Name : null).Length;

Apesar do operador ternário `?:` ser usado aqui para explicar a diferença entre dois casos, esses operadores não são equivalentes. Isso pode ser facilmente demonstrado com o seguinte exemplo:

    void Main()
    {
        var foo = new Foo();
        Console.WriteLine("Null propagation");
        Console.WriteLine(foo.Bar?.Length);

        Console.WriteLine("Ternary");
        Console.WriteLine(foo.Bar != null ? foo.Bar.Length : (int?)null);
    }
    
    class Foo
    {
        public string Bar
        {
            get
            {
                Console.WriteLine("I was read");
                return string.Empty;
            }
        }
    }

Quais saídas:

>Propagação nula
>eu fui lido
>0
>Ternário
>eu fui lido
>eu fui lido
>0

[Ver demonstração][8]

Para evitar várias invocações equivalente seria:

    var interimResult = foo.Bar;
    Console.WriteLine(interimResult != null ? interimResult.Length : (int?)null);

E essa diferença explica um pouco por que o operador de propagação nulo é [ainda não suportado][9] em árvores de expressão.


[1]: https://msdn.microsoft.com/en-us/library/dn986595.aspx
[2]: https://en.wikipedia.org/wiki/Safe_navigation_operator
[3]: http://ideone.com/p8OGBB
[4]: http://ideone.com/3aqGlE
[5]: http://ideone.com/voljZh
[6]: https://msdn.microsoft.com/en-us/library/ms173224.aspx
[7]: https://msdn.microsoft.com/en-us/library/6x16t2tx.aspx
[8]: https://dotnetfiddle.net/BytXEz
[9]: https://roslyn.codeplex.com/discussions/571077


## Membros de função com corpo de expressão
Membros de função com corpo de expressão permitem o uso de expressões lambda como corpos de membro. Para membros simples, pode resultar em um código mais limpo e legível.

Funções de expressão podem ser usadas para propriedades, indexadores, métodos e operadores.

---

# Propriedades

    public decimal TotalPrice => BasePrice + Taxes;

É equivalente a:

    public decimal TotalPrice
    {
        get
        {
            return BasePrice + Taxes;
        }
    }

Quando uma função com corpo de expressão é usada com uma propriedade, a propriedade é implementada como uma propriedade somente getter.

[Ver demonstração][1]

---

# Indexadores

    public object this[string key] => dictionary[key];

É equivalente a:

    public object this[string key]
    {
        get
        {
            return dictionary[key];
        }
    }

---

# Métodos

    static int Multiply(int a, int b) => a * b;

É equivalente a:

    static int Multiply(int a, int b)
    {
        return a * b;
    }

Que também pode ser usado com métodos `void`:

    public void Dispose() => resource?.Dispose();

Uma substituição de `ToString` pode ser adicionada à classe `Pair<T>`:

    public override string ToString() => $"{First}, {Second}";

Além disso, essa abordagem simplista funciona com a palavra-chave `override`:

    public class Foo
    {
        public int Bar { get; }
    
        public string override ToString() => $"Bar: {Bar}";
    }

---

# Operadores

Isso também pode ser usado pelos operadores:

    public class Land
    {
        public double Area { get; set; }

        public static Land operator +(Land first, Land second) =>
            new Land { Area = first.Area + second.Area };
    }

---

# Limitações

Membros de função com corpo de expressão têm algumas limitações. Eles não podem conter instruções de bloco e quaisquer outras instruções que contenham blocos: `if`, `switch`, `for`, `foreach`, `while`, `do`, `try`, etc.

Algumas instruções `if` podem ser substituídas por operadores ternários. Algumas instruções `for` e `foreach` podem ser convertidas em consultas LINQ, por exemplo:

    IEnumerable<string> Digits
    {
        get
        {
            for (int i = 0; i < 10; i++)
                yield return i.ToString();
        }
    }

<!---->

    IEnumerable<string> Digits => Enumerable.Range(0, 10).Select(i => i.ToString());

Em todos os outros casos, a sintaxe antiga para membros de função pode ser usada.

Os membros da função de expressão podem conter `async`/`await`, mas geralmente é redundante:

    async Task<int> Foo() => await Bar();  

Pode ser substituído por:

    Task<int> Foo() => Bar();

[1]: https://dotnetfiddle.net/djFd7O


## Nome do operador
O operador `nameof` retorna o nome de um elemento de código como uma `string`. Isso é útil ao lançar exceções relacionadas a argumentos de método e também ao implementar `INotifyPropertyChanged`.

    public string SayHello(string greeted)
    {
        if (greeted == null)
            throw new ArgumentNullException(nameof(greeted));
        
        Console.WriteLine("Hello, " + greeted);
    }

O operador `nameof` é avaliado em tempo de compilação e transforma a expressão em uma string literal. Isso também é útil para strings que são nomeadas de acordo com o membro que as expõe. Considere o seguinte:

    public static class Strings
    {
        public const string Foo = nameof(Foo); // Rather than Foo = "Foo"
        public const string Bar = nameof(Bar); // Rather than Bar = "Bar"
    }

Como as expressões `nameof` são constantes de tempo de compilação, elas podem ser usadas em atributos, rótulos `case`, instruções `switch` e assim por diante.

<hr/>

É conveniente usar `nameof` com `Enum`s. Ao invés de:

    Console.WriteLine(Enum.One.ToString());

é possível usar:

    Console.WriteLine(nameof(Enum.One))

A saída será `One` em ambos os casos.

<hr/>

O operador `nameof` pode acessar membros não estáticos usando sintaxe estática. Em vez de fazer:

    string foo = "Foo";
    string lengthName = nameof(foo.Length);

Pode ser substituído por:

    string lengthName = nameof(string.Length);

A saída será `Length` em ambos os exemplos. No entanto, este último impede a criação de instâncias desnecessárias.

<hr/>

Embora o operador `nameof` funcione com a maioria das construções de linguagem, existem algumas limitações. Por exemplo, você não pode usar o operador `nameof` em tipos genéricos abertos ou valores de retorno de método:

    public static int Main()
    {   
        Console.WriteLine(nameof(List<>)); // Compile-time error
        Console.WriteLine(nameof(Main())); // Compile-time error
    }

Além disso, se você aplicá-lo a um tipo genérico, o parâmetro de tipo genérico será ignorado:

    Console.WriteLine(nameof(List<int>));  // "List"
    Console.WriteLine(nameof(List<bool>)); // "List"

Para mais exemplos, veja [este tópico][1] dedicado a `nameof`.

<hr/>

# Solução alternativa para versões anteriores ([mais detalhes][2])

Embora o operador `nameof` não exista em C# para versões anteriores a 6.0, uma funcionalidade semelhante pode ser obtida usando `MemberExpression` como no seguinte:

<!-- if versão [lt 6.0] -->
Expressão:

    public static string NameOf<T>(Expression<Func<T>> propExp)
    {
        var memberExpression = propExp.Body as MemberExpression;
        return memberExpression != null ? memberExpression.Member.Name : null;
    }

    public static string NameOf<TObj, T>(Expression<Func<TObj, T>> propExp)
    {
        var memberExpression = propExp.Body as MemberExpression;
        return memberExpression != null ? memberExpression.Member.Name : null;
    }

Uso:

    string variableName = NameOf(() => variable);
    string propertyName = NameOf((Foo o) => o.Bar);

<!-- versão final if -->

Note que esta abordagem faz com que uma árvore de expressão seja criada em cada chamada, então o desempenho é muito pior comparado ao operador `nameof` que é avaliado em tempo de compilação e tem zero overhead em tempo de execução.


[1]: https://www.wikiod.com/pt/docs/c%23/80/nameof-operator#t=201608031424500177545
[2]: https://www.wikiod.com/pt/docs/c%23/80/nameof-operator/26157/name-of-extension-support-added-for-before-c-sharp-6-version#t= 201612071107472552734

## Usando tipo estático
A diretiva `using static [Namespace.Type]` permite a importação de membros estáticos de tipos e valores de enumeração. Os métodos de extensão são importados como métodos de extensão (de apenas um tipo), não no escopo de nível superior.

<!-- if versão [gte 6.0] -->

    using static System.Console;
    using static System.ConsoleColor;
    using static System.Math;
    
    class Program
    {
        static void Main()
        {
            BackgroundColor = DarkBlue;
            WriteLine(Sqrt(2));
        }
    }

[Fiddle de demonstração ao vivo][1]
<!-- versão final if -->

<!-- if versão [lt 6.0] -->

    using System;
    
    class Program
    {
        static void Main()
        {
            Console.BackgroundColor = ConsoleColor.DarkBlue;
            Console.WriteLine(Math.Sqrt(2));
        }
    }

<!-- versão final if -->


[1]: https://dotnetfiddle.net/7Ll3XN

## Inicializadores de índice
Os inicializadores de índice possibilitam criar e inicializar objetos com índices ao mesmo tempo.

Isso torna a inicialização de dicionários muito fácil:

    var dict = new Dictionary<string, int>()
    {
        ["foo"] = 34,
        ["bar"] = 42
    };


Qualquer objeto que tenha um getter ou setter indexado pode ser usado com esta sintaxe:

    class Program
    {
        public class MyClassWithIndexer
        {
            public int this[string index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
        }

        public static void Main()
        {
            var x = new MyClassWithIndexer()
            {
                ["foo"] = 34,
                ["bar"] = 42
            };

            Console.ReadKey();
        }
    }

Resultado:
>Índice: foo, valor: 34
> Índice: barra, valor: 42


[Ver demonstração][1]

Se a classe tiver vários indexadores, é possível atribuir todos eles em um único grupo de instruções:

    class Program
    {
        public class MyClassWithIndexer
        {
            public int this[string index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
            public string this[int index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
        }

        public static void Main()
        {
            var x = new MyClassWithIndexer()
            {
                ["foo"] = 34,
                ["bar"] = 42,
                [10] = "Ten",
                [42] = "Meaning of life"
            };
        }
    }

Resultado:
>Índice: foo, valor: 34
>Índice: barra, valor: 42
> Índice: 10, valor: dez
>Índice: 42, valor: Sentido da vida

Deve-se notar que o acessador `set` do indexador pode se comportar de forma diferente em comparação com um método `Add` (usado em inicializadores de coleção).

Por exemplo:

    var d = new Dictionary<string, int>
    {
        ["foo"] = 34,
        ["foo"] = 42,
    }; // does not throw, second value overwrites the first one

contra:

    var d = new Dictionary<string, int>
    {
        { "foo", 34 },
        { "foo", 42 },
    }; // run-time ArgumentException: An item with the same key has already been added.


[1]: https://dotnetfiddle.net/Evs4Qx

## Resolução de sobrecarga aprimorada
O trecho a seguir mostra um exemplo de passagem de um grupo de métodos (em oposição a um lambda) quando um delegado é esperado. A resolução de sobrecarga agora resolverá isso em vez de gerar um erro de sobrecarga ambíguo devido à capacidade do **C# 6** de verificar o tipo de retorno do método que foi passado.

    using System;
    public class Program
    {
        public static void Main()
        {
            Overloaded(DoSomething);
        }
    
        static void Overloaded(Action action)
        {
           Console.WriteLine("overload with action called");
        }
    
        static void Overloaded(Func<int> function)
        {
           Console.WriteLine("overload with Func<int> called");
        }
    
        static int DoSomething()
        {
            Console.WriteLine(0);
            return 0;
        }
    }

Resultados:

<!-- if versão [eq 6.0] -->
**Resultado**
>sobrecarregar com Func\<int\> chamado

[Ver demonstração][1]
<!-- versão final if -->

<!-- if versão [eq 5.0] -->
**Erro**
> erro CS0121: A chamada é ambígua entre os seguintes métodos ou propriedades:
     'Program.Overloaded(System.Action)' and 'Program.Overloaded(System.Func)'

<!-- versão final if -->

**C# 6** também pode lidar bem com o seguinte caso de correspondência exata para expressões lambda que resultariam em um erro em **C# 5**.

    using System;

    class Program
    {
        static void Foo(Func<Func<long>> func) {}
        static void Foo(Func<Func<int>> func) {}

        static void Main()
        {
            Foo(() => () => 7);
        }
    }


[1]: https://dotnetfiddle.net/Vnudqy

## Aguarde na captura e finalmente
É possível usar a expressão `await` para aplicar [await operator][1] a [Tasks][2] ou [Task(Of TResult)][3] nos blocos `catch` e `finally` em C#6 .

Não era possível usar a expressão `await` nos blocos `catch` e `finally` em versões anteriores devido a limitações do compilador. O C#6 facilita muito a espera de tarefas assíncronas, permitindo a expressão `await`.

    try
    {
        //since C#5
        await service.InitializeAsync();
    } 
    catch (Exception e)
    {
        //since C#6
        await logger.LogAsync(e);
    }
    finally
    {
        //since C#6
        await service.CloseAsync();
    }

Era necessário em C# 5 usar um `bool` ou declarar uma `Exception` fora do try catch para realizar operações assíncronas. Esse método é mostrado no exemplo a seguir:
    
    bool error = false;
    Exception ex = null;

    try
    {
        // Since C#5
        await service.InitializeAsync();
    } 
    catch (Exception e)
    {
        // Declare bool or place exception inside variable
        error = true;
        ex = e;
    }

    // If you don't use the exception
    if (error)
    {
        // Handle async task
    }

    // If want to use information from the exception
    if (ex != null)
    {
        await logger.LogAsync(e);
    }    

    // Close the service, since this isn't possible in the finally
    await service.CloseAsync();


[1]: https://msdn.microsoft.com/en-us/library/hh156528.aspx
[2]: https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.aspx
[3]: https://msdn.microsoft.com/en-us/library/dd321424.aspx

## Pequenas alterações e correções de bugs
Parênteses agora são proibidos em torno de parâmetros nomeados. O seguinte compila em C#5, mas não em C#6

<!-- if versão [lte 5.0] -->

    Console.WriteLine((value: 23));

<!-- versão final if -->

Operandos de `is` e `as` não podem mais ser grupos de métodos. O seguinte compila em C#5, mas não em C#6

<!-- if versão [lte 5.0] -->

    var result = "".Any is byte;

> O compilador nativo permitiu isso (embora tenha mostrado um aviso), e na verdade nem verificou a compatibilidade do método de extensão, permitindo coisas malucas como `1.Any is string` ou `IDisposable.Dispose is object`.

<!-- versão final if -->

Consulte [esta referência][1] para atualizações sobre alterações.


[1]: http://blog.slaks.net/2014-05-28/exploring-roslyn-part-3-breaking-changes/

## Usando um método de extensão para inicialização de coleção
A sintaxe de inicialização de coleção pode ser usada ao instanciar qualquer classe que implemente `IEnumerable` e tenha um método chamado `Add` que recebe um único parâmetro.

Nas versões anteriores, esse método `Add` tinha que ser um método de **instância** na classe que estava sendo inicializada. Em C#6, também pode ser um método de extensão.

    public class CollectionWithAdd : IEnumerable
    {
        public void Add<T>(T item)
        {
            Console.WriteLine("Item added with instance add method: " + item);
        }

        public IEnumerator GetEnumerator()
        {
            // Some implementation here
        }
    }
    
    public class CollectionWithoutAdd : IEnumerable
    {
        public IEnumerator GetEnumerator()
        {
            // Some implementation here
        }
    }
    
    public static class Extensions
    {
        public static void Add<T>(this CollectionWithoutAdd collection, T item)
        {
            Console.WriteLine("Item added with extension add method: " + item);
        }
    }
    
    public class Program
    {
        public static void Main()
        {
            var collection1 = new CollectionWithAdd{1,2,3}; // Valid in all C# versions
            var collection2 = new CollectionWithoutAdd{4,5,6}; // Valid only since C# 6
        }
    }


Isso irá gerar:

>Item adicionado com o método de adição de instância: 1
>Item adicionado com o método de adição de instância: 2
>Item adicionado com o método de adição de instância: 3
>Item adicionado com o método de adição de extensão: 4
>Item adicionado com o método de adição de extensão: 5
>Item adicionado com o método de adição de extensão: 6

## Desativar melhorias de avisos
No C# 5.0 e anteriores, o desenvolvedor só podia suprimir os avisos por número. Com a introdução dos Analisadores Roslyn, o C# precisa de uma maneira de desabilitar os avisos emitidos de bibliotecas específicas. Com o C# 6.0, a diretiva pragma pode suprimir avisos por nome.

Antes da:

    #pragma warning disable 0501

C# 6.0:

    #pragma warning disable CS0501

