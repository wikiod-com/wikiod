---
title: "Operador de coalescência nula"
slug: "operador-de-coalescencia-nula"
draft: false
images: []
weight: 9486
type: docs
toc: true
---

## Sintaxe
- var resultado = possibleNullObject ?? valor padrão;

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |
| `possibleNullObject` | O valor para testar o valor nulo. Se não for nulo, esse valor será retornado. Deve ser um tipo anulável. |
| `defaultValue` | O valor retornado se `possibleNullObject` for nulo. Deve ser do mesmo tipo que `possibleNullObject`. |

O próprio operador nulo de coalescência são dois caracteres de ponto de interrogação consecutivos: `??`

É uma abreviação para a expressão condicional:

    possibleNullObject != null ? possibleNullObject : defaultValue

O operando do lado esquerdo (objeto sendo testado) deve ser um tipo de valor anulável ou tipo de referência, ou ocorrerá um erro de compilação.

O ?? O operador funciona para tipos de referência e tipos de valor.



## Uso básico
O uso do [`operador de coalescência nulo (??)`][2] permite que você especifique um valor padrão para um tipo anulável se o operando à esquerda for `nulo`.

    string testString = null;
    Console.WriteLine("The specified string is - " + (testString ?? "not provided"));

[Demonstração ao vivo no .NET Fiddle](https://dotnetfiddle.net/GNosPU)

Isso é logicamente equivalente a:

    string testString = null;
    if (testString == null)
    {
        Console.WriteLine("The specified string is - not provided");
    }
    else
    {
        Console.WriteLine("The specified string is - " + testString);
    }

ou usando o operador [operador ternário (?:)][1]:

    string testString = null;
    Console.WriteLine("The specified string is - " + (testString == null ? "not provided" : testString));


[1]: https://www.wikiod.com/pt/docs/c%23/18/operators/6029/ternary-operator#t=201610101110242934481
[2]: https://msdn.microsoft.com/en-us/library/ms173224.aspx

## Null fall-through e encadeamento
O operando à esquerda deve ser anulável, enquanto o operando à direita pode ou não ser. O resultado será digitado de acordo.

**Não anulável**

    int? a = null;
    int b = 3;
    var output = a ?? b;
    var type = output.GetType();  

    Console.WriteLine($"Output Type :{type}");
    Console.WriteLine($"Output value :{output}");

**Resultado:**
>Tipo: System.Int32
>valor:3

[Ver demonstração][1]

**Anulável**

    int? a = null;
    int? b = null;
    var output = a ?? b;

`output` será do tipo `int?` e igual a `b`, ou `null`.

**Coalescência múltipla**

A coalescência também pode ser feita em cadeias:

    int? a = null;
    int? b = null;
    int c = 3;
    var output = a ?? b ?? c;

    var type = output.GetType();    
    Console.WriteLine($"Type :{type}");
    Console.WriteLine($"value :{output}");

**Resultado:**
>Tipo: System.Int32
> valor :3

[Ver demonstração][2]

**Encadeamento Condicional Nulo**

O operador de coalescência nulo pode ser usado em conjunto com o [operador de propagação nulo][3] para fornecer acesso mais seguro às propriedades dos objetos.

    object o = null;
    var output = o?.ToString() ?? "Default Value";

**Resultado:**
>Tipo :System.String
>valor: valor padrão

[Ver demonstração][4]


[1]: https://dotnetfiddle.net/hKHOcN
[2]: https://dotnetfiddle.net/xC8Bmc
[3]: https://www.wikiod.com/pt/docs/c%23/24/c-sharp-6-0-features/51/null-propagation#t=201607280322338995462
[4]: https://dotnetfiddle.net/nk1QRn

## Operador de coalescência nulo com chamadas de método
O operador de coalescência nulo facilita a garantia de que um método que pode retornar `null` retornará a um valor padrão.

Sem o operador de coalescência nulo:

    string name = GetName();

    if (name == null)
        name = "Unknown!";

Com o operador de coalescência nulo:

    string name = GetName() ?? "Unknown!";


## Use existente ou crie um novo
Um cenário de uso comum com o qual esse recurso realmente ajuda é quando você está procurando um objeto em uma coleção e precisa criar um novo, caso ainda não exista.

    IEnumerable<MyClass> myList = GetMyList();
    var item = myList.SingleOrDefault(x => x.Id == 2) ?? new MyClass { Id = 2 };

## Inicialização de propriedades lentas com operador de coalescência nulo
    private List<FooBar> _fooBars;
    
    public List<FooBar> FooBars
    {
        get { return _fooBars ?? (_fooBars = new List<FooBar>()); }
    }

A primeira vez que a propriedade `.FooBars` for acessada, a variável `_fooBars` será avaliada como `null`, portanto, a instrução de atribuição atribui e avalia o valor resultante.

Segurança do fio
===
Esta é uma maneira **não thread-safe** de implementar propriedades lentas. Para preguiça de thread-safe, use a classe [`Lazy<T>`][1] incorporada ao .NET Framework.

C# 6 Syntactic Sugar usando corpos de expressão
====

Observe que desde o C# 6, essa sintaxe pode ser simplificada usando o corpo da expressão para a propriedade:

    private List<FooBar> _fooBars;
    
    public List<FooBar> FooBars => _fooBars ?? ( _fooBars = new List<FooBar>() );

Os acessos subsequentes à propriedade renderão o valor armazenado na variável `_fooBars`.

Exemplo no padrão MVVM
===

Isso geralmente é usado ao implementar comandos no padrão MVVM. Em vez de inicializar os comandos avidamente com a construção de um viewmodel, os comandos são inicializados lentamente usando este padrão da seguinte forma:

    private ICommand _actionCommand = null;
    public ICommand ActionCommand =>
       _actionCommand ?? ( _actionCommand = new DelegateCommand( DoAction ) );


[1]: https://www.wikiod.com/pt/docs/c%23/1192/singleton-implementation/6795/lazy-thread-safe-singleton-using-lazyt

