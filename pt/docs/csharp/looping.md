---
title: "Loop"
slug: "loop"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Loop Foreach
foreach irá iterar sobre qualquer objeto de uma classe que implemente `IEnumerable` (observe que `IEnumerable<T>` herda dele). Esses objetos incluem alguns internos, mas não se limitam a: `List<T>`, `T[]` (arrays de qualquer tipo), `Dictionary<TKey, TSource>`, bem como interfaces como `IQueryable` e `ICollection`, etc.

**sintaxe**

    foreach(ItemType itemVariable in enumerableObject)
        statement;

**observações**

1. O tipo "ItemType" não precisa corresponder ao tipo preciso dos itens, ele só precisa ser atribuível a partir do tipo dos itens
2. Em vez de `ItemType`, alternativamente `var` pode ser usado para inferir o tipo de itens do enumerableObject inspecionando o argumento genérico da implementação `IEnumerable`
3. A instrução pode ser um bloco, uma única instrução ou até mesmo uma instrução vazia (`;`)
4. Se `enumerableObject` não estiver implementando `IEnumerable`, o código não será compilado
5. Durante cada iteração, o item atual é convertido em `ItemType` (mesmo que isso não seja especificado, mas inferido pelo compilador via `var`) e se o item não puder ser convertido, uma `InvalidCastException` será lançada.

Considere este exemplo:

    var list = new List<string>();
    list.Add("Ion");
    list.Add("Andrei");
    foreach(var name in list)
    {
        Console.WriteLine("Hello " + name);
    }

é equivalente a:

    var list = new List<string>();
    list.Add("Ion");
    list.Add("Andrei");
    IEnumerator enumerator;
    try
    {
        enumerator = list.GetEnumerator();
        while(enumerator.MoveNext())
        {
            string name = (string)enumerator.Current;
            Console.WriteLine("Hello " + name);
        }
    }
    finally
    {
        if (enumerator != null)
            enumerator.Dispose();
    }

## Para Loop
Um For Loop é ótimo para fazer as coisas por um determinado período de tempo. É como um While Loop, mas o incremento está incluído na condição.

Um For Loop é configurado assim:

    for (Initialization; Condition; Increment)
    {
        // Code
    }

> Inicialização - Cria uma nova variável local que só pode ser usada no loop.
> Condição - O loop só é executado quando a condição for verdadeira.
> Increment - Como a variável muda toda vez que o loop é executado.

Um exemplo:

    for (int i = 0; i < 5; i++)
    {
        Console.WriteLine(i);
    }

Resultado:

> 0
> 1
> 2
> 3
> 4

Você também pode deixar espaços no For Loop, mas você precisa ter todos os pontos e vírgulas para que funcione.

    int input = Console.ReadLine();    

    for ( ; input < 10; input + 2)
    {
        Console.WriteLine(input);
    }

Saída para 3:
>3
>5
>7
>9
>11

## Faça - Enquanto Loop
É semelhante a um loop `while`, exceto que ele testa a condição no *final* do corpo do loop. O loop Do - While executa o loop uma vez, independentemente de a condição ser verdadeira ou não.

    int[] numbers = new int[] { 6, 7, 8, 10 };
        
    // Sum values from the array until we get a total that's greater than 10,
    // or until we run out of values.
    int sum = 0;
    int i = 0;
    do
    {
        sum += numbers[i];
        i++;
    } while (sum <= 10 && i < numbers.Length);
        
    System.Console.WriteLine(sum); // 13


## Estilos de loop
**Enquanto**

O tipo de loop mais trivial. A única desvantagem é que não há nenhuma pista intrínseca para saber onde você está no circuito.

    /// loop while the condition satisfies
    while(condition)
    {
        /// do something
    }

**Fazer**

Semelhante ao `while`, mas a condição é avaliada no final do loop em vez do início. Isso resulta na execução dos loops pelo menos uma vez.

    do
    {
        /// do something
    } while(condition) /// loop while the condition satisfies


**Por**

Outro estilo de loop trivial. Durante o loop, um índice (`i`) aumenta e você pode usá-lo. Geralmente é usado para manipular arrays.

    for ( int i = 0; i < array.Count; i++ )
    {
        var currentItem = array[i];
        /// do something with "currentItem"
    }

**Para cada**

Maneira modernizada de fazer loop através de objetos `IEnumarable`. Ainda bem que você não precisa pensar no índice do item ou na contagem de itens da lista.

    foreach ( var item in someList )
    {
        /// do something with "item"
    }

**Método Foreach**

Enquanto os outros estilos são usados ​​para selecionar ou atualizar os elementos em coleções, este estilo geralmente é usado para *chamar um método* imediatamente para todos os elementos em uma coleção.

    list.ForEach(item => item.DoSomething());

    // or
    list.ForEach(item => DoSomething(item));

    // or using a method group
    list.ForEach(Console.WriteLine);

    // using an array
    Array.ForEach(myArray, Console.WriteLine);

É importante notar que este método está disponível apenas em instâncias `List<T>` e como um método estático em `Array` - ele **não** faz parte do Linq.

**Linq Parallel Foreach**

Assim como o Linq Foreach, exceto que este faz o trabalho de maneira paralela. O que significa que todos os itens da coleção executarão a ação determinada ao mesmo tempo, simultaneamente.

    collection.AsParallel().ForAll(item => item.DoSomething());

    /// or
    collection.AsParallel().ForAll(item => DoSomething(item));

## Loops aninhados
    // Print the multiplication table up to 5s
    for (int i = 1; i <= 5; i++)
    {
        for (int j = 1; j <= 5; j++)
        {
            int product = i * j;
            Console.WriteLine("{0} times {1} is {2}", i, j, product);
        }
    }

## parar
Às vezes, a condição do loop deve ser verificada no meio do loop. O primeiro é sem dúvida mais elegante que o último:

    for (;;)
    {
        // precondition code that can change the value of should_end_loop expression
    
        if (should_end_loop)
            break;
    
        // do something
    }

Alternativo:

    bool endLoop = false;
    for (; !endLoop;)
    {
        // precondition code that can set endLoop flag
    
        if (!endLoop)
        {
            // do something
        }
    }

Nota: Em loops aninhados e/ou `switch` deve usar mais do que apenas um simples `break`.

##Enquanto loop
    int n = 0;
    while (n < 5) 
    {
        Console.WriteLine(n);
        n++;
    }

Resultado:

> 0
> 1
> 2
> 3
> 4

IEnumerators podem ser iterados com um loop while:

    // Call a custom method that takes a count, and returns an IEnumerator for a list
    // of strings with the names of theh largest city metro areas.
    IEnumerator<string> largestMetroAreas = GetLargestMetroAreas(4);

    while (largestMetroAreas.MoveNext())
    {
        Console.WriteLine(largestMetroAreas.Current);
    }

Saída de amostra:

> Tóquio/Yokohama
> Metrô de Nova York
> Sao Paulo
> Seul/Incheon

## Prosseguir
Além de `break`, há também a palavra-chave `continue`. Em vez de quebrar completamente o loop, ele simplesmente pulará a iteração atual. Pode ser útil se você não quiser que algum código seja executado se um valor específico for definido.

Aqui está um exemplo simples:

    for (int i = 1; i <= 10; i++)
    {
        if (i < 9)
            continue;

        Console.WriteLine(i);
    }

Vai resultar em:

    9
    10

**Observação:** `Continuar` geralmente é mais útil em loops while ou do-while. For-loops, com condições de saída bem definidas, podem não se beneficiar tanto.

