---
title: "Tipos personalizados"
slug: "tipos-personalizados"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Normalmente, um `struct` é usado apenas quando o desempenho é muito importante. Como os tipos de valor vivem na pilha, eles podem ser acessados ​​muito mais rapidamente do que as classes. No entanto, a pilha tem muito menos espaço do que o heap, então structs devem ser mantidos pequenos (Microsoft recomenda que `struct`s não ocupem mais de 16 bytes).

Uma `class` é o tipo mais usado (desses três) em C# e geralmente é o que você deve usar primeiro.

Um `enum` é usado sempre que você pode ter uma lista distinta e claramente definida de itens que precisam ser definidos apenas uma vez (em tempo de compilação). Enums são úteis para programadores como uma referência leve para algum valor: em vez de definir uma lista de variáveis ​​`constantes` para comparar, você pode usar um enum e obter suporte do Intellisense para garantir que não use acidentalmente um valor errado.

## Definição de estrutura
Structs herdam de System.ValueType, são tipos de valor e residem na pilha. Quando os tipos de valor são passados ​​como um parâmetro, eles são passados ​​por valor.
-------------------------------------------------- -----------------------

    Struct MyStruct
    {
        public int x;
        public int y;
    }

**Passado por valor** significa que o valor do parâmetro é *copiado* para o método e quaisquer alterações feitas no parâmetro no método não são refletidas fora do método. Por exemplo, considere o código a seguir, que chama um método chamado `AddNumbers`, passando as variáveis ​​`a` e `b`, que são do tipo `int`, que é um tipo Value.

    int a = 5;
    int b = 6;
    
    AddNumbers(a,b);

    public AddNumbers(int x, int y)
    {
        int z = x + y; // z becomes 11
        x = x + 5; // now we changed x to be 10
        z = x + y; // now z becomes 16
    } 

Embora tenhamos adicionado 5 a `x` dentro do método, o valor de `a` permanece inalterado, porque é um tipo de valor, e isso significa que `x` era uma *cópia* do valor de `a`, mas na verdade não `a`.

Lembre-se, os tipos de valor vivem na pilha e são passados ​​por valor.

    

## Definição de classe
As classes herdam de System.Object, são tipos de referência e residem no heap. Quando os tipos de referência são passados ​​como um parâmetro, eles são passados ​​por referência.
-------------------------------------------------- -----------------------


    public Class MyClass
    {
        public int a;
        public int b;
    }

**Passado por referência** significa que uma *referência* ao parâmetro é passada para o método, e quaisquer alterações no parâmetro serão refletidas fora do método quando ele retornar, porque a referência é *exatamente ao mesmo objeto em memória*. Vamos usar o mesmo exemplo de antes, mas vamos "empacotar" os `int`s em uma classe primeiro.

    MyClass instanceOfMyClass = new MyClass();
    instanceOfMyClass.a = 5;
    instanceOfMyClass.b = 6;
    
    AddNumbers(instanceOfMyClass);
    
    public AddNumbers(MyClass sample)
    {
        int z = sample.a + sample.b; // z becomes 11
        sample.a = sample.a + 5; // now we changed a to be 10
        z = sample.a + sample.b; // now z becomes 16
    } 

Desta vez, quando mudamos `sample.a` para `10`, o valor de `instanceOfMyClass.a` *também* muda, porque foi *passado por referência*. Passado por referência significa que uma *referência* (também chamada de *ponteiro*) para o objeto foi passada para o método, em vez de uma cópia do próprio objeto.

Lembre-se, os tipos de referência residem no heap e são passados ​​por referência.

## Definição de enumeração
Um enum é um tipo especial de classe. A palavra-chave `enum` informa ao compilador que esta classe herda da classe abstrata System.Enum. Enums são usados ​​para listas distintas de itens.
-------------------------------------------------- -----------------------

    
    public enum MyEnum
    {
        Monday = 1,
        Tuesday,
        Wednesday,
        //...
    }

Você pode pensar em um enum como uma maneira conveniente de mapear constantes para algum valor subjacente. A enumeração definida acima declara valores para cada dia da semana e começa com `1`. `Tuesday` seria automaticamente mapeado para `2`, `Wednesday` para `3`, etc.

Por padrão, enums usam `int` como o tipo subjacente e começam em 0, mas você pode usar qualquer um dos seguintes *tipos integrais*: `byte, sbyte, short, ushort, int, uint, long ou ulong` e pode especificar valores explícitos para qualquer item. Se alguns itens forem especificados explicitamente, mas outros não, cada item após o último definido será incrementado em 1.

Nós usaríamos este exemplo *casting* algum outro valor para um *MyEnum* assim:

    MyEnum instance = (MyEnum)3; // the variable named 'instance' gets a 
                                 //value of MyEnum.Wednesday, which maps to 3.

    int x = 2;
    instance = (MyEnum)x; // now 'instance' has a value of MyEnum.Tuesday

Outro tipo de enum útil, embora mais complexo, é chamado de `Flags`. Ao *decorar* uma enumeração com o atributo `Flags`, você pode atribuir a uma variável mais de um valor por vez. Observe que ao fazer isso você *deve* definir valores explicitamente na representação de base 2.

    [Flags]
    public enum MyEnum
    {
        Monday = 1,
        Tuesday = 2,
        Wednesday = 4,
        Thursday = 8,
        Friday = 16,
        Saturday = 32, 
        Sunday = 64
    }

Agora você pode comparar mais de um valor por vez, usando *comparações bit a bit* ou, se estiver usando .NET 4.0 ou posterior, o método interno `Enum.HasFlag`.

    MyEnum instance = MyEnum.Monday | MyEnum.Thursday; // instance now has a value of
                                                       // *both* Monday and Thursday,
                                                       // represented by (in binary) 0100. 

    if (instance.HasFlag(MyEnum.Wednesday))
    {
        // it doesn't, so this block is skipped
    }
    else if (instance.HasFlag(MyEnum.Thursday))
    {
        // it does, so this block is executed
    }



Como a classe Enum é subclasse de `System.ValueType`, ela é tratada como um tipo de valor e passada por valor, não por referência. O objeto base é criado no heap, mas quando você passa um valor enum para uma chamada de função, uma cópia do valor usando o tipo de valor subjacente do Enum (normalmente System.Int32) é enviada para a pilha. O compilador rastreia a associação entre esse valor e o objeto base que foi criado na pilha. Consulte [Classe ValueType (Sistema) (MSDN)][1] para obter mais informações.


[1]: https://msdn.microsoft.com/en-us/library/system.valuetype(v=vs.110).aspx

