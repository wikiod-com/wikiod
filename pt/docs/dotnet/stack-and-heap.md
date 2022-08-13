---
title: "Pilha e pilha"
slug: "pilha-e-pilha"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Vale a pena notar que ao declarar um tipo de referência, seu valor inicial será `null`. Isso ocorre porque ainda não aponta para um local na memória e é um estado perfeitamente válido.
No entanto, com exceção dos tipos anuláveis, os tipos de valor geralmente sempre devem ter um valor.



## Tipos de valor em uso
Os tipos de valor simplesmente contêm um _**valor**_.

Todos os tipos de valor são derivados da classe [System.ValueType][1], e isso inclui a maioria dos tipos internos.

Ao criar um novo tipo de valor, uma área de memória chamada __*a pilha*__ é usada.
A pilha crescerá de acordo com o tamanho do tipo declarado. Assim, por exemplo, um int sempre receberá 32 bits de memória na pilha. Quando o tipo de valor não estiver mais no escopo, o espaço na pilha será desalocado.

O código abaixo demonstra um tipo de valor sendo atribuído a uma nova variável. Um struct está sendo usado como uma maneira conveniente de criar um tipo de valor personalizado (a classe System.ValueType não pode ser estendida de outra forma).

O importante a entender é que ao atribuir um tipo de valor, o próprio valor _**copiado**_ para a nova variável, significando que temos duas instâncias distintas do objeto, que não podem afetar uma à outra.

    struct PersonAsValueType
    {
        public string Name;
    }

    class Program
    {
        static void Main()
        {
            PersonAsValueType personA;

            personA.Name = "Bob";

            var personB = personA;

            personA.Name = "Linda";

            Console.WriteLine(                // Outputs 'False' - because 
                object.ReferenceEquals(       // personA and personB are referencing 
                    personA,                  // different areas of memory
                    personB));                

            Console.WriteLine(personA.Name);  // Outputs 'Linda'
            Console.WriteLine(personB.Name);  // Outputs 'Bob'
        }
    }


[1]: https://msdn.microsoft.com/en-us/library/system.valuetype.aspx

## Tipos de referência em uso
Os tipos de referência são compostos por uma _**referência**_ para uma área de memória e um _**valor**_ armazenado nessa área.
Isso é análogo aos ponteiros em C/C++.

Todos os tipos de referência são armazenados no que é conhecido como _**heap**_.
O heap é simplesmente uma área gerenciada da memória onde os objetos são armazenados. Quando um novo objeto é instanciado, uma parte do heap será alocada para uso por esse objeto e uma referência a esse local do heap será retornada. A pilha é gerenciada e mantida pelo _coletor de lixo_ e não permite intervenção manual.

Além do espaço de memória necessário para a própria instância, é necessário espaço adicional para armazenar a própria referência, juntamente com informações temporárias adicionais exigidas pelo .NET CLR.

O código abaixo demonstra um tipo de referência sendo atribuído a uma nova variável. Neste caso, estamos usando uma classe, todas as classes são tipos de referência (mesmo que estáticas).

Quando um tipo de referência é atribuído a outra variável, é a _**referência**_ para o objeto que é copiado, __não__ o próprio valor. Essa é uma distinção importante entre tipos de valor e tipos de referência.

As implicações disso são que agora temos _duas_ referências ao mesmo objeto.
Quaisquer alterações nos valores desse objeto serão refletidas por ambas as variáveis.

    class PersonAsReferenceType
    {
        public string Name;
    }

    class Program
    {
        static void Main()
        {
            PersonAsReferenceType personA;

            personA = new PersonAsReferenceType { Name = "Bob" };

            var personB = personA;

            personA.Name = "Linda";

            Console.WriteLine(               // Outputs 'True' - because
                object.ReferenceEquals(      // personA and personB are referencing 
                    personA,                 // the *same* memory location
                    personB));

            Console.WriteLine(personA.Name); // Outputs 'Linda'
            Console.WriteLine(personB.Name); // Outputs 'Linda'
        }

