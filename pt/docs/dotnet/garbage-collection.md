---
title: "Coleta de lixo"
slug: "coleta-de-lixo"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Em .Net, os objetos criados com new() são alocados no heap gerenciado. Esses objetos nunca são finalizados explicitamente pelo programa que os utiliza; em vez disso, esse processo é controlado pelo .Net Garbage Collector.

Alguns dos exemplos abaixo são "casos de laboratório" para mostrar o Garbage Collector em funcionamento e alguns detalhes significativos de seu comportamento, enquanto outros se concentram em como preparar as classes para o manuseio adequado pelo Garbage Collector.

O Garbage Collector visa reduzir o custo do programa em termos de memória alocada, mas isso tem um custo em termos de tempo de processamento. Para alcançar um bom compromisso geral, há uma série de otimizações que devem ser levadas em consideração ao programar com o Garbage Collector em mente:

- Se o método Collect() deve ser invocado explicitamente (o que nem sempre deve ser o caso), considere usar o modo "otimizado" que finaliza o objeto morto somente quando a memória é realmente necessária
- Em vez de invocar o método Collect(), considere usar os métodos AddMemoryPressure() e RemoveMemoryPressure(), que acionam uma coleção de memória somente se realmente necessário
- Uma coleção de memória não é garantida para finalizar todos os objetos mortos; em vez disso, o Garbage Collector gerencia 3 "gerações", um objeto às vezes "sobrevivendo" de uma geração para a próxima
- Vários modelos de encadeamento podem ser aplicados, dependendo de vários fatores, incluindo ajuste fino de configuração, resultando em diferentes graus de interferência entre o encadeamento do Garbage Collector e os outros encadeamentos do aplicativo

  

## Um exemplo básico de coleta (de lixo)
Dada a seguinte classe:

    public class FinalizableObject 
    {
        public FinalizableObject()
        {
            Console.WriteLine("Instance initialized");
        }

        ~FinalizableObject()
        {
            Console.WriteLine("Instance finalized");
        }
    }
Um programa que cria uma instância, mesmo sem usá-la:

    new FinalizableObject(); // Object instantiated, ready to be used
Produz a seguinte saída:

    <namespace>.FinalizableObject initialized
Se nada mais acontecer, o objeto não é finalizado até que o programa termine (o que libera todos os objetos no heap gerenciado, finalizando-os no processo).

É possível forçar o Garbage Collector a funcionar em um determinado ponto, como segue:

    new FinalizableObject(); // Object instantiated, ready to be used
    GC.Collect();
O que produz o seguinte resultado:

    <namespace>.FinalizableObject initialized
    <namespace>.FinalizableObject finalized
Desta vez, assim que o Garbage Collector foi invocado, o objeto não utilizado (também conhecido como "morto") foi finalizado e liberado do heap gerenciado.

## Objetos vivos e objetos mortos - o básico
Regra geral: quando a coleta de lixo ocorre, "objetos vivos" são aqueles que ainda estão em uso, enquanto "objetos mortos" são aqueles que não são mais usados ​​(qualquer variável ou campo que os referencie, se houver, saiu do escopo antes da coleta ocorrer) .

No exemplo a seguir (por conveniência, FinalizableObject1 e FinalizableObject2 são subclasses de FinalizableObject do exemplo acima e, portanto, herdam o comportamento da mensagem de inicialização/finalização):

    var obj1 = new FinalizableObject1(); // Finalizable1 instance allocated here
    var obj2 = new FinalizableObject2(); // Finalizable2 instance allocated here
    obj1 = null; // No more references to the Finalizable1 instance 
    GC.Collect();
A saída será:

    <namespace>.FinalizableObject1 initialized
    <namespace>.FinalizableObject2 initialized
    <namespace>.FinalizableObject1 finalized
No momento em que o Garbage Collector é invocado, FinalizableObject1 é um objeto morto e é finalizado, enquanto FinalizableObject2 é um objeto ativo e é mantido no heap gerenciado.

## Vários objetos mortos
E se dois (ou vários) objetos mortos referenciarem um ao outro? Isso é mostrado no exemplo abaixo, supondo que OtherObject seja uma propriedade pública de FinalizableObject:

    var obj1 = new FinalizableObject1(); 
    var obj2 = new FinalizableObject2();
    obj1.OtherObject = obj2;
    obj2.OtherObject = obj1;
    obj1 = null; // Program no longer references Finalizable1 instance
    obj2 = null; // Program no longer references Finalizable2 instance
    // But the two objects still reference each other
    GC.Collect();
Isso produz a seguinte saída:

    <namespace>.FinalizedObject1 initialized
    <namespace>.FinalizedObject2 initialized
    <namespace>.FinalizedObject1 finalized
    <namespace>.FinalizedObject2 finalized
Os dois objetos são finalizados e liberados do heap gerenciado apesar de referenciarem um ao outro (porque nenhuma outra referência existe a qualquer um deles de um objeto realmente ativo).

## Referências fracas
Referências fracas são... referências a outros objetos (também conhecidos como "alvos"), mas "fracas", pois não impedem que esses objetos sejam coletados como lixo. Em outras palavras, referências fracas não contam quando o Garbage Collector avalia objetos como "vivos" ou "mortos".

O seguinte código:

    var weak = new WeakReference<FinalizableObject>(new FinalizableObject());
    GC.Collect();
Produz a saída:

    <namespace>.FinalizableObject initialized
    <namespace>.FinalizableObject finalized
O objeto é liberado do heap gerenciado apesar de ser referenciado pela variável WeakReference (ainda no escopo quando o coletor de lixo foi invocado).

Consequência nº 1: a qualquer momento, não é seguro assumir se um destino WeakReference ainda está alocado no heap gerenciado ou não.

Consequência #2: sempre que um programa precisar acessar o destino de uma Referência Fraca, o código deve ser fornecido para ambos os casos, sendo o destino ainda alocado ou não. O método para acessar o destino é TryGetTarget:
 
    var target = new object(); // Any object will do as target
    var weak = new WeakReference<object>(target); // Create weak reference
    target = null; // Drop strong reference to the target

    // ... Many things may happen in-between

    // Check whether the target is still available
    if(weak.TryGetTarget(out target))
    {
        // Use re-initialized target variable
        // To do whatever the target is needed for
    }
    else
    {
        // Do something when there is no more target object
        // The target variable value should not be used here
    }

A versão genérica do WeakReference está disponível desde o .Net 4.5. Todas as versões do framework fornecem uma versão não genérica e sem tipo que é construída da mesma maneira e verificada da seguinte forma:

    var target = new object(); // Any object will do as target
    var weak = new WeakReference(target); // Create weak reference
    target = null; // Drop strong reference to the target

    // ... Many things may happen in-between

    // Check whether the target is still available
    if (weak.IsAlive)
    {
        target = weak.Target;

        // Use re-initialized target variable
        // To do whatever the target is needed for
    }
    else
    {
        // Do something when there is no more target object
        // The target variable value should not be used here
    }


  

## Dispose() vs. finalizadores
Implemente o método Dispose() (e declare a classe que a contém como IDisposable) como um meio de garantir que quaisquer recursos com muita memória sejam liberados assim que o objeto não for mais usado. A "pegadinha" é que não há nenhuma garantia forte de que o método Dispose() seja invocado (ao contrário dos finalizadores que sempre são invocados no final da vida útil do objeto).

Um cenário é um programa chamando Dispose() em objetos que ele cria explicitamente:

    private void SomeFunction()
    {
        // Initialize an object that uses heavy external resources
        var disposableObject = new ClassThatImplementsIDisposable();

        // ... Use that object

        // Dispose as soon as no longer used
        disposableObject.Dispose();

        // ... Do other stuff 

        // The disposableObject variable gets out of scope here
        // The object will be finalized later on (no guarantee when)
        // But it no longer holds to the heavy external resource after it was disposed
    }


Outro cenário é declarar uma classe a ser instanciada pelo framework. Neste caso a nova classe geralmente herda uma classe base, por exemplo em MVC cria-se uma classe controladora como uma subclasse de System.Web.Mvc.ControllerBase. Quando a classe base implementa a interface IDisposable, esta é uma boa dica de que Dispose() seria invocado corretamente pelo framework - mas novamente não há nenhuma garantia forte.

Assim Dispose() não é um substituto para um finalizador; em vez disso, os dois devem ser usados ​​para propósitos diferentes:

- Um finalizador eventualmente libera recursos para evitar vazamentos de memória que ocorreriam de outra forma
- Dispose() libera recursos (possivelmente os mesmos) assim que eles não são mais necessários, para aliviar a pressão sobre a alocação geral de memória.

## Descarte e finalização adequada dos objetos
Como Dispose() e finalizadores têm propósitos diferentes, uma classe que gerencia recursos externos com muita memória deve implementar ambos. A consequência é escrever a classe para que ela lide bem com dois cenários possíveis:

- Quando apenas o finalizador é invocado
- Quando Dispose() é invocado primeiro e depois o finalizador também é invocado

Uma solução é escrever o código de limpeza de forma que executá-lo uma ou duas vezes produza o mesmo resultado que executá-lo apenas uma vez. A viabilidade depende da natureza da limpeza, por exemplo:
- Fechar uma conexão de banco de dados já fechada provavelmente não teria efeito, então funciona
- Atualizar alguma "contagem de uso" é perigoso e produziria um resultado errado quando chamado duas vezes em vez de uma vez.

Uma solução mais segura é garantir por design que o código de limpeza seja chamado uma vez e apenas uma vez, qualquer que seja o contexto externo. Isso pode ser alcançado da "maneira clássica" usando um sinalizador dedicado:

    public class DisposableFinalizable1: IDisposable
    {
        private bool disposed = false;

        ~DisposableFinalizable1() { Cleanup(); }

        public void Dispose() { Cleanup(); }

        private void Cleanup()
        {
            if(!disposed)
            {
                // Actual code to release resources gets here, then
                disposed = true;
            }
        }
    }


Como alternativa, o Garbage Collector fornece um método específico SuppressFinalize() que permite pular o finalizador após a chamada de Dispose:

    public class DisposableFinalizable2 : IDisposable
    {
        ~DisposableFinalizable2() { Cleanup(); }

        public void Dispose()
        {
            Cleanup();
            GC.SuppressFinalize(this);
        }

        private void Cleanup()
        {
            // Actual code to release resources gets here
        }
    }


 

