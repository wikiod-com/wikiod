---
title: "Declarações Condicionais"
slug: "declaracoes-condicionais"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Declaração If-Else
A programação em geral geralmente requer uma 'decisão' ou uma 'ramificação' dentro do código para explicar como o código opera sob diferentes entradas ou condições. Dentro da linguagem de programação C# (e na maioria das linguagens de programação para este assunto), a maneira mais simples e às vezes mais útil de criar uma ramificação dentro do seu programa é através de uma instrução `If-Else`.

Vamos supor que temos um método (também conhecido como uma função) que recebe um parâmetro int que representará uma pontuação de até 100, e o método imprimirá uma mensagem dizendo se passamos ou falhamos.

    static void PrintPassOrFail(int score)
    {
        if (score >= 50) // If score is greater or equal to 50
        {
            Console.WriteLine("Pass!");
        }
        else // If score is not greater or equal to 50
        {
            Console.WriteLine("Fail!");
        }
    }

Ao olhar para este método, você pode notar esta linha de código (`score >= 50`) dentro da instrução `If`. Isso pode ser visto como uma condição `boolean`, onde se a condição for avaliada como `true`, então o código que está entre o `if` `{ }` é executado.

Por exemplo, se este método foi chamado assim:
`PrintPassOrFail(60);`, a saída do método seria um Console Print dizendo ***Pass!*** já que o valor do parâmetro de 60 é maior ou igual a 50.

No entanto, se o método foi chamado como: `PrintPassOrFail(30);`, a saída do método seria impressa dizendo ***Fail!***. Isso ocorre porque o valor 30 não é maior ou igual a 50, portanto, o código entre o `else` `{ }` é executado em vez da instrução `If`.

Neste exemplo, dissemos que a *pontuação* deve chegar a 100, o que não foi contabilizado. Para contabilizar a *pontuação* que não passa de 100 ou possivelmente cai abaixo de 0, consulte o exemplo **If-Else If-Else Statement**.

## Declaração If-Else If-Else
Seguindo o exemplo da **Instrução If-Else**, agora é hora de introduzir a instrução `Else If`. A instrução `Else If` segue diretamente após a instrução `If` na estrutura **If-Else If-Else**, mas intrinsecamente tem uma sintaxe semelhante à instrução `If`. Ele é usado para adicionar mais ramificações ao código do que uma simples instrução **If-Else** pode.

No exemplo da **Instrução If-Else**, o exemplo especificou que a pontuação vai até 100; no entanto, nunca houve quaisquer verificações contra isso. Para corrigir isso, vamos modificar o método da **Instrução If-Else** para ficar assim:

    static void PrintPassOrFail(int score)
    {
        if (score > 100) // If score is greater than 100
        {
            Console.WriteLine("Error: score is greater than 100!");
        }
        else if (score < 0) // Else If score is less than 0
        {
            Console.WriteLine("Error: score is less than 0!");
        }
        else if (score >= 50) // Else if score is greater or equal to 50
        {
            Console.WriteLine("Pass!");
        }
        else // If none above, then score must be between 0 and 49
        {
            Console.WriteLine("Fail!");
        }
    }

Todas essas instruções serão executadas em ordem de cima para baixo até que uma condição seja atendida. Nesta nova atualização do método, adicionamos duas novas ramificações para agora acomodar a pontuação *fora dos limites*.

Por exemplo, se agora chamássemos o método em nosso código como `PrintPassOFail(110);`, a saída seria um Console Print dizendo ***Erro: pontuação é maior que 100!***; e se chamássemos o método em nosso código como `PrintPassOrFail(-20);`, a saída diria ***Error: score is less than 0!***.

## Se as condições da instrução forem expressões e valores booleanos padrão
A seguinte declaração

    if (conditionA && conditionB && conditionC) //...
é exatamente equivalente a

    bool conditions = conditionA && conditionB && conditionC;
    if (conditions) // ...
em outras palavras, as condições dentro da instrução "if" apenas formam uma expressão booleana comum.

Um erro comum ao escrever instruções condicionais é comparar explicitamente com `true` e `false`:

    if (conditionA == true && conditionB == false && conditionC == true) // ...

Isso pode ser reescrito como

    if (conditionA && !conditionB && conditionC)

## Declarações de troca
Uma instrução switch permite que uma variável seja testada quanto à igualdade em relação a uma lista de valores. Cada valor é chamado de caso, e a variável que está sendo ativada é verificada para cada caso de comutação.

Uma instrução `switch` é geralmente mais concisa e compreensível do que instruções `if...else if...else..` ao testar vários valores possíveis para uma única variável.

    
A sintaxe é a seguinte

    switch(expression) {
       case constant-expression:
          statement(s);
          break;
       case constant-expression:
          statement(s);
          break;
      
       // you can have any number of case statements
       default : // Optional
          statement(s);
          break;
    }

existem várias coisas que devem ser consideradas ao usar a instrução switch

- A expressão usada em uma instrução switch deve ter um tipo integral ou enumerado, ou ser de um tipo de classe em que a classe tenha uma única função de conversão para um tipo integral ou enumerado.
- Você pode ter qualquer número de instruções case dentro de um switch. Cada caso é seguido pelo valor a ser comparado e dois pontos. Os valores a serem comparados devem ser exclusivos em cada instrução switch.
- Uma instrução switch pode ter um caso padrão opcional. O caso padrão pode ser usado para executar uma tarefa quando nenhum dos casos for verdadeiro.
- Cada case deve terminar com uma instrução `break`, a menos que seja uma instrução vazia. Nesse caso, a execução continuará no caso abaixo dele. A instrução break também pode ser omitida quando uma instrução `return`, `throw` ou `goto case` é usada.


O exemplo pode ser dado com as notas sábias

    char grade = 'B';

    switch (grade)
    {
        case 'A':
            Console.WriteLine("Excellent!");
            break;
        case 'B':
        case 'C':
            Console.WriteLine("Well done");
            break;
        case 'D':
            Console.WriteLine("You passed");
            break;
        case 'F':
            Console.WriteLine("Better try again");
            break;
        default:
            Console.WriteLine("Invalid grade");
            break;
    }

