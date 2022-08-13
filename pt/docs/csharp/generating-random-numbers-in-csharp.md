---
title: "Gerando números aleatórios em C#"
slug: "gerando-numeros-aleatorios-em-c"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

## Sintaxe
- Aleatório()

- Aleatório(int Semente)

-int Próximo()

- int Próximo(int maxValue)

- int Próximo(int minValue, int maxValue)


## Parâmetros
| Parâmetros | Detalhes |
| ---------- | ------- |
| Semente | Um valor para gerar números aleatórios. Se não for definido, o valor padrão é determinado pela hora atual do sistema.
| minValue | Os números gerados não serão menores que esse valor. Se não estiver definido, o valor padrão é 0.
| maxValue | Os números gerados serão menores que este valor. Se não for definido, o valor padrão é `Int32.MaxValue`.
| valor de retorno | Retorna um número com valor aleatório.

A semente aleatória gerada pelo sistema não é a mesma em cada execução diferente.

Sementes geradas ao mesmo tempo podem ser as mesmas.

## Gera um int aleatório
Este exemplo gera valores aleatórios entre 0 e 2147483647.

    Random rnd = new Random();
    int randomNumber = rnd.Next();

## Gera um int aleatório em um determinado intervalo
Gere um número aleatório entre `minValue` e `maxValue - 1`.

    Random rnd = new Random();
    var randomBetween10And20 = rnd.Next(10, 20);

## Gerando a mesma sequência de números aleatórios repetidamente
Ao criar instâncias `Random` com a mesma semente, os mesmos números serão gerados.

    int seed = 5;
    for (int i = 0; i < 2; i++)
    {
       Console.WriteLine("Random instance " + i);
       Random rnd = new Random(seed);
       for (int j = 0; j < 5; j++)
       {
          Console.Write(rnd.Next());
          Console.Write(" ");
       }
    
       Console.WriteLine();
    }

Resultado:

    Random instance 0
    726643700 610783965 564707973 1342984399 995276750
    Random instance 1
    726643700 610783965 564707973 1342984399 995276750

## Crie várias classes aleatórias com diferentes sementes simultaneamente
Duas classes Random criadas ao mesmo tempo terão o mesmo valor de semente.

Usando `System.Guid.NewGuid().GetHashCode()` pode obter uma semente diferente mesmo ao mesmo tempo.

    Random rnd1 = new Random();
    Random rnd2 = new Random();
    Console.WriteLine("First 5 random number in rnd1");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd1.Next());

    Console.WriteLine("First 5 random number in rnd2");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd2.Next());

    rnd1 = new Random(Guid.NewGuid().GetHashCode());
    rnd2 = new Random(Guid.NewGuid().GetHashCode());
    Console.WriteLine("First 5 random number in rnd1 using Guid");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd1.Next());
    Console.WriteLine("First 5 random number in rnd2 using Guid");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd2.Next());

Outra maneira de obter sementes diferentes é usar outra instância `Random` para recuperar os valores de semente.

    Random rndSeeds = new Random();
    Random rnd1 = new Random(rndSeeds.Next());
    Random rnd2 = new Random(rndSeeds.Next());
Isso também torna possível controlar o resultado de todas as instâncias `Random` definindo apenas o valor de semente para `rndSeeds`. Todas as outras instâncias serão derivadas deterministicamente desse valor de semente único.

## Gerar um duplo aleatório
Gere um número aleatório entre 0 e 1,0. (não incluindo 1.0)

    Random rnd = new Random();
    var randomDouble = rnd.NextDouble();



## Gera um caractere aleatório
Gere uma letra aleatória entre `a` e `z` usando a sobrecarga `Next()` para um determinado intervalo de números e, em seguida, convertendo o `int` resultante em um `char`

    Random rnd = new Random();
    char randomChar = (char)rnd.Next('a','z'); 
    //'a' and 'z' are interpreted as ints for parameters for Next()
    

## Gera um número que é uma porcentagem de um valor máximo
Uma necessidade comum de números aleatórios é gerar um número que seja `X%` de algum valor máximo. isso pode ser feito tratando o resultado de `NextDouble()` como uma porcentagem:

    var rnd = new Random();
    var maxValue = 5000;
    var percentage = rnd.NextDouble();
    var result = maxValue * percentage; 
    //suppose NextDouble() returns .65, result will hold 65% of 5000: 3250.



