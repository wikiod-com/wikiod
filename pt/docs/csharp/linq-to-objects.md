---
title: "Linq para objetos"
slug: "linq-para-objetos"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

LINQ to Objects refere-se ao uso de consultas LINQ com qualquer coleção IEnumerable.

## Usando LINQ to Objects em C#
**Uma consulta SELECT simples no Linq**

    static void Main(string[] args)
    {
        string[] cars = { "VW Golf", 
                            "Opel Astra", 
                            "Audi A4", 
                            "Ford Focus", 
                            "Seat Leon", 
                            "VW Passat", 
                            "VW Polo", 
                            "Mercedes C-Class" };

        var list = from car in cars
                   select car;

        StringBuilder sb = new StringBuilder();

        foreach (string entry in list)
        {
            sb.Append(entry + "\n");
        }

        Console.WriteLine(sb.ToString());
        Console.ReadLine();
    }

No exemplo acima, uma matriz de strings (cars) é usada como uma coleção de objetos a serem consultados usando LINQ. Em uma consulta LINQ, a cláusula from vem primeiro para apresentar a fonte de dados (cars) e a variável de intervalo (car). Quando a consulta for executada, a variável range servirá como referência para cada elemento sucessivo nos carros. Como o compilador pode inferir o tipo de carro, você não precisa especificá-lo explicitamente

Quando o código acima é compilado e executado, ele produz o seguinte resultado:
[![digite a descrição da imagem aqui][1]][1]

**SELECT com uma cláusula WHERE**

    var list = from car in cars
               where car.Contains("VW")
               select car;

A cláusula WHERE é usada para consultar o array de strings (cars) para encontrar e retornar um subconjunto de array que satisfaça a cláusula WHERE.

Quando o código acima é compilado e executado, ele produz o seguinte resultado:

[![digite a descrição da imagem aqui][2]][2]


**Gerando uma lista ordenada**

    var list = from car in cars
               orderby car ascending 
               select car;

Às vezes é útil classificar os dados retornados. A cláusula orderby fará com que os elementos sejam classificados de acordo com o comparador padrão para o tipo que está sendo classificado.

Quando o código acima é compilado e executado, ele produz o seguinte resultado:

[![digite a descrição da imagem aqui][3]][3]


**Trabalhando com um tipo personalizado**

Neste exemplo, uma lista digitada é criada, preenchida e consultada

    public class Car
    {
        public String Name { get; private set; }
        public int UnitsSold { get; private set; }

        public Car(string name, int unitsSold)
        {
            Name = name;
            UnitsSold = unitsSold;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {

            var car1 = new Car("VW Golf", 270952);
            var car2 = new Car("Opel Astra", 56079);
            var car3 = new Car("Audi A4", 52493);
            var car4 = new Car("Ford Focus", 51677);
            var car5 = new Car("Seat Leon", 42125);
            var car6 = new Car("VW Passat", 97586);
            var car7 = new Car("VW Polo", 69867);
            var car8 = new Car("Mercedes C-Class", 67549);

            var cars = new List<Car> { 
                car1, car2, car3, car4, car5, car6, car7, car8 };
            var list = from car in cars
                       select car.Name;

            foreach (var entry in list)
            {
                Console.WriteLine(entry);
            }
            Console.ReadLine();
        }
    }

Quando o código acima é compilado e executado, ele produz o seguinte resultado:

[![digite a descrição da imagem aqui][4]][4]


Até agora, os exemplos não parecem incríveis, pois é possível iterar pelo array para fazer basicamente o mesmo. No entanto, com os poucos exemplos abaixo, você pode ver como criar consultas mais complexas com LINQ to Objects e obter mais com muito menos código.

No exemplo abaixo, podemos selecionar carros que foram vendidos acima de 60.000 unidades e classificá-los pelo número de unidades vendidas:

    var list = from car in cars
               where car.UnitsSold > 60000 
               orderby car.UnitsSold descending 
               select car;

    StringBuilder sb = new StringBuilder();

    foreach (var entry in list)
    {
        sb.AppendLine($"{entry.Name} - {entry.UnitsSold}");
    }
    Console.WriteLine(sb.ToString());

Quando o código acima é compilado e executado, ele produz o seguinte resultado:
[![digite a descrição da imagem aqui][5]][5]


No exemplo abaixo, podemos selecionar carros que venderam um número ímpar de unidades e ordená-los em ordem alfabética sobre seu nome:

    var list = from car in cars
               where car.UnitsSold % 2 != 0 
               orderby car.Name ascending 
               select car;

Quando o código acima é compilado e executado, ele produz o seguinte resultado:
[![digite a descrição da imagem aqui][6]][6]


[1]: https://i.stack.imgur.com/lG65Q.png
[2]: https://i.stack.imgur.com/llGXx.png
[3]: https://i.stack.imgur.com/ODH55.png
[4]: https://i.stack.imgur.com/0jUOC.png
[5]: https://i.stack.imgur.com/ZDeTt.png
[6]: https://i.stack.imgur.com/fJnTp.png

## Como o LINQ to Object executa consultas
As consultas LINQ não são executadas imediatamente. Quando você está construindo a consulta, você está simplesmente armazenando a consulta para execução futura. Somente quando você realmente solicita a iteração da consulta, a consulta é executada (por exemplo, em um loop for, ao chamar ToList, Count, Max, Average, First etc.)

Isso é considerado *execução adiada*. Isso permite que você construa a consulta em várias etapas, potencialmente modificando-a com base em instruções condicionais e, em seguida, execute-a posteriormente apenas quando precisar do resultado.

Dado o código:

    var query = from n in numbers 
                where n % 2 != 0
                select n;

O exemplo acima armazena apenas a consulta na variável `query`. Ele não executa a consulta em si.

A instrução `foreach` força a execução da consulta:

    foreach(var n in query) {
        Console.WriteLine($"Number selected {n}");
    }

Alguns métodos LINQ também acionarão a execução da consulta, `Count`, `First`, `Max`, `Average`. Eles retornam valores únicos. `ToList` e `ToArray` coletam os resultados e os transformam em uma lista ou matriz, respectivamente.

Esteja ciente de que é possível iterar na consulta várias vezes se você chamar várias funções LINQ na mesma consulta. Isso pode lhe dar resultados diferentes em cada chamada. Se você quiser trabalhar apenas com um conjunto de dados, salve-o em uma lista ou array.



