---
title: "Generando Números Aleatorios en C#"
slug: "generando-numeros-aleatorios-en-c"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

## Sintaxis
- Aleatorio()

- Aleatorio (semilla int)

- int Siguiente()

- int Siguiente (int maxValue)

- int Siguiente (int minValue, int maxValue)


## Parámetros
| Parámetros | Detalles |
| ---------- | ------- |
| Semilla | Un valor para generar números aleatorios. Si no se establece, el valor predeterminado está determinado por la hora actual del sistema.
| valor mínimo | Los números generados no serán más pequeños que este valor. Si no se establece, el valor predeterminado es 0.
| maxValor | Los números generados serán más pequeños que este valor. Si no se establece, el valor predeterminado es `Int32.MaxValue`.
| valor de retorno | Devuelve un número con valor aleatorio.

La semilla aleatoria generada por el sistema no es la misma en cada ejecución diferente.

Las semillas generadas al mismo tiempo pueden ser las mismas.

## Genera un int aleatorio
Este ejemplo genera valores aleatorios entre 0 y 2147483647.

    Random rnd = new Random();
    int randomNumber = rnd.Next();

## Genera un int aleatorio en un rango dado
Genera un número aleatorio entre `minValue` y `maxValue - 1`.

    Random rnd = new Random();
    var randomBetween10And20 = rnd.Next(10, 20);

## Generar la misma secuencia de números aleatorios una y otra vez
Al crear instancias `Random` con la misma semilla, se generarán los mismos números.

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

Producción:

    Random instance 0
    726643700 610783965 564707973 1342984399 995276750
    Random instance 1
    726643700 610783965 564707973 1342984399 995276750

## Crea múltiples clases aleatorias con diferentes semillas simultáneamente
Dos clases aleatorias creadas al mismo tiempo tendrán el mismo valor inicial.

El uso de `System.Guid.NewGuid().GetHashCode()` puede obtener una semilla diferente incluso al mismo tiempo.

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

Otra forma de lograr semillas diferentes es usar otra instancia `Random` para recuperar los valores de semilla.

    Random rndSeeds = new Random();
    Random rnd1 = new Random(rndSeeds.Next());
    Random rnd2 = new Random(rndSeeds.Next());
Esto también hace posible controlar el resultado de todas las instancias `Random` configurando solo el valor inicial para `rndSeeds`. Todas las demás instancias se derivarán de forma determinista de ese único valor semilla.

## Generar un doble aleatorio
Genera un número aleatorio entre 0 y 1.0. (sin incluir 1.0)

    Random rnd = new Random();
    var randomDouble = rnd.NextDouble();



## Generar un personaje aleatorio
Genere una letra aleatoria entre `a` y `z` usando la sobrecarga `Next()` para un rango dado de números, luego convierta el `int` resultante en un `char`

    Random rnd = new Random();
    char randomChar = (char)rnd.Next('a','z'); 
    //'a' and 'z' are interpreted as ints for parameters for Next()
    

## Genera un número que es un porcentaje de un valor máximo
Una necesidad común de números aleatorios es generar un número que sea `X%` de algún valor máximo. esto se puede hacer tratando el resultado de `NextDouble()` como un porcentaje:

    var rnd = new Random();
    var maxValue = 5000;
    var percentage = rnd.NextDouble();
    var result = maxValue * percentage; 
    //suppose NextDouble() returns .65, result will hold 65% of 5000: 3250.



