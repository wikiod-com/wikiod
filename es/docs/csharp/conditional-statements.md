---
title: "Declaraciones condicionales"
slug: "declaraciones-condicionales"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Declaración If-Else
La programación en general a menudo requiere una "decisión" o una "rama" dentro del código para explicar cómo funciona el código bajo diferentes entradas o condiciones. Dentro del lenguaje de programación C# (y la mayoría de los lenguajes de programación para este asunto), la forma más simple y, a veces, la más útil de crear una rama dentro de su programa es a través de una declaración 'If-Else'.

Supongamos que tenemos un método (también conocido como una función) que toma un parámetro int que representará una puntuación de hasta 100, y el método imprimirá un mensaje diciendo si aprobamos o fallamos.

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

Al mirar este método, puede notar esta línea de código (`score>= 50`) dentro de la declaración `If`. Esto se puede ver como una condición `booleana`, donde si la condición se evalúa como igual a `verdadero`, entonces se ejecuta el código que está entre `si` `{ }`.

Por ejemplo, si este método se llamara así:
`PrintPassOrFail(60);`, la salida del método sería un Console Print diciendo ***Pass!*** ya que el valor del parámetro de 60 es mayor o igual a 50.

Sin embargo, si el método fue llamado como: `PrintPassOrFail(30);`, la salida del método se imprimiría diciendo ***Fail!***. Esto se debe a que el valor 30 no es mayor ni igual a 50, por lo que el código entre `else` `{ }` se ejecuta en lugar de la instrucción `If`.

En este ejemplo, hemos dicho que la *puntuación* debería subir hasta 100, lo que no se ha tenido en cuenta en absoluto. Para dar cuenta de que la *puntuación* no pasa de 100 o posiblemente cae por debajo de 0, consulte el ejemplo **If-Else If-Else Statement**.

## Declaración If-Else If-Else
Siguiendo con el ejemplo de la **Declaración If-Else**, ahora es el momento de presentar la declaración `Else If`. La declaración `Else If` sigue directamente después de la declaración `If` en la estructura **If-Else If-Else**, pero intrínsecamente tiene una sintaxis similar a la declaración `If`. Se utiliza para agregar más ramas al código de las que puede agregar una simple instrucción **If-Else**.

En el ejemplo de **Declaración If-Else**, el ejemplo especificó que la puntuación sube a 100; sin embargo, nunca hubo controles contra esto. Para solucionar esto, modifiquemos el método de **Declaración If-Else** para que se vea así:

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

Todas estas declaraciones se ejecutarán en orden desde arriba hasta abajo hasta que se cumpla una condición. En esta nueva actualización del método, hemos agregado dos nuevas ramas para acomodar ahora la puntuación *fuera de los límites*.

Por ejemplo, si ahora llamamos al método en nuestro código como `PrintPassOFail(110);`, la salida sería un Console Print diciendo ***Error: ¡la puntuación es mayor que 100!***; y si llamamos al método en nuestro código como `PrintPassOrFail(-20);`, la salida diría ***Error: ¡la puntuación es inferior a 0!***.

## Si las condiciones de la declaración son valores y expresiones booleanas estándar
La siguiente declaración

    if (conditionA && conditionB && conditionC) //...
es exactamente equivalente a

    bool conditions = conditionA && conditionB && conditionC;
    if (conditions) // ...
en otras palabras, las condiciones dentro de la declaración "si" simplemente forman una expresión booleana ordinaria.

Un error común al escribir sentencias condicionales es comparar explícitamente con `verdadero` y `falso`:

    if (conditionA == true && conditionB == false && conditionC == true) // ...

Esto se puede reescribir como

    if (conditionA && !conditionB && conditionC)

## Cambiar declaraciones
Una declaración de cambio permite probar la igualdad de una variable con una lista de valores. Cada valor se denomina caso, y la variable que se activa se comprueba para cada caso de cambio.

Una instrucción `switch` suele ser más concisa y comprensible que las declaraciones `if...else if... else..` cuando se prueban múltiples valores posibles para una sola variable.

    
La sintaxis es la siguiente

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

hay varias cosas que deben tenerse en cuenta al usar la instrucción switch

- La expresión utilizada en una declaración de cambio debe tener un tipo integral o enumerado, o ser de un tipo de clase en el que la clase tiene una sola función de conversión a un tipo integral o enumerado.
- Puede tener cualquier número de declaraciones de casos dentro de un conmutador. Cada caso va seguido del valor con el que se va a comparar y dos puntos. Los valores para comparar tienen que ser únicos dentro de cada declaración de cambio.
- Una declaración de cambio puede tener un caso predeterminado opcional. El caso predeterminado se puede utilizar para realizar una tarea cuando ninguno de los casos es verdadero.
- Cada caso tiene que terminar con una declaración `break` a menos que sea una declaración vacía. En ese caso, la ejecución continuará en el caso que se encuentra debajo. La sentencia break también se puede omitir cuando se utiliza una sentencia `return`, `throw` o `goto case`.


Se puede dar un ejemplo con las calificaciones sabias

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

