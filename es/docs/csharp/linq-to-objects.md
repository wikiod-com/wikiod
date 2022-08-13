---
title: "Enlace a objetos"
slug: "enlace-a-objetos"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

LINQ to Objects se refiere al uso de consultas LINQ con cualquier colección IEnumerable.

## Uso de LINQ to Objects en C#
**Una simple consulta SELECT en Linq**

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

En el ejemplo anterior, se usa una matriz de cadenas (automóviles) como una colección de objetos que se consultarán mediante LINQ. En una consulta LINQ, la cláusula from viene primero para introducir la fuente de datos (automóviles) y la variable de rango (automóvil). Cuando se ejecuta la consulta, la variable de rango servirá como referencia para cada elemento sucesivo en los automóviles. Debido a que el compilador puede inferir el tipo de automóvil, no tiene que especificarlo explícitamente

Cuando el código anterior se compila y ejecuta, produce el siguiente resultado:
[![ingrese la descripción de la imagen aquí][1]][1]

**SELECCIONE con una cláusula WHERE**

    var list = from car in cars
               where car.Contains("VW")
               select car;

La cláusula WHERE se usa para consultar la matriz de cadenas (automóviles) para encontrar y devolver un subconjunto de la matriz que satisfaga la cláusula WHERE.

Cuando el código anterior se compila y ejecuta, produce el siguiente resultado:

[![ingrese la descripción de la imagen aquí][2]][2]


**Generando una Lista Ordenada**

    var list = from car in cars
               orderby car ascending 
               select car;

A veces es útil ordenar los datos devueltos. La cláusula orderby hará que los elementos se ordenen de acuerdo con el comparador predeterminado para el tipo que se ordena.

Cuando el código anterior se compila y ejecuta, produce el siguiente resultado:

[![ingrese la descripción de la imagen aquí][3]][3]


**Trabajando con un tipo personalizado**

En este ejemplo, se crea, completa y luego se consulta una lista escrita

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

Cuando el código anterior se compila y ejecuta, produce el siguiente resultado:

[![ingrese la descripción de la imagen aquí][4]][4]


Hasta ahora, los ejemplos no parecen sorprendentes, ya que uno puede simplemente iterar a través de la matriz para hacer básicamente lo mismo. Sin embargo, con los pocos ejemplos a continuación, puede ver cómo crear consultas más complejas con LINQ to Objects y lograr más con mucho menos código.

En el siguiente ejemplo, podemos seleccionar automóviles que se han vendido más de 60000 unidades y ordenarlos según el número de unidades vendidas:

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

Cuando el código anterior se compila y ejecuta, produce el siguiente resultado:
[![ingrese la descripción de la imagen aquí][5]][5]


En el siguiente ejemplo podemos seleccionar coches que han vendido un número impar de unidades y ordenarlos alfabéticamente sobre su nombre:

    var list = from car in cars
               where car.UnitsSold % 2 != 0 
               orderby car.Name ascending 
               select car;

Cuando el código anterior se compila y ejecuta, produce el siguiente resultado:
[![ingrese la descripción de la imagen aquí][6]][6]


[1]: https://i.stack.imgur.com/lG65Q.png
[2]: https://i.stack.imgur.com/llGXx.png
[3]: https://i.stack.imgur.com/ODH55.png
[4]: https://i.stack.imgur.com/0jUOC.png
[5]: https://i.stack.imgur.com/ZDeTt.png
[6]: https://i.stack.imgur.com/fJnTp.png

## Cómo LINQ to Object ejecuta consultas
Las consultas LINQ no se ejecutan inmediatamente. Cuando está creando la consulta, simplemente está almacenando la consulta para su ejecución futura. Solo cuando solicita iterar la consulta, se ejecuta la consulta (por ejemplo, en un bucle for, al llamar a ToList, Count, Max, Average, First, etc.)

Esto se considera *ejecución diferida*. Esto le permite construir la consulta en varios pasos, modificándola potencialmente en función de declaraciones condicionales, y luego ejecutarla más tarde solo cuando necesite el resultado.

Dado el código:

    var query = from n in numbers 
                where n % 2 != 0
                select n;

El ejemplo anterior solo almacena la consulta en la variable `query`. No ejecuta la consulta en sí.

La instrucción `foreach` fuerza la ejecución de la consulta:

    foreach(var n in query) {
        Console.WriteLine($"Number selected {n}");
    }

Algunos métodos LINQ también activarán la ejecución de la consulta, `Recuento`, `Primero`, `Máx.`, `Promedio`. Devuelven valores únicos. `ToList` y `ToArray` recopilan los resultados y los convierten en una lista o una matriz, respectivamente.

Tenga en cuenta que es posible iterar en la consulta varias veces si llama a varias funciones LINQ en la misma consulta. Esto podría darte resultados diferentes en cada llamada. Si solo desea trabajar con un conjunto de datos, asegúrese de guardarlo en una lista o matriz.



