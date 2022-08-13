---
title: "Constructor de cadenas"
slug: "constructor-de-cadenas"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Qué es un StringBuilder y cuándo usarlo
Un [`StringBuilder`][1] representa una serie de caracteres que, a diferencia de una cadena normal, son mutables. Muchas veces es necesario modificar cadenas que ya hemos creado, pero el objeto de cadena estándar no es mutable. Esto significa que cada vez que se modifica una cadena, se debe crear, copiar y luego reasignar un nuevo objeto de cadena.

    string myString = "Apples";
    mystring += " are my favorite fruit";

En el ejemplo anterior, `myString` inicialmente solo tiene el valor `"Apples"`. Sin embargo, cuando concatenamos `" son mi fruta favorita"', lo que la clase de cadena debe hacer internamente implica:

- Creando una nueva matriz de caracteres igual a la longitud de `myString` y la nueva cadena que estamos agregando.
- Copiar todos los caracteres de `myString` al comienzo de nuestra nueva matriz y copiar la nueva cadena al final de la matriz.
- Crear un nuevo objeto de cadena en la memoria y reasignarlo a `myString`.

Para una sola concatenación, esto es relativamente trivial. Sin embargo, ¿qué sucede si es necesario realizar muchas operaciones de adición, por ejemplo, en un bucle?

    String myString = "";
    for (int i = 0; i < 10000; i++)
        myString += " "; // puts 10,000 spaces into our string

Debido a la copia repetida y la creación de objetos, esto degradará significativamente el rendimiento de nuestro programa. Podemos evitar esto usando un `StringBuilder`.

    StringBuilder myStringBuilder = new StringBuilder();    
    for (int i = 0; i < 10000; i++)
        myStringBuilder.Append(' ');

Ahora, cuando se ejecuta el mismo bucle, el rendimiento y la velocidad del tiempo de ejecución del programa serán significativamente más rápidos que con una cadena normal. Para hacer que `StringBuilder` vuelva a ser una cadena normal, simplemente podemos llamar al método `ToString()` de `StringBuilder`.


----------
Sin embargo, esta no es la única optimización que tiene `StringBuilder`. Para optimizar aún más las funciones, podemos aprovechar otras propiedades que ayudan a mejorar el rendimiento.

    StringBuilder sb = new StringBuilder(10000); // initializes the capacity to 10000

Si sabemos de antemano cuánto debe durar nuestro `StringBuilder`, podemos especificar su tamaño con anticipación, lo que evitará que necesite cambiar el tamaño de la matriz de caracteres que tiene internamente.

    sb.Append('k', 2000);

Aunque usar `StringBuilder` para agregar es mucho más rápido que una cadena, puede ejecutarse aún más rápido si solo necesita agregar un solo carácter muchas veces.

Una vez que haya completado la construcción de su cadena, puede usar el método `ToString()` en `StringBuilder` para convertirla en una `cadena` básica. Esto suele ser necesario porque la clase `StringBuilder` no hereda de `string`.

Por ejemplo, así es como puedes usar un `StringBuilder` para crear una `cadena`:

    string RepeatCharacterTimes(char character, int times)
    {
        StringBuilder builder = new StringBuilder("");
        for (int counter = 0; counter < times; counter++)
        {
            //Append one instance of the character to the StringBuilder.
            builder.Append(character);
        }
        //Convert the result to string and return it.
        return builder.ToString();
    }

----------
En conclusión, `StringBuilder` debe usarse en lugar de una cadena cuando se deben realizar muchas modificaciones a una cadena teniendo en cuenta el rendimiento.


[1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

## Use StringBuilder para crear una cadena a partir de una gran cantidad de registros
    public string GetCustomerNamesCsv()
    {
        List<CustomerData> customerDataRecords = GetCustomerData(); // Returns a large number of records, say, 10000+
    
        StringBuilder customerNamesCsv = new StringBuilder();
        foreach (CustomerData record in customerDataRecords)
        {
           customerNamesCsv
               .Append(record.LastName)
               .Append(',')
               .Append(record.FirstName)
               .Append(Environment.NewLine);
        }

        return customerNamesCsv.ToString();
    }


