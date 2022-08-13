---
title: "Manejo de FormatException al convertir cadenas a otros tipos"
slug: "manejo-de-formatexception-al-convertir-cadenas-a-otros-tipos"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Convirtiendo cadena a entero
Hay varios métodos disponibles para convertir explícitamente una 'cadena' en un 'entero', como:
1. `Convertir.ToInt16();`

2. `Convertir.ToInt32();`

3. `Convertir.ToInt64();`

4. `int.Parse();`

Pero todos estos métodos generarán una `FormatException`, si la cadena de entrada contiene caracteres no numéricos. Para esto, necesitamos escribir un manejo de excepciones adicional (`try..catch`) para manejarlas en tales casos.

<hr/>
 
**Explicación con ejemplos:**

Entonces, dejemos que nuestra entrada sea:

    string inputString = "10.2";


**Ejemplo 1:** `Convert.ToInt32()`

    int convertedInt = Convert.ToInt32(inputString); // Failed to Convert 
    // Throws an Exception "Input string was not in a correct format."

***Nota:** Lo mismo ocurre con los otros métodos mencionados, a saber, `Convert.ToInt16();` y `Convert.ToInt64();`*
 

**Ejemplo 2:** `int.Parse()`

    int convertedInt = int.Parse(inputString); // Same result "Input string was not in a correct format.

***¿Cómo evitamos esto?***

Como se dijo anteriormente, para manejar las excepciones generalmente necesitamos un `try..catch` como se muestra a continuación:

    try
    {
        string inputString = "10.2";
        int convertedInt = int.Parse(inputString);
    }
    catch (Exception Ex)
    {
        //Display some message, that the conversion has failed.         
    }
Pero, usar `try..catch` en todas partes no será una buena práctica, y puede haber algunos escenarios en los que queramos dar `0` si la entrada es incorrecta, _ (si seguimos el método anterior, debemos asignar `0` a `convertedInt` del bloque catch)._
Para manejar estos escenarios podemos hacer uso de un método especial llamado `.TryParse()`.

El método `.TryParse()` tiene un manejo interno de excepciones, que le dará la salida al parámetro `out` y devuelve un valor booleano que indica el estado de conversión _(`true` si la conversión fue exitosa; `false` si falló)._ Según el valor devuelto, podemos determinar el estado de la conversión. Veamos un ejemplo:

**Uso 1:** Almacenar el valor devuelto en una variable booleana

     int convertedInt; // Be the required integer
     bool isSuccessConversion = int.TryParse(inputString, out convertedInt);
Podemos verificar la variable `isSuccessConversion` después de la ejecución para verificar el estado de la conversión. Si es falso, el valor de `convertedInt` será `0`_ (no es necesario comprobar el valor devuelto si desea `0` para el error de conversión)._

**Uso 2:** Comprobar el valor devuelto con `if`

    if (int.TryParse(inputString, out convertedInt))
    {
        // convertedInt will have the converted value
        // Proceed with that
    }
    else 
    {
     // Display an error message
    }
**Uso 3:** Sin comprobar el valor de retorno
puede usar lo siguiente, si no le importa el valor de retorno _(convertido o no, `0` estará bien)_

    int.TryParse(inputString, out convertedInt);
    // use the value of convertedInt
    // But it will be 0 if not converted

