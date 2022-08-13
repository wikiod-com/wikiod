---
title: "Excepcion de referencia nula"
slug: "excepcion-de-referencia-nula"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Explicación de NullReferenceException
Se genera una `NullReferenceException` cuando intenta acceder a un miembro no estático (propiedad, método, campo o evento) de un objeto de referencia, pero es nulo.

    Car myFirstCar = new Car();
    Car mySecondCar = null;
    Color myFirstColor = myFirstCar.Color; // No problem as myFirstCar exists / is not null
    Color mySecondColor = mySecondCar.Color; // Throws a NullReferenceException 
    // as mySecondCar is null and yet we try to access its color.

Para depurar una excepción de este tipo, es bastante fácil: en la línea donde se lanza la excepción, solo tiene que buscar antes de cada ''.`' o ''`[`', o en raras ocasiones ''`(`'.

    myGarage.CarCollection[currentIndex.Value].Color = theCarInTheStreet.Color;

¿De dónde viene mi excepción?
O:

- `miGaraje` es `nulo`
- `myGarage.CarCollection` es `null`
- `currentIndex` es `nulo`
- `myGarage.CarCollection[currentIndex.Value]` es `null`
- `theCarInTheStreet` es `null`

En el modo de depuración, solo tiene que colocar el cursor del mouse sobre cada uno de estos elementos y encontrará su referencia nula. Entonces, lo que hay que hacer es entender por qué no tiene valor. La corrección depende totalmente del objetivo de su método.

¿Ha olvidado crear una instancia/inicializarlo?

    myGarage.CarCollection = new Car[10];

¿Se supone que debes hacer algo diferente si el objeto es nulo?

    if (myGarage == null)
    {
        Console.WriteLine("Maybe you should buy a garage first!");
    }

O tal vez alguien te dio un argumento nulo y se suponía que no debía:

    if (theCarInTheStreet == null)
    {
        throw new ArgumentNullException("theCarInTheStreet");
    }
En cualquier caso, recuerda que un método nunca debe lanzar una NullReferenceException. Si es así, significa que se ha olvidado de comprobar algo.

