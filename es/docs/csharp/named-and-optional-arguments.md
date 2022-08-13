---
title: "Argumentos con nombre y opcionales"
slug: "argumentos-con-nombre-y-opcionales"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

**Argumentos con nombre**

*Ref: MSDN* Los argumentos con nombre le permiten especificar un argumento para un parámetro en particular al asociar el argumento con el nombre del parámetro en lugar de con la posición del parámetro en la lista de parámetros.

Como dijo MSDN, un argumento con nombre,

- Le permite pasar el argumento a la función asociando el
nombre del parámetro.
- No es necesario recordar la posición de los parámetros que no estamos.
consciente de siempre.
- No es necesario buscar el orden de los parámetros en la lista de parámetros de
llamada función.
- Podemos especificar parámetros para cada argumento por su nombre.

**Argumentos opcionales**

*Ref: MSDN* La definición de un método, constructor, indexador o delegado puede especificar que sus parámetros son obligatorios o que son opcionales. Cualquier llamada debe proporcionar argumentos para todos los parámetros requeridos, pero puede omitir argumentos para parámetros opcionales.

Como dice MSDN, un argumento opcional,

- Podemos omitir el argumento en la llamada si ese argumento es Opcional
Argumento
- Cada argumento opcional tiene su propio valor predeterminado
- Tomará el valor predeterminado si no proporcionamos el valor
- Un valor predeterminado de un argumento opcional debe ser un
- Expresión constante.
- Debe ser un tipo de valor como enum o struct.
- Debe ser una expresión de la forma predeterminada (valueType)
- Debe establecerse al final de la lista de parámetros.

## Argumentos opcionales
Considere lo anterior es nuestra definición de función con argumentos opcionales.

    private static double FindAreaWithOptional(int length, int width=56)
           {
               try
               {
                   return (length * width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

Aquí establecimos el valor de ancho como opcional y le dimos un valor de 56. Si observa, el propio IntelliSense le muestra el argumento opcional como se muestra en la imagen a continuación.

[![ingrese la descripción de la imagen aquí][1]][1]

    Console.WriteLine("Area with Optional Argument : ");
    area = FindAreaWithOptional(120);
    Console.WriteLine(area);
    Console.Read();

Tenga en cuenta que no obtuvimos ningún error durante la compilación y le dará un resultado de la siguiente manera.

[![ingrese la descripción de la imagen aquí][2]][2]



**Usando atributo opcional.**

Otra forma de implementar el argumento opcional es usando la palabra clave `[Opcional]`. Si no pasa el valor para el argumento opcional, el valor predeterminado de ese tipo de datos se asigna a ese argumento. La palabra clave `Opcional` está presente en el espacio de nombres "Runtime.InteropServices".

    using System.Runtime.InteropServices;  
    private static double FindAreaWithOptional(int length, [Optional]int width)
       {
           try
           {
               return (length * width);
           }
           catch (Exception)
           {
               throw new NotImplementedException();
           }
       } 

    area = FindAreaWithOptional(120);  //area=0
Y cuando llamamos a la función, obtenemos 0 porque no se pasa el segundo argumento y el valor predeterminado de int es 0, por lo que el producto es 0.
    


[1]: http://i.stack.imgur.com/Uaszw.png
[2]: http://i.stack.imgur.com/3BWQA.png

## Argumentos con nombre
Considere lo siguiente es nuestra llamada de función.

    FindArea(120, 56);
En esto, nuestro primer argumento es la longitud (es decir, 120) y el segundo argumento es el ancho (es decir, 56). Y estamos calculando el área por esa función. Y a continuación se muestra la definición de la función.

    private static double FindArea(int length, int width)
           {
               try
               {
                   return (length* width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

Entonces, en la primera llamada a la función, simplemente pasamos los argumentos por su posición. ¿Derecha?

    double area;
    Console.WriteLine("Area with positioned argument is: ");
    area = FindArea(120, 56);
    Console.WriteLine(area);
    Console.Read();
Si ejecuta esto, obtendrá una salida de la siguiente manera.

[![ingrese la descripción de la imagen aquí][1]][1]

Ahora aquí vienen las características de los argumentos con nombre. Consulte la llamada de función anterior.


    Console.WriteLine("Area with Named argument is: ");
    area = FindArea(length: 120, width: 56);
    Console.WriteLine(area);
    Console.Read();

Aquí estamos dando los argumentos con nombre en la llamada al método.

    area = FindArea(length: 120, width: 56);
Ahora, si ejecuta este programa, obtendrá el mismo resultado. Podemos dar los nombres viceversa en la llamada al método si estamos usando los argumentos con nombre.

    Console.WriteLine("Area with Named argument vice versa is: ");
    area = FindArea(width: 120, length: 56);
    Console.WriteLine(area);
    Console.Read();

Uno de los usos importantes de un argumento con nombre es que cuando lo usa en su programa, mejora la legibilidad de su código. Simplemente dice cuál debe ser su argumento, o ¿qué es?.

También puede dar los argumentos posicionales. Eso significa, una combinación de argumento posicional y argumento con nombre.

    Console.WriteLine("Area with Named argument Positional Argument : ");
                area = FindArea(120, width: 56);
                Console.WriteLine(area);
                Console.Read();

En el ejemplo anterior, pasamos 120 como longitud y 56 como argumento con nombre para el ancho del parámetro.

También hay algunas limitaciones. Discutiremos la limitación de los argumentos con nombre ahora.

**Limitación del uso de un argumento con nombre**

La especificación de argumento con nombre debe aparecer después de que se hayan especificado todos los argumentos fijos.

Si usa un argumento con nombre antes de un argumento fijo, obtendrá un error de tiempo de compilación de la siguiente manera.

[![ingrese la descripción de la imagen aquí][2]][2]

La especificación de argumento con nombre debe aparecer después de que se hayan especificado todos los argumentos fijos


[1]: http://i.stack.imgur.com/aCYyR.png
[2]: http://i.stack.imgur.com/n8z4Y.png

