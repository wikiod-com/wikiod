---
title: "Programación Orientada a Objetos en C#"
slug: "programacion-orientada-a-objetos-en-c"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Este tema trata de decirnos cómo podemos escribir programas basados ​​en el enfoque OOP. Pero no tratamos de enseñar el paradigma de la Programación Orientada a Objetos.
Estaremos cubriendo los siguientes temas:
Clases, Propiedades, Herencia, Polimorfismo, Interfaces, etc.

## Clases:
El esqueleto de la clase declarante es:

<>:Requerido

[]:Opcional

    [private/public/protected/internal] class <Desired Class Name> [:[Inherited class][,][[Interface Name 1],[Interface Name 2],...]
    {
        //Your code
    }
No se preocupe si no puede entender toda la sintaxis, nos familiarizaremos con todo eso. Para el primer ejemplo, considere la siguiente clase:

    class MyClass
    {
        int i = 100;
        public void getMyValue()
        {
            Console.WriteLine(this.i);//Will print number 100 in output
        }
    }

en esta clase creamos la variable `i` con el tipo `int` y con [Modificadores de acceso] privados predeterminados (https://msdn.microsoft.com/en-us/library/ms173121.aspx) y el método `getMyValue()` con modificadores de acceso público.

