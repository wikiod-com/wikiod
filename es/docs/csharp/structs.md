---
title: "estructuras"
slug: "estructuras"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

A diferencia de las clases, una `estructura` es un tipo de valor y se crea en la pila local y no en el montón administrado, *de ​​forma predeterminada*. Esto significa que una vez que la pila específica queda fuera del alcance, la "estructura" se desasigna. Los tipos de referencia contenidos de 'estructuras' desasignadas también se barren, una vez que el GC determina que la 'estructura' ya no hace referencia a ellos.

`struct`s no pueden heredar y no pueden ser bases para la herencia, están implícitamente sellados y tampoco pueden incluir miembros `protegidos`. Sin embargo, una `estructura` puede implementar una interfaz, como lo hacen las clases.

## Declarando una estructura
    public struct Vector 
    {
        public int X;
        public int Y;
        public int Z;
    }

    public struct Point
    {
        public decimal x, y;
        
        public Point(decimal pointX, decimal pointY)
        {
            x = pointX;
            y = pointY;
        }
    }

- Los campos de instancia de `struct` se pueden configurar a través de un constructor parametrizado o individualmente después de la construcción de `struct`.
- Los miembros privados solo pueden ser inicializados por el constructor.
- `struct` define un tipo sellado que hereda implícitamente de System.ValueType.
- Las estructuras no pueden heredar de ningún otro tipo, pero pueden implementar interfaces.
- Las estructuras se copian en la asignación, lo que significa que todos los datos se copian en la nueva instancia y los cambios en uno de ellos no se reflejan en el otro.
- Una estructura no puede ser `null`, aunque *puede* usarse como un tipo anulable:

       Vector v1 = null; //illegal
       Vector? v2 = null; //OK
       Nullable<Vector> v3 = null // OK

- Las estructuras se pueden instanciar con o sin usar el operador `nuevo`.

       //Both of these are acceptable
       Vector v1 = new Vector();
       v1.X = 1;
       v1.Y = 2;
       v1.Z = 3;
    
       Vector v2;
       v2.X = 1;
       v2.Y = 2;
       v2.Z = 3;

    However, the `new` operator must be used in order to use an initializer:

       Vector v1 = new MyStruct { X=1, Y=2, Z=3 }; // OK
       Vector v2 { X=1, Y=2, Z=3 }; // illegal

Una estructura puede declarar todo lo que una clase puede declarar, con algunas excepciones:
- Una estructura no puede declarar un constructor sin parámetros. Los campos de instancia de `struct` se pueden configurar a través de un constructor parametrizado o individualmente después de la construcción de `struct`. Los miembros privados solo pueden ser inicializados por el constructor.
- Una estructura no puede declarar miembros como protegidos, ya que está sellada implícitamente.
- Los campos de estructura solo se pueden inicializar si son constantes o estáticos.

## Uso de estructuras
**Con constructor:**

    Vector v1 = new Vector();
    v1.X = 1;
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    // Output X=1,Y=2,Z=3

    Vector v1 = new Vector();
    //v1.X is not assigned
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    // Output X=0,Y=2,Z=3

    Point point1 = new Point();
    point1.x = 0.5;
    point1.y = 0.6;
    
    Point point2 = new Point(0.5, 0.6);

**Sin constructor:**

    Vector v1;
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    //Output ERROR "Use of possibly unassigned field 'X'

    Vector v1;
    v1.X = 1;
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    // Output X=1,Y=2,Z=3

    Point point3;
    point3.x = 0.5;
    point3.y = 0.6;

Si usamos una estructura con su constructor, no vamos a tener problemas con el campo sin asignar (cada campo sin asignar tiene un valor nulo).

A diferencia de las clases, no es necesario construir una estructura, es decir, no es necesario usar la nueva palabra clave, a menos que necesite llamar a uno de los constructores. Una estructura no requiere la nueva palabra clave porque es un tipo de valor y, por lo tanto, no puede ser nula.

## Interfaz de implementación de la estructura
    public interface IShape
    {
        decimal Area();
    }
    
    public struct Rectangle : IShape
    {
        public decimal Length { get; set; }
        public decimal Width { get; set; }
    
        public decimal Area()
        {
            return Length * Width;
        }
    }

## Las estructuras se copian en la asignación
Dado que las estructuras son tipos de valor, todos los datos se _copian_ en la asignación, y cualquier modificación a la nueva copia no cambia los datos de la copia original. El fragmento de código siguiente muestra que `p1` se _copió_ en `p2` y los cambios realizados en `p1` no afectan a la instancia de `p2`.

    var p1 = new Point {
        x = 1,
        y = 2
    };
    
    Console.WriteLine($"{p1.x} {p1.y}"); // 1 2
    
    var p2 = p1;
    Console.WriteLine($"{p2.x} {p2.y}"); // Same output: 1 2
    
    p1.x = 3;
    Console.WriteLine($"{p1.x} {p1.y}"); // 3 2
    Console.WriteLine($"{p2.x} {p2.y}"); // p2 remain the same: 1 2

