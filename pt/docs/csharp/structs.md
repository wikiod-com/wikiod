---
title: "Estruturas"
slug: "estruturas"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Ao contrário das classes, um `struct` é um tipo de valor e é criado na pilha local e não no heap gerenciado, *por padrão*. Isso significa que uma vez que a pilha específica sai do escopo, o `struct` é desalocado. Os tipos de referência contidos de `struct`s desalocados também são varridos, uma vez que o GC determina que eles não são mais referenciados pelo `struct`.

`struct`s não podem herdar e não podem ser bases para herança, eles são selados implicitamente e também não podem incluir membros `protected`. No entanto, um `struct` pode implementar uma interface, como as classes fazem.

## Declarando uma estrutura
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

- Campos de instância `struct` podem ser definidos através de um construtor parametrizado ou individualmente após a construção `struct`.
- Membros privados só podem ser inicializados pelo construtor.
- `struct` define um tipo selado que herda implicitamente de System.ValueType.
- Structs não podem herdar de nenhum outro tipo, mas podem implementar interfaces.
- As estruturas são copiadas na atribuição, o que significa que todos os dados são copiados para a nova instância e as alterações em uma delas não são refletidas na outra.
- Um struct não pode ser `null`, embora *possa* ser usado como um tipo anulável:

       Vector v1 = null; //illegal
       Vector? v2 = null; //OK
       Nullable<Vector> v3 = null // OK

- Estruturas podem ser instanciadas com ou sem o uso do operador `new`.

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

Uma struct pode declarar tudo o que uma classe pode declarar, com algumas exceções:
- Um struct não pode declarar um construtor sem parâmetros. Os campos de instância `struct` podem ser definidos por meio de um construtor parametrizado ou individualmente após a construção `struct`. Membros privados só podem ser inicializados pelo construtor.
- Um struct não pode declarar membros como protegidos, pois é selado implicitamente.
- Os campos struct só podem ser inicializados se forem const ou estáticos.

## Uso da estrutura
**Com construtor:**

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

**Sem construtor:**

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

Se usarmos um struct com seu construtor, não teremos problemas com campo não atribuído (cada campo não atribuído tem valor nulo).

Ao contrário das classes, uma struct não precisa ser construída, ou seja, não há necessidade de usar a palavra-chave new, a menos que você precise chamar um dos construtores. Um struct não requer a palavra-chave new porque é um tipo de valor e, portanto, não pode ser nulo.

## Interface de implementação de estrutura
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

## As estruturas são copiadas na atribuição
Se as estruturas são tipos de valor, todos os dados são _copiados_ na atribuição e qualquer modificação na nova cópia não altera os dados da cópia original. O trecho de código abaixo mostra que `p1` é _copiado_ para `p2` e as alterações feitas em `p1` não afetam a instância `p2`.

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

