---
title: "Programación funcional"
slug: "programacion-funcional"
draft: false
images: []
weight: 9866
type: docs
toc: true
---

## Función y Acción
**Func** proporciona un soporte para funciones anónimas parametrizadas. Los tipos principales son las entradas y el último tipo es siempre el valor devuelto.

    // square a number.
    Func<double, double> square = (x) => { return x * x; };

    // get the square root.
    // note how the signature matches the built in method.
    Func<double, double> squareroot = Math.Sqrt;

    // provide your workings.
    Func<double, double, string> workings = (x, y) => 
        string.Format("The square of {0} is {1}.", x, square(y))
        
Los objetos **Acción** son como métodos vacíos, por lo que solo tienen un tipo de entrada. No se coloca ningún resultado en la pila de evaluación.

    // right-angled triangle.
    class Triangle
    {
        public double a;
        public double b;
        public double h;
    }

    // Pythagorean theorem.
    Action<Triangle> pythagoras = (x) => 
        x.h = squareroot(square(x.a) + square(x.b));
    
    Triangle t = new Triangle { a = 3, b = 4 };
    pythagoras(t);
    Console.WriteLine(t.h); // 5.



## Evite las referencias nulas
Los desarrolladores de C# obtienen muchas excepciones de referencias nulas con las que lidiar. Los desarrolladores de F# no porque tienen el tipo de opción. Un tipo Opción<> (algunos prefieren Quizás<> como nombre) proporciona un tipo de retorno Algunos y Ninguno. Hace explícito que un método puede estar a punto de devolver un registro nulo.

Por ejemplo, no puede leer lo siguiente y saber si tendrá que lidiar con un valor nulo.

    var user = _repository.GetUser(id);

Si conoce el posible nulo, puede introducir algún código repetitivo para tratarlo.

    var username = user != null ? user.Name : string.Empty;

¿Qué sucede si tenemos una opción <> devuelta en su lugar?

    Option<User> maybeUser = _repository.GetUser(id);

El código ahora hace explícito que es posible que se devuelva un registro Ninguno y se requiere el código repetitivo para verificar Algunos o Ninguno:

    var username = maybeUser.HasValue ? maybeUser.Value.Name : string.Empty;

El siguiente método muestra cómo devolver una opción <>

    public Option<User> GetUser(int id)
    {
        var users = new List<User>
        {
            new User { Id = 1, Name = "Joe Bloggs" },
            new User { Id = 2, Name = "John Smith" }
        };
    
        var user = users.FirstOrDefault(user => user.Id == id);
    
        return user != null ? new Option<User>(user) : new Option<User>();
    }

Aquí hay una implementación mínima de Option<>.

    public struct Option<T>
    {
        private readonly T _value;
    
        public T Value
        {
            get
            {
                if (!HasValue)
                    throw new InvalidOperationException();

                return _value;
            }
        }

        public bool HasValue
        {
            get { return _value != null; }
        }
    
        public Option(T value)
        {
            _value = value;
        }
    
        public static implicit operator Option<T>(T value)
        {
            return new Option<T>(value);
        }
    }

Para demostrar lo anterior, [avoidNull.csx][1] se puede ejecutar con C# REPL.

Como se ha dicho, esta es una implementación mínima. Una búsqueda de [paquetes NuGet "Quizás"] [2] arrojará una serie de buenas bibliotecas.


[1]: https://gist.github.com/Boggin/d53660f32aeaa35e0b028919ddc465e3
[2]: https://www.nuget.org/packages?q=tal vez

## Funciones de orden superior
Una función de orden superior es aquella que toma otra función como argumento o devuelve una función (o ambas).

Esto se hace comúnmente con lambdas, por ejemplo, cuando se pasa un predicado a una cláusula Where de LINQ:

    var results = data.Where(p => p.Items == 0);

La cláusula Where() podría recibir muchos predicados diferentes, lo que le da una flexibilidad considerable.

Pasar un método a otro método también se ve cuando se implementa el patrón de diseño de Estrategia. Por ejemplo, se pueden elegir varios métodos de clasificación y pasarlos a un método de clasificación en un objeto según los requisitos en tiempo de ejecución.

## Inmutabilidad
La inmutabilidad es común en la programación funcional y rara en la programación orientada a objetos.

Cree, por ejemplo, un tipo de dirección con estado mutable:

    public class Address () 
    {
        public string Line1 { get; set; }
        public string Line2 { get; set; }
        public string City  { get; set; }
    }

Cualquier pieza de código podría alterar cualquier propiedad en el objeto anterior.

Ahora cree el tipo de dirección inmutable:

    public class Address () 
    {
        public readonly string Line1;
        public readonly string Line2;
        public readonly string City;

        public Address(string line1, string line2, string city) 
        {
            Line1 = line1;
            Line2 = line2;
            City  = city;
        }
    }

Tenga en cuenta que tener colecciones de solo lectura no respeta la inmutabilidad. Por ejemplo,

    public class Classroom
    {
        public readonly List<Student> Students;
        
        public Classroom(List<Student> students)
        {
            Students = students;
        }
    }

no es inmutable, ya que el usuario del objeto puede modificar la colección (añadir o eliminar elementos de ella). Para hacerlo inmutable, uno tiene que usar una interfaz como IEnumerable<Student>, que no expone métodos para agregar, o convertirlo en ReadOnlyCollection<Student>.

    public class Classroom
    {
        public readonly ReadOnlyCollection<Student> Students;

        public Classroom(ReadOnlyCollection<Student> students)
        {
            Students = students;
        }
    }

    List<Students> list = new List<Student>();
    // add students
    Classroom c = new Classroom(list.AsReadOnly());   


Con el objeto inmutable tenemos los siguientes beneficios:

- Estará en un estado conocido (otro código no puede cambiarlo).
- Es seguro para subprocesos.
- El constructor ofrece un único lugar para la validación.
- Saber que el objeto no se puede alterar hace que el código sea más fácil de entender.

## Colecciones inmutables
El paquete NuGet [`System.Collections.Immutable`][1] proporciona clases de colección inmutables.

# Crear y agregar elementos

    var stack = ImmutableStack.Create<int>();
    var stack2 = stack.Push(1); // stack is still empty, stack2 contains 1
    var stack3 = stack.Push(2); // stack2 still contains only one, stack3 has 2, 1

# Creando usando el constructor

Ciertas colecciones inmutables tienen una clase interna `Builder` que se puede usar para construir instancias inmutables grandes de forma económica:

    var builder = ImmutableList.CreateBuilder<int>(); // returns ImmutableList.Builder
    builder.Add(1);
    builder.Add(2);
    var list = builder.ToImmutable();

# Creando desde un IEnumerable existente

    var numbers = Enumerable.Range(1, 5);
    var list = ImmutableList.CreateRange<int>(numbers);

Lista de todos los tipos de colecciones inmutables:

- [`Sistema.Colecciones.Immutable.ImmutableArray<T>`][2]
- [`System.Collections.Immutable.ImmutableDictionary<TKey,TValue>`][3]
- [`Sistema.Colecciones.Immutable.ImmutableHashSet<T>`][4]
- [`Sistema.Colecciones.Immutable.ImmutableList<T>`][5]
- [`Sistema.Colecciones.Immutable.ImmutableQueue<T>`][6]
- [`System.Collections.Immutable.ImmutableSortedDictionary<TKey,TValue>`][7]
- [`System.Collections.Immutable.ImmutableSortedSet<T>`][8]
- [`Sistema.Colecciones.Immutable.ImmutableStack<T>`][9]


[1]: https://www.nuget.org/packages/System.Collections.Immutable/
[2]: https://msdn.microsoft.com/en-us/library/dn638264(v=vs.111).aspx
[3]: https://msdn.microsoft.com/en-us/library/dn467181(v=vs.111).aspx
[4]: https://msdn.microsoft.com/en-us/library/dn467171(v=vs.111).aspx
[5]: https://msdn.microsoft.com/en-us/library/dn456077.aspx
[6]: https://msdn.microsoft.com/en-us/library/dn467186(v=vs.111).aspx
[7]: https://msdn.microsoft.com/en-us/library/dn467194(v=vs.111).aspx
[8]: https://msdn.microsoft.com/en-us/library/dn467193(v=vs.111).aspx
[9]: https://msdn.microsoft.com/en-us/library/dn467197(v=vs.111).aspx

