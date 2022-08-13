---
title: "Es igual a y GetHashCode"
slug: "es-igual-a-y-gethashcode"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Cada implementación de `Equals` debe cumplir los siguientes requisitos:

- **Reflexivo**: Un objeto debe ser igual a sí mismo.<br/>`x.Equals(x)` devuelve `true`.

- **Simétrica**: no hay diferencia si comparo x con y o y con x; el resultado es el mismo. <br/>`x.Equals(y)` devuelve el mismo valor que `y.Equals(x)`.

- **Transitivo**: Si un objeto es igual a otro objeto y este es igual a un tercero, el primero tiene que ser igual al tercero.<br/>if `(x.Equals(y) && y .Equals(z))` devuelve `true`, luego `x.Equals(z)` devuelve `true`.

- **Coherente**: si compara un objeto con otro varias veces, el resultado siempre es el mismo.<br/>Las invocaciones sucesivas de `x.Equals(y)` devuelven el mismo valor siempre que los objetos a los que hace referencia x e y no se modifican.

- **Comparación con nulo**: Ningún objeto es igual a `nulo`.<br/>`x.Equals(null)` devuelve `falso`.

Implementaciones de `GetHashCode`:

- **Compatible con `Equals`**: si dos objetos son iguales (lo que significa que `Equals` devuelve verdadero), entonces `GetHashCode` **debe** devolver el mismo valor para cada uno de ellos.

- **Rango amplio**: si dos objetos no son iguales (`Equals` dice falso), debe haber una **alta probabilidad** de que sus códigos hash sean distintos. El hashing *perfecto* a menudo no es posible ya que hay un número limitado de valores para elegir.

- **Barato**: Debería ser económico calcular el código hash en todos los casos.

Consulte: [Directrices para sobrecargar Equals() y Operator ==](https://msdn.microsoft.com/en-us/library/ms173147.aspx)


## Escribir una buena anulación de GetHashCode
`GetHashCode` tiene importantes efectos de rendimiento en Dictionary<> y HashTable.

Buenos métodos `GetHashCode`

- debe tener una distribución uniforme
- cada número entero debería tener aproximadamente la misma posibilidad de regresar para una instancia aleatoria
- si su método devuelve el mismo número entero (por ejemplo, la constante '999') para cada instancia, tendrá un mal rendimiento
- debe ser rápido
- Estos NO son hashes criptográficos, donde la lentitud es una característica
- cuanto más lenta sea la función hash, más lento será el diccionario
- debe devolver el mismo HashCode en dos instancias que `Equals` evalúa como verdadero
- si no lo hacen (por ejemplo, porque `GetHashCode` devuelve un número aleatorio), es posible que los elementos no se encuentren en una `Lista`, `Diccionario` o similar.

Un buen método para implementar `GetHashCode` es usar un número primo como valor inicial y agregar los códigos hash de los campos del tipo multiplicados por otros números primos a eso:

    public override int GetHashCode()
    {
        unchecked // Overflow is fine, just wrap
        {
            int hash = 3049; // Start value (prime number).

            // Suitable nullity checks etc, of course :)
            hash = hash * 5039 + field1.GetHashCode();
            hash = hash * 883 + field2.GetHashCode();
            hash = hash * 9719 + field3.GetHashCode();
            return hash;
        }
    }

Solo los campos que se usan en el método `Equals` deben usarse para la función hash.

Si necesita tratar el mismo tipo de diferentes maneras para Dictionary/HashTables, puede usar IEqualityComparer<T>.

## Comportamiento predeterminado de Igual a.
`Equals` se declara en la propia clase `Object`.

    public virtual bool Equals(Object obj);

Por defecto, `Equals` tiene el siguiente comportamiento:

- Si la instancia es un tipo de referencia, `Equals` devolverá verdadero solo si las referencias son las mismas.

- Si la instancia es un tipo de valor, `Equals` devolverá verdadero solo si el tipo y el valor son iguales.

- `cadena` es un caso especial. Se comporta como un tipo de valor.


    namespace ConsoleApplication
    {
        public class Program
        {
            public static void Main(string[] args)
            {
                //areFooClassEqual: False
                Foo fooClass1 = new Foo("42");
                Foo fooClass2 = new Foo("42");
                bool areFooClassEqual = fooClass1.Equals(fooClass2);
                Console.WriteLine("fooClass1 and fooClass2 are equal: {0}", areFooClassEqual);
                //False
    
                //areFooIntEqual: True
                int fooInt1 = 42;
                int fooInt2 = 42;
                bool areFooIntEqual = fooInt1.Equals(fooInt2);
                Console.WriteLine("fooInt1 and fooInt2 are equal: {0}", areFooIntEqual);
    
                //areFooStringEqual: True
                string fooString1 = "42";
                string fooString2 = "42";
                bool areFooStringEqual = fooString1.Equals(fooString2);
                Console.WriteLine("fooString1 and fooString2 are equal: {0}", areFooStringEqual);
            }
        }
    
        public class Foo
        {
            public string Bar { get; }
    
            public Foo(string bar)
            {
                Bar = bar;
            }
        }
    }

## Anular Equals y GetHashCode en tipos personalizados
Para una clase `Persona` como:

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public string Clothes { get; set; }
    }
    
    var person1 = new Person { Name = "Jon", Age = 20, Clothes = "some clothes" };
    var person2 = new Person { Name = "Jon", Age = 20, Clothes = "some other clothes" };

    bool result = person1.Equals(person2); //false because it's reference Equals

Pero definiendo `Equals` y `GetHashCode` de la siguiente manera:

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public string Clothes { get; set; }

        public override bool Equals(object obj)
        {
            var person = obj as Person;
            if(person == null) return false;
            return Name == person.Name && Age == person.Age; //the clothes are not important when comparing two persons
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode()*Age;
        }
    }

    var person1 = new Person { Name = "Jon", Age = 20, Clothes = "some clothes" };
    var person2 = new Person { Name = "Jon", Age = 20, Clothes = "some other clothes" };
    
    bool result = person1.Equals(person2); // result is true

Además, al usar LINQ para realizar diferentes consultas sobre personas, se comprobarán tanto `Equals` como `GetHashCode`:

    var persons = new List<Person>
    {
         new Person{ Name = "Jon", Age = 20, Clothes = "some clothes"},
         new Person{ Name = "Dave", Age = 20, Clothes = "some other clothes"},
         new Person{ Name = "Jon", Age = 20, Clothes = ""}
    };

    var distinctPersons = persons.Distinct().ToList();//distinctPersons has Count = 2

## Iguala y GetHashCode en IEqualityComparator
Para el tipo `Persona` dado:

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public string Clothes { get; set; }
    }

    List<Person> persons = new List<Person>
    {
        new Person{ Name = "Jon", Age = 20, Clothes = "some clothes"},
        new Person{ Name = "Dave", Age = 20, Clothes = "some other clothes"},
        new Person{ Name = "Jon", Age = 20, Clothes = ""}
    };

    var distinctPersons = persons.Distinct().ToList();// distinctPersons has Count = 3

Pero definir `Equals` y `GetHashCode` en un `IEqualityComparator`:

    public class PersonComparator : IEqualityComparer<Person>
    {
        public bool Equals(Person x, Person y)
        {
            return x.Name == y.Name && x.Age == y.Age; //the clothes are not important when comparing two persons;
        }

        public int GetHashCode(Person obj) { return obj.Name.GetHashCode() * obj.Age; }
    }

    var distinctPersons = persons.Distinct(new PersonComparator()).ToList();// distinctPersons has Count = 2

Tenga en cuenta que para esta consulta, dos objetos se consideraron iguales si tanto `Equals` devolvieron verdadero como `GetHashCode` devolvieron el mismo código hash para las dos personas.

