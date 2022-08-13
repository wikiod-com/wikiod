---
title: "Equals e GetHashCode"
slug: "equals-e-gethashcode"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Cada implementação de `Equals` deve atender aos seguintes requisitos:

- **Reflexivo**: Um objeto deve ser igual a si mesmo.<br/>`x.Equals(x)` retorna `true`.

- **Simétrico**: Não há diferença se eu comparar x com y ou y com x - o resultado é o mesmo. <br/>`x.Equals(y)` retorna o mesmo valor que `y.Equals(x)`.

- **Transitivo**: se um objeto é igual a outro objeto e este é igual a um terceiro, o primeiro tem que ser igual ao terceiro.<br/>if `(x.Equals(y) && y .Equals(z))` retorna `true`, então `x.Equals(z)` retorna `true`.

- **Consistente**: se você comparar um objeto com outro várias vezes, o resultado será sempre o mesmo.<br/>Invocações sucessivas de `x.Equals(y)` retornam o mesmo valor, desde que os objetos referenciados por x e y não são modificados.

- **Comparação com null**: Nenhum objeto é igual a `null`.<br/>`x.Equals(null)` retorna `false`.

Implementações de `GetHashCode`:

- **Compatível com `Equals`**: Se dois objetos são iguais (o que significa que `Equals` retorna true), então `GetHashCode` **deve** retornar o mesmo valor para cada um deles.

- **Grande faixa**: se dois objetos não forem iguais (`Equals` diz falso), deve haver uma **alta probabilidade** de que seus códigos de hash sejam distintos. O hash *perfeito* geralmente não é possível, pois há um número limitado de valores para escolher.

- **Barato**: Deve ser barato calcular o código hash em todos os casos.

Consulte: [Diretrizes para Sobrecarregar Equals() e Operador ==](https://msdn.microsoft.com/en-us/library/ms173147.aspx)


## Escrevendo uma boa substituição de GetHashCode
`GetHashCode` tem grandes efeitos de desempenho no Dicionário<> e HashTable.

Bons métodos `GetHashCode`

- deve ter uma distribuição uniforme
- cada inteiro deve ter uma chance aproximadamente igual de retornar para uma instância aleatória
- se o seu método retornar o mesmo inteiro (por exemplo, a constante '999') para cada instância, você terá um desempenho ruim
- deve ser rápido
- Estes NÃO são hashes criptográficos, onde a lentidão é uma característica
- quanto mais lenta sua função de hash, mais lento seu dicionário
- deve retornar o mesmo HashCode em duas instâncias que `Equals` avalia como verdadeiro
- se não (por exemplo, porque `GetHashCode` retorna um número aleatório), os itens podem não ser encontrados em uma `Lista`, `Dicionário` ou similar.

Um bom método para implementar `GetHashCode` é usar um número primo como valor inicial e adicionar os códigos de hash dos campos do tipo multiplicados por outros números primos a isso:

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

Apenas os campos que são usados ​​no método `Equals` devem ser usados ​​para a função hash.

Se você precisar tratar o mesmo tipo de maneiras diferentes para Dictionary/HashTables, poderá usar IEqualityComparer<T>.

## Padrão Igual ao comportamento.
`Equals` é declarado na própria classe `Object`.

    public virtual bool Equals(Object obj);

Por padrão, `Equals` tem o seguinte comportamento:

- Se a instância for um tipo de referência, então `Equals` retornará true somente se as referências forem as mesmas.

- Se a instância for um tipo de valor, `Equals` retornará true somente se o tipo e o valor forem os mesmos.

- `string` é um caso especial. Ele se comporta como um tipo de valor.


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

## Substitua Equals e GetHashCode em tipos personalizados
Para uma classe `Person` como:

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public string Clothes { get; set; }
    }
    
    var person1 = new Person { Name = "Jon", Age = 20, Clothes = "some clothes" };
    var person2 = new Person { Name = "Jon", Age = 20, Clothes = "some other clothes" };

    bool result = person1.Equals(person2); //false because it's reference Equals

Mas definindo `Equals` e `GetHashCode` da seguinte forma:

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

Também usar o LINQ para fazer consultas diferentes em pessoas verificará tanto `Equals` quanto `GetHashCode`:

    var persons = new List<Person>
    {
         new Person{ Name = "Jon", Age = 20, Clothes = "some clothes"},
         new Person{ Name = "Dave", Age = 20, Clothes = "some other clothes"},
         new Person{ Name = "Jon", Age = 20, Clothes = ""}
    };

    var distinctPersons = persons.Distinct().ToList();//distinctPersons has Count = 2

## Equals e GetHashCode no IEqualityComparator
Para determinado tipo `Person`:

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

Mas definindo `Equals` e `GetHashCode` em um `IEqualityComparator`:

    public class PersonComparator : IEqualityComparer<Person>
    {
        public bool Equals(Person x, Person y)
        {
            return x.Name == y.Name && x.Age == y.Age; //the clothes are not important when comparing two persons;
        }

        public int GetHashCode(Person obj) { return obj.Name.GetHashCode() * obj.Age; }
    }

    var distinctPersons = persons.Distinct(new PersonComparator()).ToList();// distinctPersons has Count = 2

Observe que, para esta consulta, dois objetos foram considerados iguais se `Equals` retornasse true e `GetHashCode` retornasse o mesmo código hash para as duas pessoas.

