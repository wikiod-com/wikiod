---
title: "Égal et GetHashCode"
slug: "egal-et-gethashcode"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Chaque mise en œuvre de "Equals" doit remplir les conditions suivantes :

- **Réflexif** : un objet doit être égal à lui-même.<br/>`x.Equals(x)` renvoie `true`.

- **Symétrique** : il n'y a pas de différence si je compare x à y ou y à x - le résultat est le même. <br/>`x.Equals(y)` renvoie la même valeur que `y.Equals(x)`.

- **Transitive** : Si un objet est égal à un autre objet et que celui-ci est égal à un troisième, le premier doit être égal au troisième.<br/>if `(x.Equals(y) && y .Equals(z))` renvoie `true`, puis `x.Equals(z)` renvoie `true`.

- **Cohérent** : si vous comparez plusieurs fois un objet à un autre, le résultat est toujours le même.<br/>Les invocations successives de `x.Equals(y)` renvoient la même valeur tant que les objets référencés par x et y ne sont pas modifiés.

- **Comparaison avec null** : aucun objet n'est égal à `null`.<br/>`x.Equals(null)` renvoie `false`.

Implémentations de `GetHashCode` :

- **Compatible avec `Equals`** : si deux objets sont égaux (ce qui signifie que `Equals` renvoie vrai), alors `GetHashCode` **doit** renvoyer la même valeur pour chacun d'eux.

- **Large range** : si deux objets ne sont pas égaux ("Equals" indique "faux"), il devrait y avoir une **forte probabilité** que leurs codes de hachage soient distincts. Le hachage *parfait* n'est souvent pas possible car le nombre de valeurs à choisir est limité.

- **Bon marché** : il devrait être peu coûteux de calculer le code de hachage dans tous les cas.

Voir : [Lignes directrices pour la surcharge de Equals() et Operator ==](https://msdn.microsoft.com/en-us/library/ms173147.aspx)


## Écrire un bon override GetHashCode
`GetHashCode` a des effets majeurs sur les performances de Dictionary<> et HashTable.

Bonnes méthodes `GetHashCode`

- devrait avoir une distribution égale
- chaque entier doit avoir une chance à peu près égale de revenir pour une instance aléatoire
- si votre méthode renvoie le même entier (par exemple la constante '999') pour chaque instance, vous aurez de mauvaises performances
- devrait être rapide
- Ce ne sont PAS des hachages cryptographiques, où la lenteur est une caractéristique
- plus votre fonction de hachage est lente, plus votre dictionnaire est lent
- doit renvoyer le même HashCode sur deux instances que `Equals` évalue à true
- si ce n'est pas le cas (par exemple, parce que `GetHashCode` renvoie un nombre aléatoire), les éléments peuvent ne pas être trouvés dans une `Liste`, un `Dictionnaire` ou similaire.

Une bonne méthode pour implémenter `GetHashCode` consiste à utiliser un nombre premier comme valeur de départ et à ajouter les codes de hachage des champs du type multipliés par d'autres nombres premiers à cela :

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

Seuls les champs utilisés dans la méthode "Equals" doivent être utilisés pour la fonction de hachage.

Si vous avez besoin de traiter le même type de différentes manières pour Dictionary/HashTables, vous pouvez utiliser IEqualityComparer<T>.

## Comportement égal par défaut.
`Equals` est déclaré dans la classe `Object` elle-même.

    public virtual bool Equals(Object obj);

Par défaut, `Equals` a le comportement suivant :

- Si l'instance est un type de référence, alors `Equals` renverra true uniquement si les références sont identiques.

- Si l'instance est un type valeur, alors `Equals` renverra true uniquement si le type et la valeur sont identiques.

- `string` est un cas particulier. Il se comporte comme un type valeur.


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

## Remplacer Equals et GetHashCode sur les types personnalisés
Pour une classe "Personne" comme :

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public string Clothes { get; set; }
    }
    
    var person1 = new Person { Name = "Jon", Age = 20, Clothes = "some clothes" };
    var person2 = new Person { Name = "Jon", Age = 20, Clothes = "some other clothes" };

    bool result = person1.Equals(person2); //false because it's reference Equals

Mais en définissant `Equals` et `GetHashCode` comme suit :

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

Utiliser également LINQ pour effectuer différentes requêtes sur des personnes vérifiera à la fois `Equals` et `GetHashCode` :

    var persons = new List<Person>
    {
         new Person{ Name = "Jon", Age = 20, Clothes = "some clothes"},
         new Person{ Name = "Dave", Age = 20, Clothes = "some other clothes"},
         new Person{ Name = "Jon", Age = 20, Clothes = ""}
    };

    var distinctPersons = persons.Distinct().ToList();//distinctPersons has Count = 2

## Égal et GetHashCode dans IEqualityComparator
Pour le type "Personne" donné :

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

Mais définir `Equals` et `GetHashCode` dans un `IEqualityComparator` :

    public class PersonComparator : IEqualityComparer<Person>
    {
        public bool Equals(Person x, Person y)
        {
            return x.Name == y.Name && x.Age == y.Age; //the clothes are not important when comparing two persons;
        }

        public int GetHashCode(Person obj) { return obj.Name.GetHashCode() * obj.Age; }
    }

    var distinctPersons = persons.Distinct(new PersonComparator()).ToList();// distinctPersons has Count = 2

Notez que pour cette requête, deux objets ont été considérés comme égaux si `Equals` a renvoyé vrai et `GetHashCode` ont renvoyé le même code de hachage pour les deux personnes.

