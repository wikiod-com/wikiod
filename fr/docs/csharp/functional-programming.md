---
title: "Programmation fonctionnelle"
slug: "programmation-fonctionnelle"
draft: false
images: []
weight: 9866
type: docs
toc: true
---

## Fonction et action
**Func** fournit un support pour les fonctions anonymes paramétrées. Les principaux types sont les entrées et le dernier type est toujours la valeur de retour.

    // square a number.
    Func<double, double> square = (x) => { return x * x; };

    // get the square root.
    // note how the signature matches the built in method.
    Func<double, double> squareroot = Math.Sqrt;

    // provide your workings.
    Func<double, double, string> workings = (x, y) => 
        string.Format("The square of {0} is {1}.", x, square(y))
        
Les objets **Action** sont comme des méthodes void, ils n'ont donc qu'un type d'entrée. Aucun résultat n'est placé sur la pile d'évaluation.

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



## Évitez les références nulles
Les développeurs C# doivent gérer de nombreuses exceptions de référence nulles. Les développeurs F # ne le font pas car ils ont le type Option. Un type Option<> (certains préfèrent Maybe<> comme nom) fournit un type de retour Some et None. Cela rend explicite le fait qu'une méthode peut être sur le point de renvoyer un enregistrement nul.

Par exemple, vous ne pouvez pas lire ce qui suit et savoir si vous devrez gérer une valeur nulle.

    var user = _repository.GetUser(id);

Si vous connaissez le null possible, vous pouvez introduire du code passe-partout pour le gérer.

    var username = user != null ? user.Name : string.Empty;

Que se passe-t-il si nous avons une Option<> renvoyée à la place ?

    Option<User> maybeUser = _repository.GetUser(id);

Le code indique maintenant explicitement que nous pouvons renvoyer un enregistrement None et que le code passe-partout pour vérifier Some ou None est requis :

    var username = maybeUser.HasValue ? maybeUser.Value.Name : string.Empty;

La méthode suivante montre comment retourner une Option<>

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

Voici une implémentation minimale de Option<>.

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

Pour démontrer ce qui précède [avoidNull.csx][1] peut être exécuté avec le C# REPL.

Comme indiqué, il s'agit d'une implémentation minimale. Une recherche de ["Maybe" NuGet packages][2] affichera un certain nombre de bonnes bibliothèques.


[1] : https://gist.github.com/Boggin/d53660f32aeaa35e0b028919ddc465e3
[2] : https://www.nuget.org/packages?q=peut-être

## Fonctions d'ordre supérieur
Une fonction d'ordre supérieur est une fonction qui prend une autre fonction comme argument ou renvoie une fonction (ou les deux).

Cela se fait couramment avec les lambdas, par exemple lors du passage d'un prédicat à une clause LINQ Where :

    var results = data.Where(p => p.Items == 0);

La clause Where() peut recevoir de nombreux prédicats différents, ce qui lui donne une flexibilité considérable.

Le passage d'une méthode dans une autre méthode est également observé lors de la mise en œuvre du modèle de conception de stratégie. Par exemple, diverses méthodes de tri peuvent être choisies et transmises à une méthode Sort sur un objet en fonction des exigences au moment de l'exécution.

## Immuabilité
L'immuabilité est courante dans la programmation fonctionnelle et rare dans la programmation orientée objet.

Créez, par exemple, un type d'adresse avec un état mutable :

    public class Address () 
    {
        public string Line1 { get; set; }
        public string Line2 { get; set; }
        public string City  { get; set; }
    }

N'importe quel morceau de code pourrait modifier n'importe quelle propriété de l'objet ci-dessus.

Créez maintenant le type d'adresse immuable :

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

Gardez à l'esprit que le fait d'avoir des collections en lecture seule ne respecte pas l'immuabilité. Par exemple,

    public class Classroom
    {
        public readonly List<Student> Students;
        
        public Classroom(List<Student> students)
        {
            Students = students;
        }
    }

n'est pas immuable, car l'utilisateur de l'objet peut modifier la collection (y ajouter ou en supprimer des éléments). Pour le rendre immuable, il faut soit utiliser une interface comme IEnumerable<Student>, qui n'expose pas les méthodes à ajouter, soit en faire une ReadOnlyCollection<Student>.

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


Avec l'objet immuable, nous avons les avantages suivants :

- Il sera dans un état connu (un autre code ne peut pas le changer).
- C'est thread-safe.
- Le constructeur propose un lieu unique de validation.
- Le fait de savoir que l'objet ne peut pas être modifié facilite la compréhension du code.

## Collections immuables
Le package NuGet [`System.Collections.Immutable`][1] fournit des classes de collection immuables.

# Création et ajout d'éléments

    var stack = ImmutableStack.Create<int>();
    var stack2 = stack.Push(1); // stack is still empty, stack2 contains 1
    var stack3 = stack.Push(2); // stack2 still contains only one, stack3 has 2, 1

# Création à l'aide du constructeur

Certaines collections immuables ont une classe interne "Builder" qui peut être utilisée pour construire à moindre coût de grandes instances immuables :

    var builder = ImmutableList.CreateBuilder<int>(); // returns ImmutableList.Builder
    builder.Add(1);
    builder.Add(2);
    var list = builder.ToImmutable();

# Création à partir d'un IEnumerable existant

    var numbers = Enumerable.Range(1, 5);
    var list = ImmutableList.CreateRange<int>(numbers);

Liste de tous les types de collection immuables :

- [`System.Collections.Immutable.ImmutableArray<T>`][2]
- [`System.Collections.Immutable.ImmutableDictionary<TKey,TValue>`][3]
- [`System.Collections.Immutable.ImmutableHashSet<T>`][4]
- [`System.Collections.Immutable.ImmutableList<T>`][5]
- [`System.Collections.Immutable.ImmutableQueue<T>`][6]
- [`System.Collections.Immutable.ImmutableSortedDictionary<TKey,TValue>`][7]
- [`System.Collections.Immutable.ImmutableSortedSet<T>`][8]
- [`System.Collections.Immutable.ImmutableStack<T>`][9]


[1] : https://www.nuget.org/packages/System.Collections.Immutable/
[2] : https://msdn.microsoft.com/en-us/library/dn638264(v=vs.111).aspx
[3] : https://msdn.microsoft.com/en-us/library/dn467181(v=vs.111).aspx
[4] : https://msdn.microsoft.com/en-us/library/dn467171(v=vs.111).aspx
[5] : https://msdn.microsoft.com/en-us/library/dn456077.aspx
[6] : https://msdn.microsoft.com/en-us/library/dn467186(v=vs.111).aspx
[7] : https://msdn.microsoft.com/en-us/library/dn467194(v=vs.111).aspx
[8] : https://msdn.microsoft.com/en-us/library/dn467193(v=vs.111).aspx
[9] : https://msdn.microsoft.com/en-us/library/dn467197(v=vs.111).aspx

