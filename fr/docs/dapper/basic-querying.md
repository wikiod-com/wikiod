---
title: "Interrogation de base"
slug: "interrogation-de-base"
draft: false
images: []
weight: 9748
type: docs
toc: true
---

## Syntaxe
- public statique IEnumerable&lt;T&gt; Query&lt;T&gt;(this IDbConnection cnn, string sql, object param = null, SqlTransaction transaction = null, bool buffered = true)
- public statique IEnumerable&lt;dynamique&gt; Requête (cette IDbConnection cnn, chaîne sql, objet param = null, SqlTransaction transaction = null, bool buffered = true)

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| cnn | Votre connexion à la base de données, qui doit déjà être ouverte. |
| sql | Commande à exécuter. |
| paramètre | Objet à partir duquel extraire les paramètres. |
| opération | Transaction dont cette requête fait partie, le cas échéant. |
| tamponné | S'il faut ou non tamponner la lecture des résultats de la requête. Il s'agit d'un paramètre facultatif, la valeur par défaut étant true. Lorsque buffered est vrai, les résultats sont mis en mémoire tampon dans une `List<T>` puis renvoyés sous la forme d'un `IEnumerable<T>` qui est sûr pour une énumération multiple. Lorsque buffered est false, la connexion sql est maintenue ouverte jusqu'à ce que vous ayez fini de lire, ce qui vous permet de traiter une seule ligne à la fois en mémoire. Plusieurs énumérations généreront des connexions supplémentaires à la base de données. Alors que buffered false est très efficace pour réduire l'utilisation de la mémoire si vous ne conservez que de très petits fragments des enregistrements renvoyés, il a une [surcharge de performance importante] (http://stackoverflow.com/a/30493725/37055) par rapport à la matérialisation avide du résultat Positionner. Enfin, si vous avez de nombreuses connexions SQL simultanées sans tampon, vous devez tenir compte de la famine du pool de connexions, ce qui provoque le blocage des demandes jusqu'à ce que les connexions soient disponibles. |


## Requête pour un type statique
Pour les types connus au moment de la compilation, utilisez un paramètre générique avec `Query<T>`.

    public class Dog
    {
        public int? Age { get; set; }
        public Guid Id { get; set; }
        public string Name { get; set; }
        public float? Weight { get; set; }
    
        public int IgnoredProperty { get { return 1; } }
    }    
    
    //
    IDBConnection db = /* ... */;

    var @params = new { age = 3 };
    var sql = "SELECT * FROM dbo.Dogs WHERE Age = @age";

    IEnumerable<Dog> dogs = db.Query<Dog>(sql, @params);

## Requête pour les types dynamiques
Vous pouvez également interroger dynamiquement si vous omettez le type générique.
    
    IDBConnection db = /* ... */;
    IEnumerable<dynamic> result = db.Query("SELECT 1 as A, 2 as B");

    var first = result.First();
    int a = (int)first.A; // 1
    int b = (int)first.B; // 2

## Requête avec paramètres dynamiques
    var color = "Black";
    var age = 4;

    var query = "Select * from Cats where Color = :Color and Age > :Age";
    var dynamicParameters = new DynamicParameters();
    dynamicParameters.Add("Color", color);
    dynamicParameters.Add("Age", age);

    using (var connection = new SqlConnection(/* Your Connection String Here */))
    {
        IEnumerable<dynamic> results = connection.Query(query, dynamicParameters);
    }

