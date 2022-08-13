---
title: "Référence de la syntaxe des paramètres"
slug: "reference-de-la-syntaxe-des-parametres"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| `ce cnn` | La connexion à la base de données sous-jacente - le `this` indique une méthode d'extension ; la connexion n'a pas besoin d'être ouverte - si elle n'est pas ouverte, elle s'ouvre et se ferme automatiquement.
| `<T>` / `Type` | (optionnel) Le type d'objet à renvoyer ; si l'API non générique / non-`Type` est utilisée, un objet `dynamic` est renvoyé par ligne, simulant une propriété nommée par nom de colonne renvoyé par la requête (cet objet `dynamic` implémente également `IDicionary<string,object >`).
| `sql` | Le SQL à exécuter
| `param` | (facultatif) Les paramètres à inclure.
| `transaction` | (facultatif) La transaction de base de données à associer à la commande
| `mis en mémoire tampon` | (facultatif) Indique s'il faut pré-consommer les données dans une liste (par défaut), par rapport à l'exposition d'un "IEnumerable" ouvert sur le lecteur en direct
| `commandTimeout` | (facultatif) Le délai d'attente à utiliser sur la commande ; s'il n'est pas spécifié, `SqlMapper.Settings.CommandTimeout` est supposé (si spécifié)
| `TypeCommande` | Le type de commande en cours d'exécution ; par défaut à `CommandText`

La syntaxe d'expression des paramètres varie d'un SGBDR à l'autre. Tous les exemples ci-dessus utilisent la syntaxe SQL Server, c'est-à-dire `@foo` ; cependant, `?foo` et `:foo` devraient également fonctionner correctement.

## Inlining de valeur
Parfois, la commodité d'un paramètre (en termes de maintenance et d'expressivité) peut être compensée par son coût en performances pour le traiter comme un paramètre. Par exemple, lorsque la taille de la page est fixée par un paramètre de configuration. Ou une valeur de statut correspond à une valeur `enum`. Envisager:

    var orders = connection.Query<Order>(@"
    select top (@count) * -- these brackets are an oddity of SQL Server
    from Orders
    where CustomerId = @customerId
    and Status = @open", new { customerId, count = PageSize, open = OrderStatus.Open });

Le seul paramètre *réel* ici est `customerId` - les deux autres sont des pseudo-paramètres qui ne changeront pas réellement. Souvent, le RDBMS peut faire un meilleur travail s'il les détecte comme des constantes. Dapper a une syntaxe spéciale pour cela - `{=nom}` au lieu de `@nom` - qui s'applique *uniquement* aux types numériques. (Cela minimise toute surface d'attaque de l'injection SQL). Un exemple est le suivant :

    var orders = connection.Query<Order>(@"
    select top {=count} *
    from Orders
    where CustomerId = @customerId
    and Status = {=open}", new { customerId, count = PageSize, open = OrderStatus.Open });

Dapper remplace les valeurs par des littéraux avant d'émettre le SQL, de sorte que le RDBMS voit réellement quelque chose comme :

    select top 10 *
    from Orders
    where CustomerId = @customerId
    and Status = 3

Ceci est particulièrement utile lorsque les systèmes RDBMS permettent non seulement de prendre de meilleures décisions, mais aussi d'ouvrir des plans de requête que les paramètres réels empêchent. Par exemple, si un prédicat de colonne est par rapport à un paramètre, un index filtré avec des valeurs spécifiques sur ces colonnes ne peut pas être utilisé. En effet, la requête *next* peut avoir un paramètre autre que l'une de ces valeurs spécifiées.

Avec des valeurs littérales, l'optimiseur de requête peut utiliser les index filtrés car il sait que la valeur ne peut pas changer dans les requêtes futures.

## SQL paramétré de base
Dapper facilite le suivi des meilleures pratiques grâce à un SQL entièrement paramétré.

![Tableaux Bobby](https://imgs.xkcd.com/comics/exploits_of_a_mom.png)

Les paramètres sont importants, donc dapper permet de bien faire les choses. Vous exprimez simplement vos paramètres de la manière normale pour votre SGBDR (généralement `@foo`, `?foo` ou `:foo`) et donnez à dapper un objet qui *a un membre appelé `foo`*. La manière la plus courante de procéder consiste à utiliser un type anonyme :

    int id = 123;
    string name = "abc";
    connection.Execute("insert [KeyLookup](Id, Name) values(@id, @name)",
        new { id, name });

Et c'est tout. Dapper ajoutera les paramètres requis et tout devrait fonctionner.

Utilisation de votre modèle d'objet
---

Vous pouvez également utiliser votre modèle d'objet existant comme paramètre :

    KeyLookup lookup = ... // some existing instance
    connection.Execute("insert [KeyLookup](Id, Name) values(@Id, @Name)", lookup);

Dapper utilise le texte de la commande pour déterminer quels membres de l'objet ajouter - il n'ajoutera généralement pas d'éléments inutiles tels que `Description`, `IsActive`, `CreationDate` car la commande que nous avons émise ne les implique clairement pas - bien qu'il y ait des cas où cela pourrait le faire, par exemple si votre commande contient :

    // TODO - removed for now; include the @Description in the insert

Il n'essaie pas de comprendre que ce qui précède n'est qu'un commentaire.

Procédures stockées
---

Les paramètres des procédures stockées fonctionnent exactement de la même manière, sauf que dapper ne peut pas tenter de déterminer ce qui doit/ne doit pas être inclus - tout ce qui est disponible est traité comme un paramètre. Pour cette raison, les types anonymes sont généralement préférés :

    connection.Execute("KeyLookupInsert", new { id, name },
        commandType: CommandType.StoredProcedure);



## Extensions de liste
Un scénario courant dans les requêtes de base de données est `IN (...)` où la liste ici est générée au moment de l'exécution. La plupart des RDBMS manquent d'une bonne métaphore pour cela - et il n'y a pas de solution universelle * cross-RDBMS * pour cela. Au lieu de cela, dapper fournit une extension de commande automatique douce. Tout ce qui est requis est une valeur de paramètre fournie qui est `IEnumerable`. Une commande impliquant `@foo` est étendue à `(@foo0,@foo1,@foo2,@foo3)` (pour une séquence de 4 éléments). L'utilisation la plus courante de ceci serait `IN` :

    int[] orderIds = ...
    var orders = connection.Query<Order>(@"
    select *
    from Orders
    where Id in @orderIds", new { orderIds });

Cela se développe ensuite automatiquement pour émettre le code SQL approprié pour la récupération multiligne :

    select *
    from Orders
    where Id in (@orderIds0, @orderIds1, @orderIds2, @orderIds3)

avec les paramètres `@orderIds0` etc ajoutés en tant que valeurs tirées du tableau.
Notez que le fait qu'il ne s'agisse pas de SQL valide à l'origine est intentionnel, pour garantir que cette fonctionnalité n'est pas utilisée par erreur. Cette fonctionnalité fonctionne également correctement avec l'indicateur de requête `OPTIMIZE FOR` / `UNKNOWN` dans SQL Server ; si tu utilises:

    option (optimize for
        (@orderIds unknown))

il étendra ceci correctement à:

    option (optimize for
        (@orderIds0 unknown, @orderIds1 unknown, @orderIds2 unknown, @orderIds3 unknown))

## Exécution d'opérations sur plusieurs ensembles d'entrées
Parfois, vous voulez faire la même chose plusieurs fois. Dapper le prend en charge sur la méthode `Execute` si le paramètre *outermost* (qui est généralement un type anonyme unique ou une instance de modèle de domaine) est en fait fourni sous la forme d'une séquence `IEnumerable`. Par exemple:

    Order[] orders = ...
    // update the totals
    connection.Execute("update Orders set Total=@Total where Id=@Id", orders);

Ici, dapper fait juste une simple boucle sur nos données, essentiellement la même que si nous l'avions fait :

    Order[] orders = ...
    // update the totals
    foreach(Order order in orders) {
        connection.Execute("update Orders set Total=@Total where Id=@Id", order);
    }

Cette utilisation devient * particulièrement * intéressante lorsqu'elle est combinée avec l'API `async` sur une connexion qui est explicitement configurée pour tous les "ensembles de résultats actifs multiples" - dans cette utilisation, dapper va automatiquement * canaliser * les opérations, donc vous ne payez pas le coût de latence par ligne. Cela nécessite une utilisation un peu plus compliquée,

    await connection.ExecuteAsync(
        new CommandDefinition(
            "update Orders set Total=@Total where Id=@Id", 
             orders, flags: CommandFlags.Pipelined))

Notez, cependant, que vous pourriez également souhaiter étudier les paramètres de table.

## Paramètres pseudo-positionnels (pour les fournisseurs qui ne prennent pas en charge les paramètres nommés)
Certains fournisseurs ADO.NET (notamment : OleDB) ne prennent pas en charge les paramètres *named* ; les paramètres sont à la place spécifiés uniquement par *position*, avec l'espace réservé `?`. Dapper ne saurait pas quel membre utiliser pour ceux-ci, donc dapper autorise une syntaxe alternative, `?foo?`; ce serait la même chose que `@foo` ou `:foo` dans d'autres variantes SQL, sauf que dapper **remplacera** complètement le jeton de paramètre par `?` avant d'exécuter la requête.

Cela fonctionne en combinaison avec d'autres fonctionnalités telles que l'expansion de la liste, donc ce qui suit est valide :

    string region = "North";
    int[] users = ...
    var docs = conn.Query<Document>(@"
         select * from Documents
         where Region = ?region?
         and OwnerId in ?users?", new { region, users }).AsList();

Les membres `.region` et `.users` sont utilisés en conséquence, et le SQL émis est (par exemple, avec 3 utilisateurs) :

         select * from Documents
         where Region = ?
         and OwnerId in (?,?,?)

Notez, cependant, que dapper **n'autorise pas** l'utilisation du même paramètre plusieurs fois lors de l'utilisation de cette fonctionnalité ; cela évite d'avoir à ajouter plusieurs fois la même valeur de paramètre (qui peut être grande). Si vous devez faire référence à la même valeur plusieurs fois, envisagez de déclarer une variable, par exemple :

    declare @id int = ?id?; // now we can use @id multiple times in the SQL

Si les variables ne sont pas disponibles, vous pouvez utiliser des noms de membres en double dans les paramètres - cela rendra également évident que la valeur est envoyée plusieurs fois :

    int id = 42;
    connection.Execute("... where ParentId = $id0$ ... SomethingElse = $id1$ ...",
          new { id0 = id, id1 = id });

