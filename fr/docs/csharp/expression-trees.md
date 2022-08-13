---
title: "Arbres d'expressions"
slug: "arbres-dexpressions"
draft: false
images: []
weight: 9793
type: docs
toc: true
---

Les arbres d'expression sont des expressions disposées dans une structure de données arborescente. Chaque nœud de l'arbre est une représentation d'une expression, une expression étant du code. Une représentation en mémoire d'une expression Lambda serait une arborescence d'expressions, qui contient les éléments réels (c'est-à-dire le code) de la requête, mais pas son résultat. Les arbres d'expression rendent la structure d'une expression lambda transparente et explicite.

## Syntaxe
- Expression\<TDelegate\> nom = lambdaExpression ;

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| TDéléguer | Le type délégué à utiliser pour l'expression |
| Expression lambda | L'expression lambda (ex. `num => num < 5`) |


<h1>Introduction aux arbres d'expression</h1>
<h2>D'où nous venons</h2>

Les arbres d'expression consistent à consommer du "code source" au moment de l'exécution. Considérons une méthode qui calcule la taxe de vente due sur une commande client `decimal CalculateTotalTaxDue(SalesOrder order)`. L'utilisation de cette méthode dans un programme .NET est facile &mdash; vous l'appelez simplement `decimal taxDue = CalculateTotalTaxDue(order);`. Et si vous voulez l'appliquer à tous les résultats d'une requête distante (SQL, XML, un serveur distant, etc) ? Ces sources de requêtes distantes ne peuvent pas appeler la méthode ! Traditionnellement, il faudrait inverser le flux dans tous ces cas. Effectuez la requête entière, stockez-la en mémoire, puis parcourez les résultats et calculez la taxe pour chaque résultat.

<h2>Comment éviter les problèmes de mémoire et de latence d'inversion de flux</h2>

Les arbres d'expression sont des structures de données dans un format d'arbre, où chaque nœud contient une expression. Ils sont utilisés pour traduire les instructions compilées (comme les méthodes utilisées pour filtrer les données) en expressions qui pourraient être utilisées en dehors de l'environnement du programme, comme dans une requête de base de données.

Le problème ici est qu'une requête distante * ne peut pas accéder à notre méthode *. Nous pourrions éviter ce problème si, à la place, nous envoyions les *instructions* de la méthode à la requête distante. Dans notre exemple "CalculateTotalTaxDue", cela signifie que nous envoyons ces informations :
1. Créez une variable pour stocker la taxe totale
2. Parcourez toutes les lignes de la commande
3. Pour chaque ligne, vérifiez si le produit est taxable
4. Si c'est le cas, multipliez le total de la ligne par le taux de taxe applicable et ajoutez ce montant au total.
5. Sinon ne rien faire

Avec ces instructions, la requête distante peut effectuer le travail pendant qu'elle crée les données.

Il y a deux défis à relever pour mettre cela en œuvre. Comment transformez-vous une méthode .NET compilée en une liste d'instructions et comment formatez-vous les instructions de manière à ce qu'elles puissent être consommées par le système distant ?

Sans arbres d'expression, vous ne pourriez résoudre que le premier problème avec MSIL. (MSIL est le code de type assembleur créé par le compilateur .NET.) L'analyse de MSIL est *possible*, mais ce n'est pas facile. Même lorsque vous l'analysez correctement, il peut être difficile de déterminer quelle était l'intention du programmeur d'origine avec une routine particulière.

<h2>Les arbres d'expression sauvent la situation</h2>
Les arbres d'expression traitent ces problèmes précis. Ils représentent des instructions de programme une structure de données arborescente où chaque nœud représente * une instruction * et a des références à toutes les informations dont vous avez besoin pour exécuter cette instruction. Par exemple, un `MethodCallExpression` fait référence à 1) le `MethodInfo` qu'il va appeler, 2) une liste d'`Expression`s qu'il passera à cette méthode, 3) pour les méthodes d'instance, le `Expression` you' appellerons la méthode on. Vous pouvez "parcourir l'arbre" et appliquer les instructions sur votre requête à distance.


<h2>Créer des arborescences d'expressions</h2>
Le moyen le plus simple de créer une arborescence d'expressions consiste à utiliser une expression lambda. Ces expressions ressemblent presque aux méthodes C# normales. Il est important de réaliser que c'est *la magie du compilateur*. Lorsque vous créez une expression lambda pour la première fois, le compilateur vérifie à quoi vous l'assignez. S'il s'agit d'un type `Delegate` (y compris `Action` ou `Func`), le compilateur convertit l'expression lambda en délégué. Si c'est une `LambdaExpression` (ou une `Expression<Action<T>>` ou `Expression<Func<T>>` qui sont fortement typées `LambdaExpression`), le compilateur la transforme en une `LambdaExpression`. C'est là que la magie opère. Dans les coulisses, le compilateur *utilise l'API d'arborescence d'expressions* pour transformer votre expression lambda en une `LambdaExpression`.

Les expressions lambda ne peuvent pas créer tous les types d'arborescence d'expression. Dans ces cas, vous pouvez utiliser l'API Expressions manuellement pour créer l'arborescence dont vous avez besoin. Dans l'exemple [Comprendre l'API des expressions](//stackoverflow.com/documentation/c%23/75/expression-trees/19200/understanding-the-expressions-api), nous créons l'expression `CalculateTotalSalesTax` à l'aide de l'API.

REMARQUE : Les noms sont un peu déroutants ici. Une *expression lambda* (deux mots, minuscules) fait référence au bloc de code avec un indicateur `=>`. Il représente une méthode anonyme en C# et est converti en un `Delegate` ou `Expression`. Un *`LambdaExpression`* (un mot, PascalCase) fait référence au type de nœud dans l'API Expression qui représente une méthode que vous pouvez exécuter.

<h1>Arbres d'expression et LINQ</h1>

L'une des utilisations les plus courantes des arbres d'expression est avec LINQ et les requêtes de base de données. LINQ associe une arborescence d'expressions à un fournisseur de requêtes pour appliquer vos instructions à la requête distante cible. Par exemple, le fournisseur de requêtes LINQ to Entity Framework transforme une arborescence d'expressions en SQL qui est exécuté directement sur la base de données.

En rassemblant toutes les pièces, vous pouvez voir la véritable puissance derrière LINQ.

1. Écrivez une requête en utilisant une expression lambda : `products.Where(x => x.Cost > 5)`
2. Le compilateur transforme cette expression en une arborescence d'expressions avec les instructions "vérifier si la propriété Cost du paramètre est supérieure à cinq".
3. Le fournisseur de requêtes analyse l'arborescence d'expressions et produit une requête SQL valide `SELECT * FROM products WHERE Cost > 5`
4. L'ORM projette tous les résultats dans des POCO et vous récupérez une liste d'objets

<h1>Remarques</h1>

* Les arbres d'expression sont immuables. Si vous souhaitez modifier une arborescence d'expressions, vous devez en créer une nouvelle, copier celle qui existe dans la nouvelle (pour parcourir une arborescence d'expressions, vous pouvez utiliser `ExpressionVisitor`) et apporter les modifications souhaitées.

## Créer des arbres d'expression avec une expression lambda
Voici l'arbre d'expression le plus basique créé par lambda.

    Expression<Func<int, bool>> lambda = num => num == 42;

Pour créer des arbres d'expression "à la main", il faut utiliser la classe "Expression".

L'expression ci-dessus serait équivalente à :

    ParameterExpression parameter = Expression.Parameter(typeof(int), "num"); // num argument
    ConstantExpression constant = Expression.Constant(42, typeof(int)); // 42 constant
    BinaryExpression equality = Expression.Equals(parameter, constant); // equality of two expressions (num == 42)
    Expression<Func<int, bool>> lambda = Expression.Lambda<Func<int, bool>>(equality, parameter);

## Création d'arborescences d'expressions à l'aide de l'API
    using System.Linq.Expressions;
    
    // Manually build the expression tree for 
    // the lambda expression num => num < 5.
    ParameterExpression numParam = Expression.Parameter(typeof(int), "num");
    ConstantExpression five = Expression.Constant(5, typeof(int));
    BinaryExpression numLessThanFive = Expression.LessThan(numParam, five);
    Expression<Func<int, bool>> lambda1 =
        Expression.Lambda<Func<int, bool>>(
            numLessThanFive,
            new ParameterExpression[] { numParam });

## Compilation d'arborescences d'expressions
    // Define an expression tree, taking an integer, returning a bool.
    Expression<Func<int, bool>> expr = num => num < 5;
    
    // Call the Compile method on the expression tree to return a delegate that can be called.
    Func<int, bool> result = expr.Compile();
    
    // Invoke the delegate and write the result to the console.
    Console.WriteLine(result(4)); // Prints true
    
    // Prints True.
    
    // You can also combine the compile step with the call/invoke step as below:
    Console.WriteLine(expr.Compile()(4));

## Analyse des arbres d'expression
    using System.Linq.Expressions;
    
    // Create an expression tree.
    Expression<Func<int, bool>> exprTree = num => num < 5;
    
    // Decompose the expression tree.
    ParameterExpression param = (ParameterExpression)exprTree.Parameters[0];
    BinaryExpression operation = (BinaryExpression)exprTree.Body;
    ParameterExpression left = (ParameterExpression)operation.Left;
    ConstantExpression right = (ConstantExpression)operation.Right;
    
    Console.WriteLine("Decomposed expression: {0} => {1} {2} {3}",
                      param.Name, left.Name, operation.NodeType, right.Value);
    
    // Decomposed expression: num => num LessThan 5      

## Arbre d'expression de base
Les arbres d'expression représentent le code dans une structure de données arborescente, où chaque nœud est une expression

Expression Trees permet la modification dynamique du code exécutable, l'exécution de requêtes LINQ dans diverses bases de données et la création de requêtes dynamiques. Vous pouvez compiler et exécuter du code représenté par des arborescences d'expressions.

Ceux-ci sont également utilisés dans le Dynamic Language Runtime (DLR) pour assurer l'interopérabilité entre les langages dynamiques et le .NET Framework et pour permettre aux rédacteurs de compilateurs d'émettre des arborescences d'expressions au lieu du langage intermédiaire Microsoft (MSIL).

Les arbres d'expression peuvent être créés via

1. Expression lambda anonyme,
2. Manuellement en utilisant l'espace de noms System.Linq.Expressions.


**Arbres d'expression à partir d'expressions Lambda**

Lorsqu'une expression lambda est affectée à la variable de type Expression<TDelegate> , le compilateur émet du code pour créer une arborescence d'expressions qui représente l'expression lambda.

Les exemples de code suivants montrent comment faire en sorte que le compilateur C# crée une arborescence d'expressions qui représente l'expression lambda num => num < 5.

    Expression<Func<int, bool>> lambda = num => num < 5;

**Arbres d'expression à l'aide de l'API**

Les arbres d'expression ont également été créés à l'aide de la classe **Expression**. Cette classe contient des méthodes de fabrique statiques qui créent des nœuds d'arbre d'expression de types spécifiques.

Vous trouverez ci-dessous quelques types de nœuds d'arbre.
1. ExpressionParamètre
2. MethodCallExpression

L'exemple de code suivant montre comment créer une arborescence d'expressions qui représente l'expression lambda num => num < 5 à l'aide de l'API.

    ParameterExpression numParam = Expression.Parameter(typeof(int), "num");
    ConstantExpression five = Expression.Constant(5, typeof(int));
    BinaryExpression numLessThanFive = Expression.LessThan(numParam, five);
    Expression<Func<int, bool>> lambda1 = Expression.Lambda<Func<int, bool>>(numLessThanFive,new ParameterExpression[] { numParam });


## Examen de la structure d'une expression à l'aide de Visitor
Définissez une nouvelle classe de visiteurs en remplaçant certaines des méthodes de [ExpressionVisitor][1] :

    class PrintingVisitor : ExpressionVisitor {
        protected override Expression VisitConstant(ConstantExpression node) {
            Console.WriteLine("Constant: {0}", node);
            return base.VisitConstant(node);
        }
        protected override Expression VisitParameter(ParameterExpression node) {
            Console.WriteLine("Parameter: {0}", node);
            return base.VisitParameter(node);
        }
        protected override Expression VisitBinary(BinaryExpression node) {
            Console.WriteLine("Binary with operator {0}", node.NodeType);
            return base.VisitBinary(node);
        }
    }

Appelez `Visit` pour utiliser ce visiteur sur une expression existante :

    Expression<Func<int,bool>> isBig = a => a > 1000000;
    var visitor = new PrintingVisitor();
    visitor.Visit(isBig);

[1] : https://msdn.microsoft.com/en-us/library/system.linq.expressions.expressionvisitor(v=vs.110).aspx


## Comprendre l'API des expressions
Nous allons utiliser l'API d'arbre d'expression pour créer un arbre `CalculateSalesTax`. En clair, voici un résumé des étapes nécessaires pour créer l'arbre.

1. Vérifiez si le produit est taxable
2. Si c'est le cas, multipliez le total de la ligne par le taux de taxe applicable et renvoyez ce montant
3. Sinon retourner 0


    //For reference, we're using the API to build this lambda expression
        orderLine => orderLine.IsTaxable ? orderLine.Total * orderLine.Order.TaxRate : 0;
    
    //The orderLine parameter we pass in to the method.  We specify it's type (OrderLine) and the name of the parameter.
        ParameterExpression orderLine = Expression.Parameter(typeof(OrderLine), "orderLine");
    
    //Check if the parameter is taxable;  First we need to access the is taxable property, then check if it's true
        PropertyInfo isTaxableAccessor = typeof(OrderLine).GetProperty("IsTaxable");
        MemberExpression getIsTaxable = Expression.MakeMemberAccess(orderLine, isTaxableAccessor);
        UnaryExpression isLineTaxable = Expression.IsTrue(getIsTaxable);
    
    //Before creating the if, we need to create the braches
        //If the line is taxable, we'll return the total times the tax rate; get the total and tax rate, then multiply
        //Get the total
        PropertyInfo totalAccessor = typeof(OrderLine).GetProperty("Total");
        MemberExpression getTotal = Expression.MakeMemberAccess(orderLine, totalAccessor);
        
        //Get the order
        PropertyInfo orderAccessor = typeof(OrderLine).GetProperty("Order");
        MemberExpression getOrder = Expression.MakeMemberAccess(orderLine, orderAccessor);
        
        //Get the tax rate - notice that we pass the getOrder expression directly to the member access
        PropertyInfo taxRateAccessor = typeof(Order).GetProperty("TaxRate");
        MemberExpression getTaxRate = Expression.MakeMemberAccess(getOrder, taxRateAccessor);
        
        //Multiply the two - notice we pass the two operand expressions directly to multiply
        BinaryExpression multiplyTotalByRate = Expression.Multiply(getTotal, getTaxRate);
        
    //If the line is not taxable, we'll return a constant value - 0.0 (decimal)
        ConstantExpression zero = Expression.Constant(0M);
    
    //Create the actual if check and branches
        ConditionalExpression ifTaxableTernary = Expression.Condition(isLineTaxable, multiplyTotalByRate, zero);
        
    //Wrap the whole thing up in a "method" - a LambdaExpression
        Expression<Func<OrderLine, decimal>> method = Expression.Lambda<Func<OrderLine, decimal>>(ifTaxableTernary, orderLine);
    

