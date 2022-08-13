---
title: "Árvores de Expressão"
slug: "arvores-de-expressao"
draft: false
images: []
weight: 9793
type: docs
toc: true
---

Árvores de Expressão são Expressões organizadas em uma estrutura de dados em forma de árvore. Cada nó na árvore é uma representação de uma expressão, uma expressão sendo código. Uma representação na memória de uma expressão Lambda seria uma árvore de expressão, que contém os elementos reais (ou seja, código) da consulta, mas não seu resultado. As árvores de expressão tornam a estrutura de uma expressão lambda transparente e explícita.

## Sintaxe
- Expressão\<TDelegate\> nome = lambdaExpression;

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| TDelegate | O tipo de delegado a ser usado para a expressão |
| expressão lambda | A expressão lambda (ex. `num => num < 5`) |


<h1>Introdução às árvores de expressão</h1>
<h2>De onde viemos</h2>

As árvores de expressão tratam do consumo de "código-fonte" em tempo de execução. Considere um método que calcula o imposto sobre vendas devido em um pedido de vendas `decimal CalculateTotalTaxDue(SalesOrder order)`. Usar esse método em um programa .NET é fácil &mdash; basta chamá-lo de `decimal taxDue = CalculateTotalTaxDue(order);`. E se você quiser aplicá-lo a todos os resultados de uma consulta remota (SQL, XML, um servidor remoto, etc)? Essas fontes de consulta remotas não podem chamar o método! Tradicionalmente, você teria que inverter o fluxo em todos esses casos. Faça a consulta inteira, armazene-a na memória, faça um loop pelos resultados e calcule o imposto para cada resultado.

<h2>Como evitar problemas de memória e latência da inversão de fluxo</h2>

Árvores de expressão são estruturas de dados em formato de árvore, onde cada nó contém uma expressão. Eles são usados ​​para traduzir as instruções compiladas (como métodos usados ​​para filtrar dados) em expressões que podem ser usadas fora do ambiente do programa, como dentro de uma consulta de banco de dados.

O problema aqui é que uma consulta remota *não pode acessar nosso método*. Poderíamos evitar esse problema se, em vez disso, enviássemos as *instruções* do método para a consulta remota. Em nosso exemplo `CalculateTotalTaxDue`, isso significa que enviamos esta informação:
1. Crie uma variável para armazenar o imposto total
2. Percorra todas as linhas do pedido
3. Para cada linha, verifique se o produto é tributável
4. Se for, multiplique o total da linha pela taxa de imposto aplicável e adicione esse valor ao total
5. Caso contrário, não faça nada

Com essas instruções, a consulta remota pode realizar o trabalho enquanto cria os dados.

Há dois desafios para implementar isso. Como você transforma um método .NET compilado em uma lista de instruções e como formata as instruções de forma que possam ser consumidas pelo sistema remoto?

Sem árvores de expressão, você só poderia resolver o primeiro problema com o MSIL. (MSIL é o código do tipo assembler criado pelo compilador .NET.) Analisar o MSIL é *possível*, mas não é fácil. Mesmo quando você analisa corretamente, pode ser difícil determinar qual era a intenção do programador original com uma rotina específica.

<h2>Árvores de expressão salvam o dia</h2>
As árvores de expressão tratam exatamente desses problemas. Eles representam as instruções do programa uma estrutura de dados em árvore onde cada nó representa *uma instrução* e tem referências a todas as informações que você precisa para executar essa instrução. Por exemplo, um `MethodCallExpression` tem referência a 1) o `MethodInfo` que ele irá chamar, 2) uma lista de `Expression`s que ele passará para aquele método, 3) para métodos de instância, o `Expression` você' Vou chamar o método on. Você pode "andar na árvore" e aplicar as instruções em sua consulta remota.


<h2>Criando árvores de expressão</h2>
A maneira mais fácil de criar uma árvore de expressão é com uma expressão lambda. Essas expressões parecem quase iguais aos métodos normais do C#. É importante perceber que isso é *mágica do compilador*. Quando você cria uma expressão lambda pela primeira vez, o compilador verifica a que você atribui. Se for um tipo `Delegate` (incluindo `Action` ou `Func`), o compilador converte a expressão lambda em um delegado. Se for uma `LambdaExpression` (ou uma `Expression<Action<T>>` ou `Expression<Func<T>>` que são `LambdaExpression`'s fortemente tipadas), o compilador o transforma em uma `LambdaExpression`. É aqui que a mágica começa. Nos bastidores, o compilador *usa a API da árvore de expressão* para transformar sua expressão lambda em uma `LambdaExpression`.

As expressões lambda não podem criar todos os tipos de árvore de expressão. Nesses casos, você pode usar a API Expressions manualmente para criar a árvore necessária. No exemplo [Entendendo a API de expressões](//stackoverflow.com/documentation/c%23/75/expression-trees/19200/understanding-the-expressions-api), criamos a expressão `CalculateTotalSalesTax` usando a API.

NOTA: Os nomes ficam um pouco confusos aqui. Uma *expressão lambda* (duas palavras, minúsculas) refere-se ao bloco de código com um indicador `=>`. Ele representa um método anônimo em C# e é convertido em um `Delegate` ou `Expression`. Um *`LambdaExpression`* (uma palavra, PascalCase) refere-se ao tipo de nó dentro da API Expression que representa um método que você pode executar.

<h1>Árvores de expressão e LINQ</h1>

Um dos usos mais comuns de árvores de expressão é com LINQ e consultas de banco de dados. O LINQ emparelha uma árvore de expressão com um provedor de consulta para aplicar suas instruções à consulta remota de destino. Por exemplo, o provedor de consulta LINQ to Entity Framework transforma uma árvore de expressão em SQL que é executada diretamente no banco de dados.

Juntando todas as peças, você pode ver o verdadeiro poder por trás do LINQ.

1. Escreva uma consulta usando uma expressão lambda: `products.Where(x => x.Cost > 5)`
2. O compilador transforma essa expressão em uma árvore de expressões com as instruções "verificar se a propriedade Cost do parâmetro é maior que cinco".
3. O provedor de consulta analisa a árvore de expressão e produz uma consulta SQL válida `SELECT * FROM products WHERE Cost > 5`
4. O ORM projeta todos os resultados em POCOs e você recebe uma lista de objetos de volta

<h1>Observações</h1>

* As árvores de expressão são imutáveis. Se você deseja alterar uma árvore de expressão, você precisa criar uma nova, copiar a existente na nova (para percorrer uma árvore de expressão você pode usar o `ExpressionVisitor`) e fazer as alterações desejadas.

## Crie árvores de expressão com uma expressão lambda
A seguir está a árvore de expressão mais básica que é criada pelo lambda.

    Expression<Func<int, bool>> lambda = num => num == 42;

Para criar árvores de expressão 'manualmente', deve-se usar a classe `Expression`.

A expressão acima seria equivalente a:

    ParameterExpression parameter = Expression.Parameter(typeof(int), "num"); // num argument
    ConstantExpression constant = Expression.Constant(42, typeof(int)); // 42 constant
    BinaryExpression equality = Expression.Equals(parameter, constant); // equality of two expressions (num == 42)
    Expression<Func<int, bool>> lambda = Expression.Lambda<Func<int, bool>>(equality, parameter);

## Criando árvores de expressão usando a API
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

## Compilando árvores de expressão
    // Define an expression tree, taking an integer, returning a bool.
    Expression<Func<int, bool>> expr = num => num < 5;
    
    // Call the Compile method on the expression tree to return a delegate that can be called.
    Func<int, bool> result = expr.Compile();
    
    // Invoke the delegate and write the result to the console.
    Console.WriteLine(result(4)); // Prints true
    
    // Prints True.
    
    // You can also combine the compile step with the call/invoke step as below:
    Console.WriteLine(expr.Compile()(4));

## Analisando árvores de expressão
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

## Árvore de Expressão Básica
As árvores de expressão representam o código em uma estrutura de dados semelhante a uma árvore, onde cada nó é uma expressão

As Árvores de Expressão permitem a modificação dinâmica do código executável, a execução de consultas LINQ em vários bancos de dados e a criação de consultas dinâmicas. Você pode compilar e executar código representado por árvores de expressão.

Eles também são usados ​​no tempo de execução de linguagem dinâmica (DLR) para fornecer interoperabilidade entre linguagens dinâmicas e o .NET Framework e para permitir que os escritores do compilador emitam árvores de expressão em vez da linguagem intermediária da Microsoft (MSIL).

Árvores de Expressão podem ser criadas via

1. Expressão lambda anônima,
2. Manualmente usando o namespace System.Linq.Expressions.


**Árvores de expressão de expressões lambda**

Quando uma expressão lambda é atribuída à variável do tipo Expression<TDelegate> , o compilador emite código para criar uma árvore de expressão que representa a expressão lambda.

Os exemplos de código a seguir mostram como fazer com que o compilador C# crie uma árvore de expressão que representa a expressão lambda num => num < 5.

    Expression<Func<int, bool>> lambda = num => num < 5;

**Árvores de expressão usando a API**

Árvores de Expressão também criadas usando a classe **Expression**. Essa classe contém métodos de fábrica estáticos que criam nós de árvore de expressão de tipos específicos.

Abaixo estão alguns tipos de nós de árvore.
1. Expressão de Parâmetro
2. Expressão de Chamada de Método

O exemplo de código a seguir mostra como criar uma árvore de expressão que representa a expressão lambda num => num < 5 usando a API.

    ParameterExpression numParam = Expression.Parameter(typeof(int), "num");
    ConstantExpression five = Expression.Constant(5, typeof(int));
    BinaryExpression numLessThanFive = Expression.LessThan(numParam, five);
    Expression<Func<int, bool>> lambda1 = Expression.Lambda<Func<int, bool>>(numLessThanFive,new ParameterExpression[] { numParam });


## Examinando a estrutura de uma expressão usando Visitor
Defina uma nova classe de visitante substituindo alguns dos métodos de [ExpressionVisitor][1]:

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

Chame `Visit` para usar este visitante em uma expressão existente:

    Expression<Func<int,bool>> isBig = a => a > 1000000;
    var visitor = new PrintingVisitor();
    visitor.Visit(isBig);

[1]: https://msdn.microsoft.com/en-us/library/system.linq.expressions.expressionvisitor(v=vs.110).aspx


## Entendendo a API de expressões
Vamos usar a API da árvore de expressão para criar uma árvore `CalculateSalesTax`. Em linguagem simples, aqui está um resumo das etapas necessárias para criar a árvore.

1. Verifique se o produto é tributável
2. Se for, multiplique o total da linha pela taxa de imposto aplicável e devolva esse valor
3. Caso contrário, retorne 0


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
    

