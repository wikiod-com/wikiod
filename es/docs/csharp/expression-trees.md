---
title: "Árboles de expresión"
slug: "arboles-de-expresion"
draft: false
images: []
weight: 9793
type: docs
toc: true
---

Los árboles de expresión son expresiones organizadas en una estructura de datos en forma de árbol. Cada nodo del árbol es una representación de una expresión, siendo una expresión código. Una representación en memoria de una expresión Lambda sería un árbol de expresión, que contiene los elementos reales (es decir, el código) de la consulta, pero no su resultado. Los árboles de expresión hacen que la estructura de una expresión lambda sea transparente y explícita.

## Sintaxis
- Expresión\<TDelegate\> nombre = lambdaExpression;

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| Delegado | El tipo de delegado que se utilizará para la expresión |
| lambdaExpresión | La expresión lambda (ej. `num => num < 5`) |


<h1>Introducción a los árboles de expresión</h1>
<h2>De dónde venimos</h2>

Los árboles de expresión tienen que ver con consumir "código fuente" en tiempo de ejecución. Considere un método que calcula el impuesto sobre las ventas adeudado en un pedido de ventas `decimal CalculateTotalTaxDue(SalesOrder order)`. Usar ese método en un programa .NET es fácil & mdash; simplemente llámelo `impuesto decimal adeudado = Calcular impuesto total adeudado (pedido);`. ¿Qué sucede si desea aplicarlo a todos los resultados de una consulta remota (SQL, XML, un servidor remoto, etc.)? ¡Esas fuentes de consulta remotas no pueden llamar al método! Tradicionalmente, tendría que invertir el flujo en todos estos casos. Realice la consulta completa, guárdela en la memoria, luego recorra los resultados y calcule el impuesto para cada resultado.

<h2>Cómo evitar problemas de latencia y memoria de inversión de flujo</h2>

Los árboles de expresión son estructuras de datos en formato de árbol, donde cada nodo contiene una expresión. Se utilizan para traducir las instrucciones compiladas (como métodos utilizados para filtrar datos) en expresiones que podrían utilizarse fuera del entorno del programa, como dentro de una consulta de base de datos.

El problema aquí es que una consulta remota *no puede acceder a nuestro método*. Podríamos evitar este problema si, en cambio, enviáramos las *instrucciones* del método a la consulta remota. En nuestro ejemplo `CalculateTotalTaxDue`, eso significa que enviamos esta información:
1. Crear una variable para almacenar el impuesto total
2. Recorra todas las líneas del pedido
3. Para cada línea, verifique si el producto está sujeto a impuestos
4. Si es así, multiplique el total de la línea por la tasa impositiva aplicable y agregue esa cantidad al total
5. De lo contrario, no hagas nada

Con esas instrucciones, la consulta remota puede realizar el trabajo mientras crea los datos.

Hay dos desafíos para implementar esto. ¿Cómo transforma un método .NET compilado en una lista de instrucciones y cómo formatea las instrucciones de manera que puedan ser consumidas por el sistema remoto?

Sin árboles de expresión, solo podría resolver el primer problema con MSIL. (MSIL es el código similar a un ensamblador creado por el compilador .NET). Analizar MSIL es *posible*, pero no es fácil. Incluso cuando lo analiza correctamente, puede ser difícil determinar cuál era la intención del programador original con una rutina en particular.

<h2>Los árboles de expresión salvan el día</h2>
Los árboles de expresión abordan estos problemas exactos. Representan instrucciones de programa, una estructura de datos de árbol donde cada nodo representa *una instrucción* y tiene referencias a toda la información que necesita para ejecutar esa instrucción. Por ejemplo, una `MethodCallExpression` tiene referencia a 1) la `MethodInfo` a la que llamará, 2) una lista de `Expression`s que pasará a ese método, 3) para métodos de instancia, la `Expression` llamaré al método on. Puede "caminar por el árbol" y aplicar las instrucciones en su consulta remota.


<h2>Creación de árboles de expresión</h2>
La forma más sencilla de crear un árbol de expresión es con una expresión lambda. Estas expresiones tienen casi el mismo aspecto que los métodos normales de C#. Es importante darse cuenta de que esto es *magia del compilador*. Cuando crea por primera vez una expresión lambda, el compilador verifica a qué lo asigna. Si es un tipo `Delegado` (incluyendo `Action` o `Func`), el compilador convierte la expresión lambda en un delegado. Si es una `LambdaExpression` (o una `Expression<Action<T>>` o `Expression<Func<T>>` que son `LambdaExpression` fuertemente tipadas), el compilador la transforma en una `LambdaExpression`. Aquí es donde entra en juego la magia. Detrás de escena, el compilador *usa la API del árbol de expresiones* para transformar su expresión lambda en una `LambdaExpression`.

Las expresiones lambda no pueden crear todos los tipos de árboles de expresión. En esos casos, puede usar la API de expresiones manualmente para crear el árbol que necesita. En el ejemplo [Comprender la API de expresiones](//stackoverflow.com/documentation/c%23/75/expression-trees/19200/understanding-the-expressions-api), creamos la expresión `CalculateTotalSalesTax` utilizando la API.

NOTA: Los nombres se vuelven un poco confusos aquí. Una *expresión lambda* (dos palabras, en minúsculas) se refiere al bloque de código con un indicador `=>`. Representa un método anónimo en C# y se convierte en `Delegado` o `Expresión`. Una *`LambdaExpression`* (una palabra, PascalCase) se refiere al tipo de nodo dentro de la API de expresión que representa un método que puede ejecutar.

<h1>Árboles de expresión y LINQ</h1>

Uno de los usos más comunes de los árboles de expresión es con LINQ y consultas de bases de datos. LINQ empareja un árbol de expresión con un proveedor de consultas para aplicar sus instrucciones a la consulta remota de destino. Por ejemplo, el proveedor de consultas de LINQ to Entity Framework transforma un árbol de expresión en SQL que se ejecuta directamente en la base de datos.

Al juntar todas las piezas, puede ver el poder real detrás de LINQ.

1. Escriba una consulta usando una expresión lambda: `products.Where(x => x.Cost > 5)`
2. El compilador transforma esa expresión en un árbol de expresiones con las instrucciones "verifique si la propiedad Costo del parámetro es mayor que cinco".
3. El proveedor de consultas analiza el árbol de expresiones y genera una consulta SQL válida `SELECT * FROM products WHERE Cost > 5`
4. El ORM proyecta todos los resultados en POCO y obtienes una lista de objetos

<h1>Notas</h1>

* Los árboles de expresión son inmutables. Si desea cambiar un árbol de expresión, debe crear uno nuevo, copiar el existente en el nuevo (para atravesar un árbol de expresión, puede usar `ExpressionVisitor`) y realizar los cambios deseados.

## Crear árboles de expresión con una expresión lambda
El siguiente es el árbol de expresión más básico creado por lambda.

    Expression<Func<int, bool>> lambda = num => num == 42;

Para crear árboles de expresión 'a mano', se debe usar la clase 'Expresión'.

La expresión anterior sería equivalente a:

    ParameterExpression parameter = Expression.Parameter(typeof(int), "num"); // num argument
    ConstantExpression constant = Expression.Constant(42, typeof(int)); // 42 constant
    BinaryExpression equality = Expression.Equals(parameter, constant); // equality of two expressions (num == 42)
    Expression<Func<int, bool>> lambda = Expression.Lambda<Func<int, bool>>(equality, parameter);

## Creación de árboles de expresión mediante la API
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

## Compilar árboles de expresión
    // Define an expression tree, taking an integer, returning a bool.
    Expression<Func<int, bool>> expr = num => num < 5;
    
    // Call the Compile method on the expression tree to return a delegate that can be called.
    Func<int, bool> result = expr.Compile();
    
    // Invoke the delegate and write the result to the console.
    Console.WriteLine(result(4)); // Prints true
    
    // Prints True.
    
    // You can also combine the compile step with the call/invoke step as below:
    Console.WriteLine(expr.Compile()(4));

## Análisis de árboles de expresión
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

## Árbol de expresión básico
Los árboles de expresión representan código en una estructura de datos similar a un árbol, donde cada nodo es una expresión

Expression Trees permite la modificación dinámica del código ejecutable, la ejecución de consultas LINQ en varias bases de datos y la creación de consultas dinámicas. Puede compilar y ejecutar código representado por árboles de expresión.

Estos también se utilizan en el tiempo de ejecución de lenguaje dinámico (DLR) para proporcionar interoperabilidad entre lenguajes dinámicos y .NET Framework y para permitir que los escritores de compiladores emitan árboles de expresión en lugar del lenguaje intermedio de Microsoft (MSIL).

Los árboles de expresión se pueden crear a través de

1. Expresión lambda anónima,
2. Manualmente usando el espacio de nombres System.Linq.Expressions.


**Árboles de expresión de Lambda Expressions**

Cuando se asigna una expresión lambda a la variable de tipo Expression<TDelegate> , el compilador emite código para crear un árbol de expresión que representa la expresión lambda.

Los siguientes ejemplos de código muestran cómo hacer que el compilador de C# cree un árbol de expresión que represente la expresión lambda num => num < 5.

    Expression<Func<int, bool>> lambda = num => num < 5;

**Árboles de expresión mediante el uso de la API**

Los árboles de expresión también se crean usando la clase **Expresión**. Esta clase contiene métodos de fábrica estáticos que crean nodos de árboles de expresión de tipos específicos.

A continuación se muestran algunos tipos de nodos de árbol.
1. ParámetroExpresión
2. MétodoLlamadaExpresión

El siguiente ejemplo de código muestra cómo crear un árbol de expresión que represente la expresión lambda num => num < 5 mediante la API.

    ParameterExpression numParam = Expression.Parameter(typeof(int), "num");
    ConstantExpression five = Expression.Constant(5, typeof(int));
    BinaryExpression numLessThanFive = Expression.LessThan(numParam, five);
    Expression<Func<int, bool>> lambda1 = Expression.Lambda<Func<int, bool>>(numLessThanFive,new ParameterExpression[] { numParam });


## Examinando la estructura de una expresión usando Visitor
Defina una nueva clase de visitante anulando algunos de los métodos de [ExpressionVisitor][1]:

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

Llame a `Visit` para usar este visitante en una expresión existente:

    Expression<Func<int,bool>> isBig = a => a > 1000000;
    var visitor = new PrintingVisitor();
    visitor.Visit(isBig);

[1]: https://msdn.microsoft.com/en-us/library/system.linq.expressions.expressionvisitor(v=vs.110).aspx


## Comprender la API de expresiones
Vamos a utilizar la API del árbol de expresiones para crear un árbol `CalculateSalesTax`. En lenguaje sencillo, aquí hay un resumen de los pasos necesarios para crear el árbol.

1. Comprobar si el producto está sujeto a impuestos
2. Si es así, multiplique el total de la línea por la tasa impositiva aplicable y devuelva esa cantidad
3. De lo contrario, devuelve 0


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
    

