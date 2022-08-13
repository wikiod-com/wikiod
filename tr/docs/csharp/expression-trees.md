---
title: "İfade Ağaçları"
slug: "ifade-agaclar"
draft: false
images: []
weight: 9793
type: docs
toc: true
---

İfade Ağaçları, ağaç benzeri bir veri yapısında düzenlenmiş İfadelerdir. Ağaçtaki her düğüm, bir ifadenin temsilidir, bir ifade koddur. Bir Lambda ifadesinin Bellek İçi temsili, sorgunun gerçek öğelerini (yani kodu) tutan ancak sonucunu içermeyen bir İfade ağacı olacaktır. İfade ağaçları, bir lambda ifadesinin yapısını şeffaf ve açık hale getirir.

## Sözdizimi
- İfade\<TDelegate\> adı = lambdaExpression;

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| Delege | İfade için kullanılacak temsilci türü |
| lambdaİfade | Lambda ifadesi (ör. `num => num < 5`) |


<h1>İfade Ağaçlarına Giriş</h1>
<h2>Nereden geldik</h2>

İfade ağaçları, çalışma zamanında "kaynak kodu" tüketmekle ilgilidir. Bir satış siparişi `decimal CalculateTotalTaxDue(SalesOrder order)` üzerinden ödenmesi gereken satış vergisini hesaplayan bir yöntem düşünün. Bu yöntemi bir .NET programında kullanmak kolaydır &mdash; sadece `decimal taxDue = CalculateTotalTaxDue(order);` olarak adlandırın. Uzak bir sorgudan (SQL, XML, uzak sunucu, vb.) gelen tüm sonuçlara uygulamak isterseniz ne olur? Bu uzak sorgu kaynakları yöntemi çağıramaz! Geleneksel olarak, tüm bu durumlarda akışı tersine çevirmeniz gerekir. Tüm sorguyu yapın, bellekte saklayın, ardından sonuçlar arasında dolaşın ve her sonuç için vergi hesaplayın.

<h2>Akış ters çevirmenin bellek ve gecikme sorunları nasıl önlenir</h2>

İfade ağaçları, her düğümün bir ifadeyi tuttuğu, bir ağaç biçimindeki veri yapılarıdır. Bir veritabanı sorgusu gibi program ortamının dışında kullanılabilecek ifadelerde derlenmiş talimatları (verileri filtrelemek için kullanılan yöntemler gibi) çevirmek için kullanılırlar.

Buradaki sorun, uzak bir sorgunun *yöntemimize* erişememesidir. Bunun yerine yöntem için *talimatları* uzak sorguya gönderseydik bu sorunu önleyebilirdik. `CalculateTotalTaxDue` örneğimizde bu, şu bilgiyi gönderdiğimiz anlamına gelir:
1. Toplam vergiyi depolamak için bir değişken oluşturun
2. Siparişteki tüm satırlar arasında dolaşın
3. Her satır için ürünün vergiye tabi olup olmadığını kontrol edin
4. Varsa, satır toplamını geçerli vergi oranıyla çarpın ve bu tutarı toplama ekleyin
5. Aksi takdirde hiçbir şey yapmayın

Bu talimatlarla, uzak sorgu, verileri oluştururken işi gerçekleştirebilir.

Bunu uygulamanın iki zorluğu var. Derlenmiş bir .NET yöntemini bir talimat listesine nasıl dönüştürürsünüz ve talimatları uzak sistem tarafından tüketilebilecek şekilde nasıl biçimlendirirsiniz?

İfade ağaçları olmadan, yalnızca MSIL ile ilk sorunu çözebilirsiniz. (MSIL, .NET derleyicisi tarafından oluşturulan derleyici benzeri koddur.) MSIL'i ayrıştırmak *mümkün*, ancak kolay değil. Düzgün bir şekilde ayrıştırdığınızda bile, orijinal programcının belirli bir rutinle amacının ne olduğunu belirlemek zor olabilir.

<h2>İfade ağaçları günü kurtarıyor</h2>
İfade ağaçları tam olarak bu sorunları ele alır. Program talimatlarını, her düğümün *bir talimatı* temsil ettiği bir ağaç veri yapısını temsil ederler ve bu talimatı yürütmek için ihtiyaç duyduğunuz tüm bilgilere referansları vardır. Örneğin, bir 'MethodCallExpression', 1) çağıracağı 'MethodInfo'ya, 2) bu yönteme ileteceği 'İfade'lerin bir listesine, 3) örneğin yöntemlere, 'Sizin İfadesine' atıfta bulunur. yöntemini çağıracağım. "Ağaçta yürüyebilir" ve talimatları uzaktan sorgunuza uygulayabilirsiniz.


<h2>İfade ağaçları oluşturma</h2>
Bir ifade ağacı oluşturmanın en kolay yolu bir lambda ifadesidir. Bu ifadeler, normal C# yöntemleriyle neredeyse aynı görünür. Bunun *derleyici büyüsü* olduğunun farkına varmak önemlidir. Bir lambda ifadesi ilk oluşturduğunuzda, derleyici onu neye atadığınızı kontrol eder. "Delegate" tipiyse ("Eylem" veya "Func" dahil), derleyici lambda ifadesini bir temsilciye dönüştürür. Eğer bu bir "LambdaExpression" (veya "Expression<Action<T>>" veya "Expression<Func<T>>" ise ve bunlar güçlü bir şekilde "LambdaExpression" olarak yazılırsa), derleyici bunu bir "LambdaExpression"a dönüştürür. İşte sihir burada devreye giriyor. Perde arkasında, derleyici *deyim ağacı API'sini* kullanarak lambda ifadenizi bir 'LambdaExpression'a dönüştürür.

Lambda ifadeleri, her tür ifade ağacını oluşturamaz. Bu durumlarda, ihtiyacınız olan ağacı oluşturmak için Expressions API'yi manuel olarak kullanabilirsiniz. [Expression the API](//stackoverflow.com/documentation/c%23/75/expression-trees/19200/underunder-the-expressions-api) örneğinde, API'yi kullanarak "CalculateTotalSalesTax" ifadesini oluşturuyoruz.

NOT: İsimler burada biraz kafa karıştırıcı oluyor. Bir *lambda ifadesi* (iki kelime, küçük harf), `=>` göstergeli kod bloğunu ifade eder. C#'da anonim bir yöntemi temsil eder ve bir "Temsilci" veya "İfade"ye dönüştürülür. Bir *`LambdaExpression`* (tek kelime, PascalCase), Expression API içindeki yürütebileceğiniz bir yöntemi temsil eden düğüm türünü ifade eder.

<h1>İfade Ağaçları ve LINQ</h1>

İfade ağaçlarının en yaygın kullanımlarından biri LINQ ve veritabanı sorgularıdır. LINQ, talimatlarınızı hedef uzak sorguya uygulamak için bir ifade ağacını bir sorgu sağlayıcısıyla eşleştirir. Örneğin, LINQ to Entity Framework sorgu sağlayıcısı, bir ifade ağacını doğrudan veritabanında yürütülen SQL'e dönüştürür.

Tüm parçaları bir araya getirdiğinizde, LINQ'un arkasındaki gerçek gücü görebilirsiniz.

1. Bir lambda ifadesi kullanarak bir sorgu yazın: `products.Where(x => x.Cost > 5)`
2. Derleyici, "parametrenin Cost özelliğinin beşten büyük olup olmadığını kontrol edin" talimatlarıyla bu ifadeyi bir ifade ağacına dönüştürür.
3. Sorgu sağlayıcı, ifade ağacını ayrıştırır ve geçerli bir SQL sorgusu üretir `SELECT * FROM FROM Products WHERE Cost > 5`
4. ORM tüm sonuçları POCO'lara yansıtır ve nesnelerin bir listesini geri alırsınız

<h1>Notlar</h1>

* İfade ağaçları değişmezdir. Bir ifade ağacını değiştirmek istiyorsanız, yeni bir tane oluşturmanız gerekir, mevcut olanı yenisine kopyalayın (bir ifade ağacını geçmek için 'ExpressionVisitor'u kullanabilirsiniz) ve istenen değişiklikleri yapın.

## Bir lambda ifadesi ile İfade Ağaçları oluşturun
Lambda tarafından oluşturulan en temel ifade ağacı aşağıdadır.

    Expression<Func<int, bool>> lambda = num => num == 42;

'El ile' ifade ağaçları oluşturmak için, 'Expression' sınıfı kullanılmalıdır.

Yukarıdaki ifade şuna eşdeğer olacaktır:

    ParameterExpression parameter = Expression.Parameter(typeof(int), "num"); // num argument
    ConstantExpression constant = Expression.Constant(42, typeof(int)); // 42 constant
    BinaryExpression equality = Expression.Equals(parameter, constant); // equality of two expressions (num == 42)
    Expression<Func<int, bool>> lambda = Expression.Lambda<Func<int, bool>>(equality, parameter);

## API Kullanarak İfade Ağaçları Oluşturma
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

## İfade Ağaçlarını Derleme
    // Define an expression tree, taking an integer, returning a bool.
    Expression<Func<int, bool>> expr = num => num < 5;
    
    // Call the Compile method on the expression tree to return a delegate that can be called.
    Func<int, bool> result = expr.Compile();
    
    // Invoke the delegate and write the result to the console.
    Console.WriteLine(result(4)); // Prints true
    
    // Prints True.
    
    // You can also combine the compile step with the call/invoke step as below:
    Console.WriteLine(expr.Compile()(4));

## İfade Ağaçlarını Ayrıştırma
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

## İfade Ağacı Temel
İfade ağaçları, her düğümün bir ifade olduğu, ağaç benzeri bir veri yapısındaki kodu temsil eder.

İfade Ağaçları, yürütülebilir kodun dinamik olarak değiştirilmesini, çeşitli veritabanlarında LINQ sorgularının yürütülmesini ve dinamik sorguların oluşturulmasını sağlar. İfade ağaçlarıyla temsil edilen kodu derleyebilir ve çalıştırabilirsiniz.

Bunlar ayrıca dinamik diller ve .NET Framework arasında birlikte çalışabilirlik sağlamak ve derleyici yazarlarının Microsoft ara dili (MSIL) yerine ifade ağaçları yaymalarını sağlamak için dinamik dil çalışma zamanında (DLR) kullanılır.

İfade Ağaçları Via oluşturulabilir

1. Anonim lambda ifadesi,
2. System.Linq.Expressions ad alanını kullanarak el ile.


**Lambda İfadelerinden İfade Ağaçları**

Expression<TDelegate> type değişkenine bir lambda ifadesi atandığında, derleyici lambda ifadesini temsil eden bir ifade ağacı oluşturmak için kod yayar.

Aşağıdaki kod örnekleri, C# derleyicisinin num => num < 5 lambda ifadesini temsil eden bir ifade ağacı oluşturmasını sağlar.

    Expression<Func<int, bool>> lambda = num => num < 5;

**API Kullanarak İfade Ağaçları**

İfade Ağaçları ayrıca **İfade** Sınıfı kullanılarak oluşturulmuştur. Bu sınıf, belirli türlerde ifade ağacı düğümleri oluşturan statik fabrika yöntemlerini içerir.

Aşağıda birkaç tür Ağaç düğümü bulunmaktadır.
1. Parametre İfadesi
2. MethodCallExpression

Aşağıdaki kod örneği, API kullanılarak num => num < 5 lambda ifadesini temsil eden bir ifade ağacının nasıl oluşturulacağını gösterir.

    ParameterExpression numParam = Expression.Parameter(typeof(int), "num");
    ConstantExpression five = Expression.Constant(5, typeof(int));
    BinaryExpression numLessThanFive = Expression.LessThan(numParam, five);
    Expression<Func<int, bool>> lambda1 = Expression.Lambda<Func<int, bool>>(numLessThanFive,new ParameterExpression[] { numParam });


## Ziyaretçi Kullanarak Bir İfadenin Yapısını İnceleme
[ExpressionVisitor][1] yöntemlerinden bazılarını geçersiz kılarak yeni bir ziyaretçi sınıfı tanımlayın:

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

Bu ziyaretçiyi mevcut bir ifadede kullanmak için "Ziyaret"i arayın:

    Expression<Func<int,bool>> isBig = a => a > 1000000;
    var visitor = new PrintingVisitor();
    visitor.Visit(isBig);

[1]: https://msdn.microsoft.com/en-us/library/system.linq.expressions.expressionvisitor(v=vs.110).aspx


## İfadeler API'sini anlama
Bir `CalculateSalesTax` ağacı oluşturmak için ifade ağacı API'sini kullanacağız. Sade İngilizce olarak, ağacı oluşturmak için atılan adımların bir özetini burada bulabilirsiniz.

1. Ürünün vergiye tabi olup olmadığını kontrol edin
2. Varsa, satır toplamını geçerli vergi oranıyla çarpın ve bu tutarı iade edin.
3. Aksi takdirde 0 döndür


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
    

