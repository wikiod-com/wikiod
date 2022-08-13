---
title: "Lambda İfadeleri"
slug: "lambda-ifadeleri"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Kapanışlar
---

Lambda ifadeleri örtük olarak [kullanılan değişkenleri yakalar ve bir kapatma oluşturur][0]. Kapatma, bazı durum bağlamlarıyla birlikte bir işlevdir. Derleyici, bir lambda ifadesi çevresindeki bağlamdan bir değeri 'kapsadığında' bir kapatma oluşturur.

Örneğin. aşağıdaki yürütüldüğünde

    Func<object, bool> safeApplyFiltererPredicate = o => (o != null) && filterer.Predicate(i);

"safeApplyFilterPredicate", geçerli "filterer" değerine özel bir referansı olan ve "Invoke" yöntemi şöyle davranan yeni oluşturulan bir nesneyi ifade eder.

    o => (o != null) && filterer.Predicate(i);

Bu önemli olabilir, çünkü "safeApplyFilterPredicate" içindeki değere yapılan başvuru korunduğu sürece, "filterer" öğesinin şu anda başvurduğu nesneye bir başvuru olacaktır. Bunun çöp toplama üzerinde bir etkisi vardır ve "filtreleyici"nin halihazırda başvurduğu nesne mutasyona uğramışsa beklenmeyen davranışlara neden olabilir.

Öte yandan, kapatmalar, diğer nesnelere referanslar içeren bir davranışı kapsüllemek için kasıtlı bir etki için kullanılabilir.

Örneğin.

    var logger = new Logger();
    Func<int, int> Add1AndLog = i => {
        logger.Log("adding 1 to " + i);
        return (i + 1);
    };

Kapatmalar durum makinelerini modellemek için de kullanılabilir:

    Func<int, int> MyAddingMachine() {
        var i = 0;
        return x => i += x;
    };

[0]: http://csharpindepth.com/Articles/Chapter5/Closures.aspx

## Bir kapanış oluşturmak için lambda sözdizimini kullanma
Kapanışların tartışılması için açıklamalara bakın. Bir arayüzümüz olduğunu varsayalım:

    public interface IMachine<TState, TInput>
    {
        TState State { get; }
        public void Input(TInput input);
    }

ve ardından aşağıdakiler yürütülür:

    IMachine<int, int> machine = ...;
    Func<int, int> machineClosure = i => {
        machine.Input(i);
        return machine.State;
    };

Şimdi 'machineClosure', 'int' ile 'int' arasındaki bir işlevi ifade eder; bu, perde arkasında, hesaplamayı gerçekleştirmek için 'makine'nin atıfta bulunduğu 'IMachine' örneğini kullanır. "machine" referansı kapsam dışına çıksa bile, "machineClosure" nesnesi korunduğu sürece, orijinal "IMachine" örneği, derleyici tarafından otomatik olarak tanımlanan bir "kapatma"nın parçası olarak tutulacaktır.

Uyarı: Bu, aynı işlev çağrısının farklı zamanlarda farklı değerler döndürdüğü anlamına gelebilir (örneğin, bu örnekte, makine girişlerinin toplamını tutarsa). Çoğu durumda, bu beklenmeyen bir durum olabilir ve işlevsel stildeki herhangi bir kod için kaçınılmalıdır - kazara ve beklenmedik kapanmalar bir hata kaynağı olabilir.

## Temel lambda ifadeleri
    Func<int, int> add1 = i => i + 1;

    Func<int, int, int> add = (i, j) => i + j;

    // Behaviourally equivalent to:

    int Add1(int i)
    {
        return i + 1;
    }

    int Add(int i, int j)
    {
        return i + j;
    }

    ...

    Console.WriteLine(add1(42)); //43
    Console.WriteLine(Add1(42)); //43
    Console.WriteLine(add(100, 250)); //350
    Console.WriteLine(Add(100, 250)); //350

## LINQ ile temel lambda ifadeleri
    // assume source is {0, 1, 2, ..., 10}

    var evens = source.Where(n => n%2 == 0);
    // evens = {0, 2, 4, ... 10}

    var strings = source.Select(n => n.ToString());
    // strings = {"0", "1", ..., "10"}

## İfade bloğu gövdesine sahip Lambda sözdizimi
    Func<int, string> doubleThenAddElevenThenQuote = i => {
        var doubled = 2 * i;
        var addedEleven = 11 + doubled;
        return $"'{addedEleven}'";
    };

## System.Linq.Expressions ile Lambda ifadeleri
    Expression<Func<int, bool>> checkEvenExpression = i => i%2 == 0;
    // lambda expression is automatically converted to an Expression<Func<int, bool>>

