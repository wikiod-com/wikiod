---
title: "taşma"
slug: "tasma"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Tamsayı taşması
Bir tamsayının saklayabileceği maksimum kapasite vardır. Ve bu sınırı aştığınızda, negatif tarafa geri dönecektir. "int" için "2147483647"

    int x = int.MaxValue;                //MaxValue is 2147483647
    x = unchecked(x + 1);                //make operation explicitly unchecked so that the example also works when the check for arithmetic overflow/underflow is enabled in the project settings 
    Console.WriteLine(x);                //Will print -2147483648
    Console.WriteLine(int.MinValue);     //Same as Min value

Bu aralığın dışındaki herhangi bir tamsayı için, veri türüne sahip System.Numerics ad alanını kullanın.
BüyükTamsayı. Daha fazla bilgi için aşağıdaki bağlantıyı kontrol edin https://msdn.microsoft.com/en-us/library/system.numerics.biginteger(v=vs.110).aspx

## Çalışma sırasında taşma
İşlem sırasında taşma da olur. Aşağıdaki örnekte, x bir 'int', 1 varsayılan olarak bir 'int'dir. Bu nedenle ekleme bir 'int' eklemesidir. Ve sonuç bir 'int' olacaktır. Ve taşacak.

    int x = int.MaxValue;               //MaxValue is 2147483647
    long y = x + 1;                     //It will be overflown
    Console.WriteLine(y);               //Will print -2147483648
    Console.WriteLine(int.MinValue);    //Same as Min value

1L kullanarak bunu önleyebilirsiniz. Şimdi 1 "uzun" olacak ve ekleme "uzun" bir ekleme olacak

    int x = int.MaxValue;               //MaxValue is 2147483647
    long y = x + 1L;                    //It will be OK
    Console.WriteLine(y);               //Will print 2147483648


## Sipariş önemlidir
Aşağıdaki kodda taşma var

    int x = int.MaxValue;
    Console.WriteLine(x + x + 1L);  //prints -1

Aşağıdaki kodda ise taşma yok

    int x = int.MaxValue;
    Console.WriteLine(x + 1L + x);  //prints 4294967295

Bunun nedeni, işlemlerin soldan sağa sıralanmasıdır. İlk kod parçasında "x + x" taşar ve bundan sonra "uzun" olur. Öte yandan 'x + 1L' 'uzun' olur ve bundan sonra bu değere 'x' eklenir.


