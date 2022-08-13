---
title: "NullReferenceException"
slug: "nullreferenceexception"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## NullReferenceException açıkladı
Bir başvuru nesnesinin statik olmayan bir üyesine (özellik, yöntem, alan veya olay) erişmeye çalıştığınızda bir "NullReferenceException" atılır, ancak bu boştur.

    Car myFirstCar = new Car();
    Car mySecondCar = null;
    Color myFirstColor = myFirstCar.Color; // No problem as myFirstCar exists / is not null
    Color mySecondColor = mySecondCar.Color; // Throws a NullReferenceException 
    // as mySecondCar is null and yet we try to access its color.

Böyle bir istisnanın hatalarını ayıklamak oldukça kolaydır: istisnanın atıldığı satırda, her ''.'' veya ''[''' veya nadir durumlarda ''('''den önce bakmanız yeterlidir.

    myGarage.CarCollection[currentIndex.Value].Color = theCarInTheStreet.Color;

İstisnam nereden geliyor?
Herhangi biri:

- "myGarage" "boş"
- "myGarage.CarCollection" "boş"
- "currentIndex" "boş"
- "myGarage.CarCollection[currentIndex.Value]" "boş"
- "CarInTheStreet" "boş"

Hata ayıklama modunda, fare imlecinizi bu öğelerin her birinin üzerine getirmeniz yeterlidir ve boş referansınızı bulacaksınız. O zaman yapmanız gereken, neden bir değeri olmadığını anlamaktır. Düzeltme tamamen yönteminizin amacına bağlıdır.

Başlatmayı/başlatmayı unuttunuz mu?

    myGarage.CarCollection = new Car[10];

Nesne null ise farklı bir şey mi yapmanız gerekiyor?

    if (myGarage == null)
    {
        Console.WriteLine("Maybe you should buy a garage first!");
    }

Ya da belki birisi size boş bir argüman verdi ve yapmaması gerekiyordu:

    if (theCarInTheStreet == null)
    {
        throw new ArgumentNullException("theCarInTheStreet");
    }
Her durumda, bir yöntemin hiçbir zaman NullReferenceException oluşturmaması gerektiğini unutmayın. Eğer öyleyse, bir şeyi kontrol etmeyi unuttunuz demektir.

