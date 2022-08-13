---
title: "Kod Sözleşmeleri ve İddialar"
slug: "kod-sozlesmeleri-ve-iddialar"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Mantığı kontrol etmeye yönelik iddialar her zaman doğru olmalıdır
İddialar, girdi parametrelerinin testini gerçekleştirmek için değil, program akışının doğru olduğunu, yani belirli bir zamanda kodunuz hakkında belirli varsayımlarda bulunabileceğinizi doğrulamak için kullanılır. Başka bir deyişle: "Debug.Assert" ile yapılan bir test, *daima* test edilen değerin "doğru" olduğunu varsaymalıdır.

Debug.Assert yalnızca DEBUG yapılarında yürütülür; RELEASE yapılarından filtrelenir. Kod sözleşmelerinin veya girdi doğrulama yöntemlerinin yerine geçmemesi için birim testine ek olarak bir hata ayıklama aracı olarak düşünülmelidir.

Örneğin, bu iyi bir iddiadır:

    var systemData = RetrieveSystemConfiguration();
    Debug.Assert(systemData != null);

Burada assert iyi bir seçimdir çünkü RetrieveSystemConfiguration() öğesinin geçerli bir değer döndüreceğini ve hiçbir zaman boş döndürmeyeceğini varsayabiliriz.

İşte başka bir iyi örnek:

    UserData user = RetrieveUserData();
    Debug.Assert(user != null);
    Debug.Assert(user.Age > 0);
    int year = DateTime.Today.Year - user.Age;

İlk olarak, RetrieveUserData()'nın geçerli bir değer döndüreceğini varsayabiliriz. Ardından, Age özelliğini kullanmadan önce, kullanıcının yaşının kesinlikle pozitif olduğu varsayımını (her zaman doğru olması gereken) doğrularız.

Bu, assert'in kötü bir örneğidir:

    string input = Console.ReadLine();
    int age = Convert.ToInt32(input);
    Debug.Assert(age > 16);
    Console.WriteLine("Great, you are over 16");

Assert, giriş doğrulaması için değildir çünkü bu iddianın her zaman doğru olacağını varsaymak yanlıştır. Bunun için giriş doğrulama yöntemlerini kullanmalısınız. Yukarıdaki durumda, giriş değerinin ilk etapta bir sayı olduğunu da doğrulamalısınız.


