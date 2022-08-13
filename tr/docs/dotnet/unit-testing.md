---
title: "Birim testi"
slug: "birim-testi"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Mevcut bir çözüme MSTest birim test projesi ekleme
* Çözüme sağ tıklayın, Yeni proje ekle
* Test bölümünden Birim Test Projesi seçin
* Derleme için bir ad seçin - 'Foo' projesini test ediyorsanız, ad 'Foo.Tests' olabilir
* Birim test proje referanslarında test edilen projeye bir referans ekleyin


## Örnek bir test yöntemi oluşturma
MSTest (varsayılan test çerçevesi), test sınıflarınızın bir "[TestClass]" özniteliği ile ve test yöntemlerinin bir "[TestMethod]" özniteliği ile dekore edilmesini ve genel olmanızı gerektirir.

    [TestClass]
    public class FizzBuzzFixture
    {
        [TestMethod]
        public void Test1()
        {
            //arrange
            var solver = new FizzBuzzSolver();
            //act
            var result = solver.FizzBuzz(1);
            //assert
            Assert.AreEqual("1",result);
        }
    }

