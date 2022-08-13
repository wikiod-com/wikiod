---
title: "Klonlanabilir"
slug: "klonlanabilir"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Sözdizimi
- nesne ICloneable.Clone() { return Clone(); } // Özel genel Clone() işlevimizi kullanan arabirim yönteminin özel uygulaması.
- public Foo Clone() { yeni Foo(this); } // Ortak klon yöntemi, kopya oluşturucu mantığını kullanmalıdır.

"CLR", tür güvenli olmayan bir "object Clone()" yöntem tanımı gerektirir. Bu davranışı geçersiz kılmak ve içeren sınıfın bir kopyasını döndüren bir tür güvenli yöntem tanımlamak yaygın bir uygulamadır.

Klonlamanın yalnızca yüzeysel kopya mı yoksa derin kopya mı olduğuna karar vermek yazara bağlıdır. Referans içeren değişmez yapılar için derin bir kopya yapılması önerilir. Kendilerine referans olan sınıflar için, sığ bir kopya uygulamak muhtemelen iyidir.

<sub>NOT: "C#" dilinde bir arayüz yöntemi, yukarıda gösterilen sözdizimi ile özel olarak uygulanabilir.</sub>

## ICloneable'ı bir sınıfta uygulama
'ICloneable'ı bir bükülme ile bir sınıfta uygulayın. Genel bir tür güvenli 'Clone()' ortaya çıkarın ve 'object Clone()'u özel olarak uygulayın.

    public class Person : ICloneable
    {
        // Contents of class
        public string Name { get; set; }
        public int Age { get; set; }
        // Constructor
        public Person(string name, int age)
        {
            this.Name=name;
            this.Age=age;
        }
        // Copy Constructor
        public Person(Person other)
        {
            this.Name=other.Name;
            this.Age=other.Age;
        }

        #region ICloneable Members
        // Type safe Clone
        public Person Clone() { return new Person(this); }
        // ICloneable implementation
        object ICloneable.Clone()
        {
            return Clone();
        }
        #endregion
    }

Daha sonra aşağıdaki gibi kullanılacaktır:

    {
        Person bob=new Person("Bob", 25);
        Person bob_clone=bob.Clone();
        Debug.Assert(bob_clone.Name==bob.Name);
    
        bob.Age=56;
        Debug.Assert(bob.Age!=bob.Age);
    }

"bob"un yaşını değiştirmenin "bob_clone"un yaşını değiştirmediğine dikkat edin. Bunun nedeni, tasarımın (referans) değişkenleri atamak yerine klonlamayı kullanmasıdır.

## ICloneable'ı bir yapıya uygulama
Bir yapı için ICloneable'ın uygulanması genellikle gerekli değildir, çünkü yapılar `=` atama operatörüyle üye bazında bir kopya yapar. Ancak tasarım, "ICloneable"dan miras kalan başka bir arabirimin uygulanmasını gerektirebilir.

Diğer bir neden de, yapının kopyalanması gereken bir referans türü (veya bir dizi) içermesidir.

    // Structs are recommended to be immutable objects
    [ImmutableObject(true)]
    public struct Person : ICloneable
    {
        // Contents of class
        public string Name { get; private set; }
        public int Age { get; private set; }
        // Constructor
        public Person(string name, int age)
        {
            this.Name=name;
            this.Age=age;
        }
        // Copy Constructor
        public Person(Person other)
        {
            // The assignment operator copies all members
            this=other;
        }

        #region ICloneable Members
        // Type safe Clone
        public Person Clone() { return new Person(this); }
        // ICloneable implementation
        object ICloneable.Clone()
        {
            return Clone();
        }
        #endregion
    }

Daha sonra aşağıdaki gibi kullanılacaktır:

    static void Main(string[] args)
    {
        Person bob=new Person("Bob", 25);
        Person bob_clone=bob.Clone();
        Debug.Assert(bob_clone.Name==bob.Name);
    }




