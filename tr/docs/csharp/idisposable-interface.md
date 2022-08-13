---
title: "Tek kullanımlık arayüz"
slug: "tek-kullanmlk-arayuz"
draft: false
images: []
weight: 9867
type: docs
toc: true
---

- Nesneyi kullanmayı bitirdiklerinde 'Dispose' yöntemini çağırdıklarından emin olmak 'IDisposable' uygulayan sınıfın istemcilerine kalmıştır. CLR'de, çağrılacak bir "Dispose" yöntemi için nesneleri doğrudan arayan hiçbir şey yoktur.

- Nesneniz yalnızca yönetilen kaynaklar içeriyorsa, bir sonlandırıcı uygulamak gerekli değildir. Kendi 'Dispose' yönteminizi uyguladığınızda, sınıfınızın kullandığı tüm nesnelerde 'Dispose'u çağırdığınızdan emin olun.

- İdeal olarak yalnızca bir kez çağrılmasına rağmen, sınıfın birden fazla 'Dispose' çağrısına karşı güvenli hale getirilmesi önerilir. Bu, sınıfınıza bir "private bool" değişkeni ekleyerek ve "Dispose" yöntemi çalıştığında değeri "true" olarak ayarlayarak başarılabilir.

## Yalnızca yönetilen kaynakları içeren bir sınıfta
Yönetilen kaynaklar, çalışma zamanının çöp toplayıcısının bildiği ve denetimi altındaki kaynaklardır. BCL'de, örneğin yönetilmeyen bir kaynak için sarmalayıcı bir sınıf olan "SqlConnection" gibi birçok sınıf mevcuttur. Bu sınıflar zaten "IDisposable" arabirimini uygular - işiniz bittiğinde bunları temizlemek kodunuza bağlıdır.

Sınıfınız yalnızca yönetilen kaynaklar içeriyorsa, bir sonlandırıcı uygulamak gerekli değildir.

    public class ObjectWithManagedResourcesOnly : IDisposable
    {
        private SqlConnection sqlConnection = new SqlConnection();

        public void Dispose()
        {
            sqlConnection.Dispose();
        }
    }

## Yönetilen ve yönetilmeyen kaynaklara sahip bir sınıfta
Sonlandırmanın yönetilen kaynakları yok saymasına izin vermek önemlidir. Sonlandırıcı başka bir iş parçacığında çalışır -- sonlandırıcı çalıştığında yönetilen nesnelerin artık mevcut olmaması mümkündür. Korumalı bir "Dispose(bool)" yönteminin uygulanması, yönetilen kaynakların bir sonlandırıcıdan çağrılan "Dispose" yöntemine sahip olmadığından emin olmak için yaygın bir uygulamadır.

    public class ManagedAndUnmanagedObject : IDisposable
    {
        private SqlConnection sqlConnection = new SqlConnection();
        private UnmanagedHandle unmanagedHandle = Win32.SomeUnmanagedResource();
        private bool disposed;

        public void Dispose()
        {
            Dispose(true); // client called dispose
            GC.SuppressFinalize(this); // tell the GC to not execute the Finalizer
        }

        protected virtual void Dispose(bool disposeManaged)
        {
            if (!disposed)
            {
                if (disposeManaged)
                {
                    if (sqlConnection != null)
                    {
                        sqlConnection.Dispose();
                    }
                }
    
                unmanagedHandle.Release();

                disposed = true;
            }
        }

        ~ManagedAndUnmanagedObject()
        {
            Dispose(false);
        }
    }

## Tek Kullanımlık, Atma


## anahtar kelime kullanma
Bir nesne "IDisposable" arabirimini uyguladığında, "using" sözdizimi içinde oluşturulabilir:

    using (var foo = new Foo())
    {
        // do foo stuff
    } // when it reaches here foo.Dispose() will get called

    public class Foo : IDisposable
    {
        public void Dispose()
        {
            Console.WriteLine("dispose called");
        }
    }

[Demoyu görüntüle][1]

'use', bir 'try/finally' bloğu için [sözdizimsel şeker][2]'dir; yukarıdaki kullanım kabaca şu anlama gelir:

    {
        var foo = new Foo();
        try
        {
            // do foo stuff
        }
        finally
        {
            if (foo != null)
                ((IDisposable)foo).Dispose();
        }
    }

[1]: https://dotnetfiddle.net/StEPc2
[2]: https://en.wikipedia.org/wiki/Syntactic_sugar

## Yönetilen kaynaklara sahip devralınan bir sınıfta
'IDisposable' uygulayan bir sınıf oluşturmanız ve ardından yönetilen kaynakları da içeren sınıflar türetmeniz oldukça yaygındır. İstemcilerin sahip olabilecekleri tüm kaynakları temizleme becerisine sahip olmaları için "Dispose" yöntemini "virtual" anahtar sözcüğüyle işaretlemeniz önerilir.

    public class Parent : IDisposable
    {
        private ManagedResource parentManagedResource = new ManagedResource();

        public virtual void Dispose()
        {
            if (parentManagedResource != null)
            {
                parentManagedResource.Dispose();
            }
        }
    }

    public class Child : Parent
    {
        private ManagedResource childManagedResource = new ManagedResource();

        public override void Dispose()
        {
            if (childManagedResource != null)
            {
                childManagedResource.Dispose();
            }
            //clean up the parent's resources
            base.Dispose();
        }
    }

