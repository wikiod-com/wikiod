---
title: "Yönetilen Genişletilebilirlik Çerçevesi"
slug: "yonetilen-genisletilebilirlik-cercevesi"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

MEF'in denetimin tersine çevrilmesi modelini destekleyen diğer teknolojilere göre en büyük avantajlarından biri, tasarım zamanında bilinmeyen bağımlılıkları (varsa) fazla yapılandırmaya ihtiyaç duymadan çözmeyi desteklemesidir.

Tüm örnekler, System.ComponentModel.Composition derlemesine bir başvuru gerektirir.

Ayrıca, tüm (Temel) örnekler bunları örnek iş nesneleri olarak kullanır:

    using System.Collections.ObjectModel;

    namespace Demo
    {
        public sealed class User
        {
            public User(int id, string name)
            {
                this.Id = id;
                this.Name = name;
            }

            public int Id { get; }
            public string Name { get; }
            public override string ToString() => $"User[Id: {this.Id}, Name={this.Name}]";
        }

        public interface IUserProvider
        {
            ReadOnlyCollection<User> GetAllUsers();
        }
    }

## Bağlanıyor (Temel)
Yukarıdaki diğer (Temel) örneklere bakın.

    using System.ComponentModel.Composition;
    using System.ComponentModel.Composition.Hosting;

    namespace Demo
    {
        public static class Program
        {
            public static void Main()
            {
                using (var catalog = new ApplicationCatalog())
                using (var exportProvider = new CatalogExportProvider(catalog))
                using (var container = new CompositionContainer(exportProvider))
                {
                    exportProvider.SourceProvider = container;

                    UserWriter writer = new UserWriter();

                    // at this point, writer's userProvider field is null
                    container.ComposeParts(writer);

                    // now, it should be non-null (or an exception will be thrown).
                    writer.PrintAllUsers();
                }
            }
        }
    }

Uygulamanın derleme arama yolunda bir şey `[Export(typeof(IUserProvider))]` değerine sahip olduğu sürece, `UserWriter`ın ilgili içe aktarımı karşılanacak ve kullanıcılar yazdırılacaktır.

Diğer katalog türleri (örneğin, "DirectoryCatalog"), "ApplicationCatalog" yerine (veya buna ek olarak) içe aktarmayı karşılayan başka yerlere bakmak için kullanılabilir.

## Bir Türü Dışa Aktarma (Temel)
    using System.Collections.Generic;
    using System.Collections.ObjectModel;
    using System.ComponentModel.Composition;

    namespace Demo
    {
        [Export(typeof(IUserProvider))]
        public sealed class UserProvider : IUserProvider
        {
            public ReadOnlyCollection<User> GetAllUsers()
            {
                return new List<User>
                {
                    new User(0, "admin"),
                    new User(1, "Dennis"),
                    new User(2, "Samantha"),
                }.AsReadOnly();
            }
        }
    }

Bu hemen hemen her yerde tanımlanabilir; önemli olan tek şey, uygulamanın onu nerede arayacağını bilmesidir (oluşturduğu ComposablePartCatalogs aracılığıyla).

## İçe Aktarma (Temel)

    using System;
    using System.ComponentModel.Composition;

    namespace Demo
    {
        public sealed class UserWriter
        {
            [Import(typeof(IUserProvider))]
            private IUserProvider userProvider;

            public void PrintAllUsers()
            {
                foreach (User user in this.userProvider.GetAllUsers())
                {
                    Console.WriteLine(user);
                }
            }
        }
    }

Bu, herhangi bir yerde tanımlanabilecek bir "IUserProvider"a bağımlı olan bir türdür. Önceki örnekte olduğu gibi, önemli olan tek şey, uygulamanın eşleşen dışa aktarmayı nerede arayacağını bilmesidir (oluşturduğu ComposablePartCatalogs aracılığıyla).

