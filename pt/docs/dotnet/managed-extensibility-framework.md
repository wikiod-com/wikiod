---
title: "Estrutura de Extensibilidade Gerenciada"
slug: "estrutura-de-extensibilidade-gerenciada"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

Uma das grandes vantagens do MEF sobre outras tecnologias que suportam o padrão de inversão de controle é que ele suporta a resolução de dependências que não são conhecidas em tempo de design, sem precisar de muita configuração (se houver).

Todos os exemplos requerem uma referência ao assembly System.ComponentModel.Composition.

Além disso, todos os exemplos (Básicos) os usam como objetos de negócios de amostra:

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

## Conectando (Básico)
Veja os outros exemplos (básicos) acima.

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

Contanto que algo no caminho de pesquisa do assembly do aplicativo tenha `[Export(typeof(IUserProvider))]`, a importação correspondente do `UserWriter` será satisfeita e os usuários serão impressos.

Outros tipos de catálogos (por exemplo, `DirectoryCatalog`) podem ser usados ​​em vez de (ou além de) `ApplicationCatalog`, para procurar em outros lugares por exportações que satisfaçam as importações.

## Exportando um tipo (básico)
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

Isso pode ser definido virtualmente em qualquer lugar; tudo o que importa é que o aplicativo saiba onde procurá-lo (através dos ComposablePartCatalogs que ele cria).

## Importação (Básico)

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

Este é um tipo que depende de um `IUserProvider`, que pode ser definido em qualquer lugar. Como no exemplo anterior, tudo o que importa é que o aplicativo saiba onde procurar a exportação correspondente (por meio do ComposablePartCatalogs que ele cria).

