---
title: "Marco de extensibilidad administrado"
slug: "marco-de-extensibilidad-administrado"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

Una de las grandes ventajas de MEF sobre otras tecnologías que admiten el patrón de inversión de control es que admite la resolución de dependencias que no se conocen en el momento del diseño, sin necesidad de mucha configuración (si es que la hay).

Todos los ejemplos requieren una referencia al ensamblado System.ComponentModel.Composition.

Además, todos los ejemplos (básicos) los usan como sus objetos comerciales de muestra:

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

## Conexión (básico)
Consulte los otros ejemplos (básicos) anteriores.

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

Siempre que algo en la ruta de búsqueda del ensamblado de la aplicación tenga `[Export(typeof(IUserProvider))]`, se cumplirá la importación correspondiente de `UserWriter` y se imprimirán los usuarios.

Se pueden usar otros tipos de catálogos (por ejemplo, `DirectoryCatalog`) en lugar de (o además de) `ApplicationCatalog`, para buscar en otros lugares exportaciones que satisfagan las importaciones.

## Exportación de un tipo (básico)
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

Esto podría definirse prácticamente en cualquier lugar; lo único que importa es que la aplicación sepa dónde buscarlo (a través de ComposablePartCatalogs que crea).

## Importación (básico)

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

Este es un tipo que tiene una dependencia en un `IUserProvider`, que podría definirse en cualquier lugar. Al igual que en el ejemplo anterior, todo lo que importa es que la aplicación sepa dónde buscar la exportación coincidente (a través de ComposablePartCatalogs que crea).

