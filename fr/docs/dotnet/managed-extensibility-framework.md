---
title: "Cadre d'extensibilité gérée"
slug: "cadre-dextensibilite-geree"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

L'un des grands avantages de MEF par rapport aux autres technologies qui prennent en charge le modèle d'inversion de contrôle est qu'il prend en charge la résolution des dépendances qui ne sont pas connues au moment de la conception, sans nécessiter beaucoup (le cas échéant) de configuration.

Tous les exemples nécessitent une référence à l'assembly System.ComponentModel.Composition.

En outre, tous les exemples (de base) les utilisent comme exemples d'objets métier :

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

## Connexion (de base)
Voir les autres exemples (de base) ci-dessus.

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

Tant que quelque chose dans le chemin de recherche d'assemblage de l'application a `[Export(typeof(IUserProvider))]`, l'importation correspondante de `UserWriter` sera satisfaite et les utilisateurs seront imprimés.

D'autres types de catalogues (par exemple, `DirectoryCatalog`) peuvent être utilisés à la place (ou en plus) de `ApplicationCatalog`, pour rechercher ailleurs les exportations qui satisfont les importations.

## Exportation d'un type (basique)
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

Cela pourrait être défini pratiquement n'importe où; tout ce qui compte est que l'application sache où le chercher (via les ComposablePartCatalogs qu'il crée).

## Importation (de base)

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

Il s'agit d'un type qui dépend d'un `IUserProvider`, qui peut être défini n'importe où. Comme dans l'exemple précédent, tout ce qui compte est que l'application sache où chercher l'export correspondant (via les ComposablePartCatalogs qu'elle crée).

