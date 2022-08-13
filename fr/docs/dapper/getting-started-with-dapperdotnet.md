---
title: "Premiers pas avec Dapper.NET"
slug: "premiers-pas-avec-dappernet"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installer Dapper à partir de Nuget
Soit rechercher dans l'interface graphique de Visual Studio :

Outils > Gestionnaire de packages NuGet > Gérer les packages pour la solution... (Visual Studio 2015)

[![capture d'écran de l'interface du gestionnaire de packages Visual Studio avec Dapper sélectionné][1]][1]

Ou exécutez cette commande dans une instance Nuget Power Shell pour installer la dernière version stable

    Install-Package Dapper

Ou pour une version spécifique

    Install-Package Dapper -Version 1.42.0

[1] : http://i.stack.imgur.com/sWn6V.png

## Utiliser Dapper en C#
    using System.Data;
    using System.Linq;
    using Dapper;
    
    class Program
    {
        static void Main()
        {
            using (IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true"))
            {
                db.Open();
                var result = db.Query<string>("SELECT 'Hello World'").Single();
                Console.WriteLine(result);
            }
        }
    }

Envelopper la connexion dans un [`Using` block](https://www.wikiod.com/fr/docs/c%23/38/using-statement/157/cleaner-dispose-syntax) fermera la connexion

## Utilisation de Dapper dans LINQPad
[LINQPad](http://www.linqpad.net/) est idéal pour tester les requêtes de base de données et inclut [l'intégration NuGet](http://www.linqpad.net/Purchase.aspx#NuGet). Pour utiliser Dapper dans LINQPad, appuyez sur **F4** pour ouvrir les propriétés de la requête, puis sélectionnez **Ajouter NuGet**. Recherchez **dapper dot net** et sélectionnez **Ajouter à la requête**. Vous voudrez également cliquer sur ** Ajouter des espaces de noms ** et mettre en surbrillance Dapper pour inclure les méthodes d'extension dans votre requête LINQPad.

Une fois Dapper activé, vous pouvez modifier la liste déroulante Langue en **Programme C#**, mapper les résultats de la requête aux classes C# et utiliser la méthode .Dump() pour inspecter les résultats :

void Main()
	{
using (IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true")){
db.Open();
var scalaire = db.Query<string>("SELECT GETDATE()").SingleOrDefault();
scalar.Dump("Ceci est un résultat scalaire de chaîne :");
			
var résultats = db.Query<monobjet>(@"
CHOISIR * DE (
VALEURS (1,'un'),
(2,'deux'),
(3, 'trois')
) AS matable(id,nom)");
results.Dump("Ceci est une table associée à une classe :");
		}
	}
	
// Définissez d'autres méthodes et classes ici
classe monobjet {
public int id { obtenir ; Positionner; }
nom de chaîne publique { obtenir ; Positionner; }
	}

Les résultats lors de l'exécution du programme ressembleraient à ceci :

[![Capture d'écran LINQPad][1]][1]


[1] : http://i.stack.imgur.com/swXB1.png

