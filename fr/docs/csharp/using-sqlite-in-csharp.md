---
title: "Utiliser SQLite en C#"
slug: "utiliser-sqlite-en-c"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Création de CRUD simple à l'aide de SQLite en C#
Tout d'abord, nous devons ajouter le support SQLite à notre application. Il y a deux façons de faire ça

- Téléchargez la DLL adaptée à votre système à partir de la <a href="https://sqlite.org/download.html">page de téléchargement SQLite</a>, puis ajoutez-la manuellement au projet
- Ajouter une dépendance SQLite via NuGet

Nous le ferons de la deuxième manière

Ouvrez d'abord le menu NuGet

[![entrez la description de l'image ici][1]][1]


et recherchez **System.Data.SQLite**, sélectionnez-le et appuyez sur **Installer**

[![entrez la description de l'image ici][2]][2]

L'installation peut également être effectuée à partir de [Package Manager Console] [3] avec

    PM> Install-Package System.Data.SQLite

Ou uniquement pour les fonctionnalités principales

    PM> Install-Package System.Data.SQLite.Core 

C'est tout pour le téléchargement, nous pouvons donc passer directement au codage.

Créez d'abord une base de données SQLite simple avec cette table et ajoutez-la en tant que fichier au projet

    CREATE TABLE User(
      Id INTEGER PRIMARY KEY AUTOINCREMENT,
      FirstName TEXT NOT NULL,
      LastName TEXT NOT NULL
    );

N'oubliez pas non plus de définir la propriété **Copier dans le répertoire de sortie** du fichier sur **Copier si plus récent** de **Copier toujours**, en fonction de vos besoins

[![entrez la description de l'image ici][4]][4]

Créez une classe appelée User, qui sera l'entité de base de notre base de données

    private class User
    {
        public string FirstName { get; set; }
        public string Lastname { get; set; }
    }

Nous allons écrire deux méthodes pour l'exécution des requêtes, la première pour insérer, mettre à jour ou supprimer de la base de données

    private int ExecuteWrite(string query, Dictionary<string, object> args)
    {
        int numberOfRowsAffected;

        //setup the connection to the database
        using (var con = new SQLiteConnection("Data Source=test.db"))
        {
            con.Open();
            
            //open a new command
            using (var cmd = new SQLiteCommand(query, con))
            {
                //set the arguments given in the query
                foreach (var pair in args)
                {
                    cmd.Parameters.AddWithValue(pair.Key, pair.Value);
                }

                //execute the query and get the number of row affected
                numberOfRowsAffected = cmd.ExecuteNonQuery();
            }

            return numberOfRowsAffected;
        }
    }

et le second pour lire à partir de la base de données

    private DataTable Execute(string query)
    {
        if (string.IsNullOrEmpty(query.Trim()))
            return null;

        using (var con = new SQLiteConnection("Data Source=test.db"))
        {
            con.Open();
            using (var cmd = new SQLiteCommand(query, con))
            {
                foreach (KeyValuePair<string, object> entry in args)
                {
                    cmd.Parameters.AddWithValue(entry.Key, entry.Value);
                }

                var da = new SQLiteDataAdapter(cmd);

                var dt = new DataTable();
                da.Fill(dt);

                da.Dispose();
                return dt;
            }
        }
    }


Passons maintenant à nos méthodes **CRUD**

Ajout d'un utilisateur

    private int AddUser(User user)
    {
        const string query = "INSERT INTO User(FirstName, LastName) VALUES(@firstName, @lastName)";

        //here we are setting the parameter values that will be actually 
        //replaced in the query in Execute method
        var args = new Dictionary<string, object>
        {
            {"@firstName", user.FirstName},
            {"@lastName", user.Lastname}
        };

        return ExecuteWrite(query, args);
    }


Modification de l'utilisateur

    private int EditUser(User user)
    {
        const string query = "UPDATE User SET FirstName = @firstName, LastName = @lastName WHERE Id = @id";

        //here we are setting the parameter values that will be actually 
        //replaced in the query in Execute method
        var args = new Dictionary<string, object>
        {
            {"@id", user.Id},
            {"@firstName", user.FirstName},
            {"@lastName", user.Lastname}
        };

        return ExecuteWrite(query, args);
    }
    
Suppression d'un utilisateur

    private int DeleteUser(User user)
    {
        const string query = "Delete from User WHERE Id = @id";

        //here we are setting the parameter values that will be actually 
        //replaced in the query in Execute method
        var args = new Dictionary<string, object>
        {
            {"@id", user.Id}
        };

        return ExecuteWrite(query, args);
    }

Obtenir un utilisateur par `Id`

    private User GetUserById(int id)
    {
        var query = "SELECT * FROM User WHERE Id = @id";

        var args = new Dictionary<string, object>
        {
            {"@id", id}
        };

        DataTable dt = ExecuteRead(query, args);

        if (dt == null || dt.Rows.Count == 0)
        {
            return null;
        }

        var user = new User
        {
            Id = Convert.ToInt32(dt.Rows[0]["Id"]),
            FirstName = Convert.ToString(dt.Rows[0]["FirstName"]),
            Lastname = Convert.ToString(dt.Rows[0]["LastName"])
        };

        return user;
    }


[1] : http://i.stack.imgur.com/owqid.png
[2] : http://i.stack.imgur.com/4N4MH.png
[3] : https://docs.nuget.org/ndocs/tools/package-manager-console
[4] : http://i.stack.imgur.com/baf9b.png

## Exécution de la requête

    using (SQLiteConnection conn = new SQLiteConnection(@"Data Source=data.db;Pooling=true;FailIfMissing=false"))
    {
        conn.Open();
        using (SQLiteCommand cmd = new SQLiteCommand(conn))
        {
           cmd.CommandText = "query";
           using (SqlDataReader dr = cmd.ExecuteReader())
           {
               while(dr.Read())
               {
                   //do stuff
               }
           }
        }
    }
    
*Remarque* : Définir `FailIfMissing` sur true crée le fichier `data.db` s'il est manquant. Cependant, le fichier sera vide. Ainsi, toutes les tables requises doivent être recréées.

