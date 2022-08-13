---
title: "Usando SQLite en C#"
slug: "usando-sqlite-en-c"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Creando CRUD simple usando SQLite en C#
En primer lugar, debemos agregar soporte SQLite a nuestra aplicación. Hay dos formas de hacerlo

- Descargue DLL adecuado para su sistema desde la <a href="https://sqlite.org/download.html">página de descarga de SQLite</a> y luego agréguelo al proyecto manualmente
- Agregar dependencia de SQLite a través de NuGet

Lo haremos de la segunda manera.

Primero abra el menú NuGet

[![ingrese la descripción de la imagen aquí][1]][1]


y busque **System.Data.SQLite**, selecciónelo y presione **Instalar**

[![ingrese la descripción de la imagen aquí][2]][2]

La instalación también se puede realizar desde la [Consola del administrador de paquetes][3] con

    PM> Install-Package System.Data.SQLite

O solo para funciones principales

    PM> Install-Package System.Data.SQLite.Core 

Eso es todo para la descarga, así que podemos pasar directamente a la codificación.

Primero cree una base de datos SQLite simple con esta tabla y agréguela como un archivo al proyecto

    CREATE TABLE User(
      Id INTEGER PRIMARY KEY AUTOINCREMENT,
      FirstName TEXT NOT NULL,
      LastName TEXT NOT NULL
    );

Además, no olvide establecer la propiedad **Copiar en el directorio de salida** del archivo en **Copiar si es más nuevo** o **Copiar siempre**, según sus necesidades.

[![ingrese la descripción de la imagen aquí][4]][4]

Cree una clase llamada Usuario, que será la entidad base para nuestra base de datos.

    private class User
    {
        public string FirstName { get; set; }
        public string Lastname { get; set; }
    }

Escribiremos dos métodos para la ejecución de consultas, el primero para insertar, actualizar o eliminar de la base de datos

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

y el segundo para leer de la base de datos.

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


Ahora entremos en nuestros métodos **CRUD**

Agregar usuario

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


Editando usuario

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
    
Eliminando usuario

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

Obtener usuario por `Id`

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


[1]: http://i.stack.imgur.com/owqid.png
[2]: http://i.stack.imgur.com/4N4MH.png
[3]: https://docs.nuget.org/ndocs/tools/package-manager-console
[4]: http://i.stack.imgur.com/baf9b.png

## Ejecutando consulta

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
    
*Nota*: Establecer `FailIfMissing` en verdadero crea el archivo `data.db` si falta. Sin embargo, el archivo estará vacío. Por lo tanto, todas las tablas requeridas deben volver a crearse.

