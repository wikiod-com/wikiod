---
title: "Usando SQLite em C#"
slug: "usando-sqlite-em-c"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Criando CRUD simples usando SQLite em C#
Antes de tudo, precisamos adicionar suporte SQLite ao nosso aplicativo. Existem duas maneiras de fazer isso

- Faça o download da DLL adequada ao seu sistema na <a href="https://sqlite.org/download.html">página de download do SQLite</a> e adicione ao projeto manualmente
- Adicionar dependência SQLite via NuGet

Nós vamos fazer isso da segunda maneira

Primeiro abra o menu NuGet

[![digite a descrição da imagem aqui][1]][1]


e procure por **System.Data.SQLite**, selecione-o e clique em **Instalar**

[![digite a descrição da imagem aqui][2]][2]

A instalação também pode ser feita a partir do [Console do Gerenciador de Pacotes][3] com

    PM> Install-Package System.Data.SQLite

Ou apenas para recursos principais

    PM> Install-Package System.Data.SQLite.Core 

É isso para o download, para que possamos ir direto para a codificação.

Primeiro crie um banco de dados SQLite simples com esta tabela e adicione-o como um arquivo ao projeto

    CREATE TABLE User(
      Id INTEGER PRIMARY KEY AUTOINCREMENT,
      FirstName TEXT NOT NULL,
      LastName TEXT NOT NULL
    );

Além disso, não se esqueça de definir a propriedade **Copiar para o diretório de saída** do arquivo como **Copiar se for mais recente** de **Copiar sempre**, com base em suas necessidades

[![digite a descrição da imagem aqui][4]][4]

Crie uma classe chamada User, que será a entidade base do nosso banco de dados

    private class User
    {
        public string FirstName { get; set; }
        public string Lastname { get; set; }
    }

Vamos escrever dois métodos para execução de consultas, primeiro para inserir, atualizar ou remover do banco de dados

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

e o segundo para leitura do banco de dados

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


Agora vamos entrar em nossos métodos **CRUD**

Adicionando usuário

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


Editando usuário

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
    
Excluindo usuário

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

Obtendo usuário por 'Id'

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

## Executando a consulta

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
    
*Observação*: Definir `FailIfMissing` como true cria o arquivo `data.db` se estiver ausente. No entanto, o arquivo estará vazio. Portanto, todas as tabelas necessárias devem ser recriadas.

