---
title: "Ecto"
slug: "ecto"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Adding a Ecto.Repo in an elixir program
This can be done in 3 steps :
 
1. You must define an elixir module which use Ecto.Repo and register your app as an otp_app.
 
       defmodule Repo do
         use Ecto.Repo, otp_app: :custom_app
       end

2. You must also define some config for the Repo which will allow you to connect to the database. Here is an example with postgres.

       config :custom_app, Repo,
          adapter: Ecto.Adapters.Postgres,
          database: "ecto_custom_dev",
          username: "postgres_dev",
          password: "postgres_dev",
          hostname: "localhost",
         # OR use a URL to connect instead
         url: "postgres://postgres_dev:postgres_dev@localhost/ecto_custom_dev"

3. Before using Ecto in your application, you need to ensure that Ecto is started before your app is started. It can be done with registering Ecto in lib/custom_app.ex as a supervisor.

           def start(_type, _args) do
             import Supervisor.Spec

             children = [
              supervisor(Repo, [])
             ]

             opts = [strategy: :one_for_one, name: MyApp.Supervisor]
             Supervisor.start_link(children, opts)
           end



## "and" clause in a Repo.get_by/3
If you have an Ecto.Queryable, named Post, which has a title and an description. 

You can fetch the Post with title: "hello" and description : "world" by performing : 

     MyRepo.get_by(Post, [title: "hello", description: "world"])

All of this is possible because Repo.get_by expects in second argument a Keyword List.

## Querying with dynamic fields
To query a field which name is contained in a variable, use the [field function][1].

    some_field = :id
    some_value = 10

    from p in Post, where: field(p, ^some_field) == ^some_value


  [1]: https://hexdocs.pm/ecto/Ecto.Query.API.html#field/2

## Add custom data types to migration and to schema
[*(From this answer)*][1]

The example below adds an [enumerated type][2] to a postgres database.

First, edit the **migration file** (created with `mix ecto.gen.migration`):

    def up do
      # creating the enumerated type
      execute("CREATE TYPE post_status AS ENUM ('published', 'editing')")
    
      # creating a table with the column
      create table(:posts) do
        add :post_status, :post_status, null: false
      end
    end
    
    def down do
      drop table(:posts)
      execute("DROP TYPE post_status")
    end

Second, in the **model file** either add a field with an Elixir type :

    schema "posts" do
      field :post_status, :string
    end

or implement the [`Ecto.Type`][3] behaviour.

A good example for the latter is the [`ecto_enum`][4] package and it can be used as a template. Its usage is well documented on its **[github page][5]**. 

[This commit][6] shows an example usage in a Phoenix project from adding enum_ecto to the project and using the enumerated type in views and models.

  


  [1]: http://stackoverflow.com/questions/35245859/how-to-use-postgres-enumerated-type-with-ecto
  [2]: https://www.postgresql.org/docs/current/static/datatype-enum.html
  [3]: https://hexdocs.pm/ecto/Ecto.Type.html
  [4]: https://hex.pm/packages/ecto_enum
  [5]: https://github.com/gjaldon/ecto_enum
  [6]: https://github.com/society-for-the-blind/timesheets/commit/6b1362a5a9b81bc46032b3a900af42824c6cd8cc

