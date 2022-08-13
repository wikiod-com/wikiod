---
title: "Ecto models usage in phoenix"
slug: "ecto-models-usage-in-phoenix"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

How to generate, edit and use ecto models in the phoenix frameworks.

## Generate User model from command line
To generate `json` user model with `username`, `password_hash`, `email_id`, `created_at`, `updated_at`, type

`mix phoenix.gen.json User users username:string email_id:string password_hash:string timestamps()` 


## Migrations of ecto model
When you run `mix phoenix.gen.html` or `mix phoenix.gen.json` from command line, migrations are created in `priv -> repo -> migrations` in your project folder. 

To run migrations type `mix ecto.migrate`.

To generate migrations for your project `mix ecto.gen migrations <model_name>`

To generate migrations for a different repository than default one run `mix ecto.gen migrations <model_name> -r <repo_name>`

