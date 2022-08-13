---
title: "Create models"
slug: "create-models"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Model creation with the command line interface
In the node JS command prompt, inside your loopback project, type the following command to create a new model.

```
slc loopback:model
```

If you have installed LoopBack CLI tool, you can create model with:

    lb model

The command prompt will request informations about the model to create. In this example, a model `Foo` will be created.

```
Enter the model name: Foo
```
Models can be based on one of the built-in loopback classes. Persisted models are the most common choice, representing structured data that should be stored (= **persisted**) inside a database.  
```
[?] Select model's base class: (Use arrow keys)
  Model
❯ PersistedModel
  ACL
  AccessToken
  Application
  Change
  Checkpoint
```
It is possible to expose `Foo` through a REST API. The generated API will contain methods for reading, creating, updating and deleting instances of the `Foo` model.
```
[?] Expose Foo via the REST API? (Y/n) Y
```

To define all REST API urls for this model, Loopback uses the plural form. In the next prompt below, leaving the field empty means the `s` character will be appended to the model's name `Foo`.

For instance, loopback will define the endpoint `GET` `/Foos`.

But some names do not have such a plural form, such as `Repository` becomes `Repositories` in plural form. In those cases, type the plural form in those fields.
```
[?] Custom plural form (used to build REST URL):
```
TBD
```
? Common model or server only?
❯ common
  server
```

```
Let's add some Foo properties now.
Enter an empty property name when done.
[?] Property name: name
   invoke   loopback:property
[?] Property type: (Use arrow keys)
❯ string
  number
  boolean
  object
  array
  date
  buffer
  geopoint
  (other)
```

```
[?] Required? (y/N)
```

```
node .
```
Navigate to `localhost:3000/explorer` and observe the REST API generated for `Foo`

