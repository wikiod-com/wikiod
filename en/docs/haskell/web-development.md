---
title: "Web Development"
slug: "web-development"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Servant
[Servant][1] is a library for declaring APIs at the type-level and then:

> * write servers (this part of servant can be considered a web framework),
> * obtain client functions (in haskell),
> * generate client functions for other programming languages,
> * generate documentation for your web applications
> * and more...

Servant has a concise yet powerful API. A simple API can be written in very few lines of code:

    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE TypeOperators #-}
    
    import Data.Text
    import Data.Aeson.Types
    import GHC.Generics
    import Servant.API
    
    data SortBy = Age | Name
    
    data User = User {
      name :: String,
      age :: Int
    } deriving (Eq, Show, Generic)

    instance ToJSON User  -- automatically convert User to JSON

Now we can declare our API:

    type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

which states that we wish to expose `/users` to `GET` requests with a query param `sortby` of type `SortBy` and return JSON of type `User` in the response.

Now we can define our handler:

    -- This is where we'd return our user data, or e.g. do a database lookup
    server :: Server UserAPI
    server = return [User "Alex" 31]
    
    userAPI :: Proxy UserAPI
    userAPI = Proxy
    
    app1 :: Application
    app1 = serve userAPI server

And the main method which listens on port `8081` and serves our user API:

    main :: IO ()
    main = run 8081 app1

Note, [Stack][2] has a template for generating basic APIs in Servant, which is useful for getting up and running very quick.

  [1]: http://haskell-servant.readthedocs.io/en/stable/
  [2]: https://www.wikiod.com/haskell/stack

## Yesod
Yesod project can be created with `stack new` using following templates:

- `yesod-minimal`. Simplest Yesod scaffold possible.
- `yesod-mongo`. Uses MongoDB as DB engine.
- `yesod-mysql`. Uses MySQL as DB engine.
- `yesod-postgres`. Uses PostgreSQL as DB engine.
- `yesod-postgres-fay`. Uses PostgreSQL as DB engine. Uses Fay language for front-end.
- `yesod-simple`. Recommended template to use, if you don't need database.
- `yesod-sqlite`. Uses SQlite as DB engine.

`yesod-bin` package provides `yesod` executable, which can be used to run development server. Note that you also can run your application directly, so `yesod` tool is optional.

`Application.hs` contains code that dispatches requests between handlers. It also sets up database and logging settings, if you used them.

`Foundation.hs` defines `App` type, that can be seen as an environment for all handlers. Being in `HandlerT` monad, you can get this value using `getYesod` function.

`Import.hs` is a module that just re-exports commonly used stuff.

`Model.hs` contains Template Haskell that generates code and data types used for DB interaction. Present only if you are using DB.

`config/models` is where you define your DB schema. Used by `Model.hs`.

`config/routes` defines URI's of the Web application. For each HTTP method of the route, you'd need to create a handler named `{method}{RouteR}`.

`static/` directory contains site's static resources. These get compiled into binary by `Settings/StaticFiles.hs` module.

`templates/` directory contains [Shakespeare][1] templates that are used when serving requests.

Finally, `Handler/` directory contains modules that define handlers for routes.

Each handler is a `HandlerT` monad action based on IO. You can inspect request parameters, its body and other information, make queries to the DB with `runDB`, perform arbitrary IO and return various types of content to the user. To serve HTML, `defaultLayout` function is used that allows neat composition of shakespearian templates.

  [1]: https://hackage.haskell.org/package/shakespeare

