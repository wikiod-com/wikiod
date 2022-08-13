---
title: "Getting started with phoenix-framework"
slug: "getting-started-with-phoenix-framework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Skeleton Installation
Sometimes you want an installation without anything except the bare minimum phoenix setup. The follow command will give you that.

    mix phoenix.new web --no-brunch --no-ecto

**Note:** You must have installed Elixir, Erlang, Hex, Mix and the Phoenix archive for skeleton installation

## Creating Phoenix project
For creating your first project in Phoenix framework at this point **you should have**, Elixir, Erlang, Hex, and the Phoenix archive installed. You should also have PostgreSQL and node.js installed to build a default application.

Open terminal or command prompt and go to location on your file system where you want to **create application**. `phoenix.new` is the mix command which will create new project for you. Assuming that the name of our application is `hello_phoenix_world`, then type

    $ mix phoenix.new hello_phoenix_world

**Alternately**, We can run mix phoenix.new from any directory in order to bootstrap our Phoenix application. Phoenix will accept either an absolute or relative path for the directory of our new project

    $ mix phoenix.new /Users/username/work/elixir-projects/hello_phoenix_world

**Output**

    mix phoenix.new hello_phoenix_world
    * creating hello_phoenix_world/config/config.exs
    * creating hello_phoenix_world/config/dev.exs
    * creating hello_phoenix_world/config/prod.exs
    ...
    * creating hello_phoenix_world/web/views/layout_view.ex
    * creating hello_phoenix_world/web/views/page_view.ex
    
    Fetch and install dependencies? [Yn]

Phoenix will generate the directory structure for your project and it will create all the files required for application. Mix will ask you if you want it to **install other required dependencies**. Let's say yes to that.

    Fetch and install dependencies? [Yn] Y
    * running mix deps.get
    * running npm install && node node_modules/brunch/bin/brunch build

Once **dependencies are installed**, the task will prompt you to change into our project directory and start application.

    Move into your new project folder:

        $cd hello_phoenix_world

You now need to setup the postgres username and password unless its already setup with the default postgres useranme and postgres password. Edit your `config/dev.exs` file and set the username and password:

    # config/dev.exs
    config :hello_phoenix_world, HelloPhoenixWorld.Repo,
      adapter: Ecto.Adapters.Postgres,
      username: "postgres",
      password: "postgres",
      database: "hello_phoenix_world_dev",
      hostname: "localhost",
      pool_size: 10
      
    Now, create the database with the ecto mix task:

        $ mix ecto.create

    We have a working application! Run your Phoenix application:
    
        $ mix phoenix.server
    
    You can also run your app inside IEx (Interactive Elixir) as:
    
        $ iex -S mix phoenix.server

    Load `http://localhost:4000` into your browser and you will see the default landing page of your application.

Now, lets add hello world to the Phoenix application. Open the `web/templates/page/index.html.eex` file and replace the contents with the following and save the file:

    <h2>Hello World</h2>

If you have not quit the server, the new code will be automatically compiled and your browser should now display your "Hello World" message.

You can now [create CRUD resource](https://www.wikiod.com/phoenix-framework/getting-started-with-phoenix-framework#Generating resources for a model).

Finally, to exit out of the server, type `ctrl-c` `crtl-c` (press the `control key` and the `c` key together) twice in a row.




## Installation
[Phoenix framework][1] is written in [Elixir][2], and Elixir itself is based on [Erlang][3] language and leverages the Erlang VM, known for running low-latency, distributed and fault-tolerant systems. Both languages are required for using phoenix framework. Following following step to install phoenix framework:

**1. Install Elixir** on your machine. See [Elixir Installation][4] and how to [install  Elixir guide][5].
    
**2. Install Hex** package manager. [Hex][6] is a necessary tool to get a Phoenix app running and to install any extra dependencies we might need along the way. From your terminal or command control window, type:

    $ mix local.hex

This command will install or update Hex, if you already have.

**3. Install Erlang** on your machine. Without Erlang, Elixir code will not compile because Elixir use Erlang's VM for code compilation. When you will install Elixir, you have probably installed Erlang too, but if it is not the case then follow [these instruction][7] on Elixir guide to install Erlang. However, If you are have Debian-based system, you may need to explicitly install Erlang.

    $ wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && sudo dpkg -i erlang-solutions_1.0_all.deb
    $ sudo apt-get update
    $ sudo apt-get install esl-erlang

**4. Install phoenix framework** on your machine. Once we have Elixir and Erlang, we are ready to install the Phoenix Mix archive. A Mix archive is a Zip file which contains an application as well as its compiled BEAM files. It is tied to a specific version of the application. The archive is what we will use to generate a new, base Phoenix application which we can build from. Here's the command to install the Phoenix archive:

    $ mix archive.install https://github.com/phoenixframework/archives/raw/master/phoenix_new.ez

Your can download packages manually, if above command doesn't work properly for you. Download packages to your file system [Phoenix archives][8] and run following command

    mix archive.install /path/to/local/phoenix_new.ez

**5 Plug, Cowboy, and Ecto** are components of Phoenix framework, they will be installed automatically by mix, if you let mix install its dependencies, when you will first create Phoenix projects. Furthermore, if you don't allow mix to download these components then mix will tell you how how to do so later.

**6. Install Node.js** (not less then v5.0.0) on your machine. This is an **optional** dependency. [Node.js][9] is required to install [brunch.io][10] dependencies. Brunch.io is used by Phoenix for compiling static assets (javascript, css, etc), by default.

We can get node.js from the [download page][11]. When selecting a package to download, it's important to note that Phoenix requires version 5.0.0 or greater.

Mac OS X users can also install node.js via [homebrew][12].

Note: io.js, which is an npm compatible platform originally based on Node.js, is not known to work with Phoenix.

Debian/Ubuntu users might see an error that looks like this:

    sh: 1: node: not found
    npm WARN This failure might be due to the use of legacy binary "node"

This is due to Debian having conflicting binaries for node: see discussion on following SO question

http://stackoverflow.com/questions/21168141/cannot-install-packages-using-node-package-manager-in-ubuntu

There are two options to fix this problem, either:

install nodejs-legacy:

    $ apt-get install nodejs-legacy

or
create a symlink

    $ ln -s /usr/bin/nodejs /usr/bin/node

**7 Install Database** ([PostgreSQL][13]) on your machine. Phoenix configures applications to use it by default, but we can switch to [MySQL][14] by passing the `--database mysql` flag when creating a new application. The PostgreSQL wiki has [installation guides][15] for a number of different systems.

Postgrex is a direct Phoenix dependency and it will be used to create models. **Postgrex** will be automatically installed along with the rest of dependencies when you will create and start Phoenix project.

**8 inotify-tools** (for linux users)
This is a Linux-only filesystem watcher that Phoenix uses for live code reloading. (Mac OS X or Windows users can safely ignore it.)

Linux users need to install this dependency. Please consult the [inotify-tools wiki][16] for distribution-specific installation instructions.


  [1]: http://www.phoenixframework.org/
  [2]: http://elixir-lang.org/
  [3]: https://www.erlang.org/
  [4]: https://www.wikiod.com/elixir/installation
  [5]: http://elixir-lang.org/getting-started/introduction.html#installation
  [6]: https://hex.pm/
  [7]: http://elixir-lang.org/install.html#installing-erlang
  [8]: https://github.com/phoenixframework/archives
  [9]: https://nodejs.org/en/
  [10]: http://brunch.io/
  [11]: https://nodejs.org/en/download/
  [12]: http://brew.sh/
  [13]: https://www.postgresql.org/
  [14]: https://www.mysql.com/
  [15]: https://wiki.postgresql.org/wiki/Detailed_installation_guides
  [16]: https://github.com/rvoicilas/inotify-tools/wiki

## Running Elixir/Phoenix on OSX
**Elixir / Phoenix**

Install [Homebrew][1] first:

    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

Then running `brew install elixir` will install both Elixir and it's dependency - Erlang.

Install mix with `mix local.hex`.

Install Phoenix as per instructions:

    mix archive.install https://github.com/phoenixframework/archives/raw/master/phoenix_new.ez

**Node.js**

You can install and manage your Node.js versions with NVM. Install [nvm][2] with:

    curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.4/install.sh | bash

If `curl` is not available, you can install it with `brew install curl`. Then run:

    nvm install node

to download and compile and latest version of Node.js.

**Database**

Download [Postgres.app][3] and run it. When you create your Phoenix project, in your `config/dev.exs` file, you just need to supply a name for your database - the adapter will use default values for the rest:

    config :myphoenixapp, MyPhoenixApp.Repo,
      adapter: Ecto.Adapters.Postgres,
      database: "myphoenixapp_dev",
      hostname: "localhost",
      pool_size: 10


  [1]: http://brew.sh/
  [2]: https://github.com/creationix/nvm
  [3]: http://postgresapp.com/


## Generating resources for a model
To generate schema, view, controller, migration file for the repository, default CRUD templates  and test files for a model (like a scaffolding in Rails) one can use `phoenix.gen.html` mix task like this:

    mix phoenix.gen.html Book books title note:text pages:integer author_id:references:authors

Where `Book` is the module name, `books` is plural form used for schema, followed by resource fields: `title` (string by default), `note` (text field), `pages` (integer), `author_id` which creates a `belongs_to` association with the Author model. 

