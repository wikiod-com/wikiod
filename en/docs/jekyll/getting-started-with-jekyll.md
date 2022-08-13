---
title: "Getting started with jekyll"
slug: "getting-started-with-jekyll"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
## Quickstart for Jekyll

     $ gem install jekyll
     $ jekyll new my-awesome-site
     $ cd my-awesome-site
    ~/my-awesome-site $ jekyll serve

Now browse to http://localhost:4000

---

## Quickstart for Jekyll with Bundler

     $ gem install jekyll bundler
     $ jekyll new my-awesome-site
     $ cd my-awesome-site
    ~/my-awesome-site $ bundle exec jekyll serve

Now browse to http://localhost:4000

## Basic Usage
The Jekyll gem makes a jekyll executable available to you in your Terminal window. You can use this command in a number of ways:
```
$ jekyll build
# => The current folder will be generated into ./_site

$ jekyll build --destination <destination>
# => The current folder will be generated into <destination>

$ jekyll build --source <source> --destination <destination>
# => The <source> folder will be generated into <destination>

$ jekyll build --watch
# => The current folder will be generated into ./_site,
#    watched for changes, and regenerated automatically.
```
Jekyll also comes with a built-in development server that will allow you to preview what the generated site will look like in your browser locally.
```
$ jekyll serve
# => A development server will run at http://localhost:4000/
# Auto-regeneration: enabled. Use `--no-watch` to disable.
```

## Create Jekyll Post And Pages
# Create new Jekyll Post

To create a new Jekyll Post, create a new file on `_posts` directory with the format
```
YYYY-MM-DD-title.MARKUP
```
Replace `MARKUP` with the file extension for the language you want to use. This is usually Markdown(.md or .markdown) or HTML(.html).

```
_posts/2017-01-01-hello-jekyll.md
```

# Create new Jekyll Page

To create a new Jekyll Page, create a new file on any folder or directory not excluded by Jekyll in your project directory.

```
about.html
contact/company_info.md
```

> **NOTE:** Both `Page` and `Post` files require Front Matter dashes to be considered for processing. Otherwise, they're simply designated as a `StaticFile`.
>
> Front Matter dashes should be at the very beginning, before your content, and simply look like this:
>```
>---
>---
>
>< your content >
>```

## Install Jekyll on Linux Mint 18
Install jekyll on Linux Mint 18 with the following steps:

    sudo apt install ruby
    sudo apt install build-essential 
    sudo apt install ruby-dev
    sudo gem install jekyll

## Install Jekyll on Windows
1. Open a command prompt with Administrator access
2. Install Chocolatey: ```@powershell -NoProfile -ExecutionPolicy Bypass -Command "iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))" && SET PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin```
3. Close the command prompt as Chocolatey will not be available until you close and reopen.
4. Open a command prompt with Administrator access
6. Intall Ruby: ````choco install ruby -y````
7. Close and open a new command prompt with Administrator access
8. Install Jekyll: ````gem install jekyll````

Found this guide [here](https://davidburela.wordpress.com/2015/11/28/easily-install-jekyll-on-windows-with-3-command-prompt-entries-and-chocolatey/).

