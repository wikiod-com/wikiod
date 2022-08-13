---
title: "Hosting"
slug: "hosting"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## GitHub Pages
GitHub offers unlimited hosting for users or organizations and project site. Both Jekyll and static files are available.

Here are the steps in hosting your Jekyll blog on Github.

# Setup

## Users or organizations site

1. Create a repository named **username.github.io**, where username is your username (or organization name) on GitHub.
2. Clone the repository onto your computer:
    ```
    $ git clone https://github.com/username/username.github.io
    ```
3. Enter the project folder, [bootstrap](https://www.wikiod.com/jekyll/getting-started-with-jekyll), design and debug your site:
    ```
    $ cd username.github.io
    $ bundle install
    $ bundle exec jekyll serve
    ```
4. Commit and push the repository:
    ```
    $ git add --all
    $ git commit -m "Initial commit"
    $ git push -u origin master
    ```

Now you should be able to go to **username.github.io** to see your blog.

## Project site

Project site can be enabled in every repository including private repositories.

1. Enable project site.

    Go to Settings-GitHub Pages-Sources, choose a source to switch on GitHub Pages for the repository.

2. Build site

    You may build a Jekyll site from scratch or use Theme Chooser to find a theme for your project site.

3. Edit content
4. Commit

Now you should be able to go to **username.github.io/your-project** to see your project site.

# Custom Domains

1. Open Settings->GitHub Pages->Custom domain, add your custom domain.
2. Create a `CNAME` file:
    ```
    $ cd username.github.io
    $ echo "example.com" > CNAME
3. Commit and push
    ```
    $ git commit -m "Add CNAME" CNAME
    $ git push -u origin master
    ```

# Restrictions

## Plugins

Jekyll has a plugin system with hooks that allow you to create custom generated content specific to your site. However, GitHub Pages only allows a white list of plugins for security reasons.

Here is the white list:

* Jekyll Sitemap
* Jekyll SEO Tag
* github-metadata
* Jekyll Feed
* Jekyll Redirect From
* Jemoji
* Jekyll Mentions

To avoid the inconsistency with GitHub Pages, you may use `--safe` to serve in local.

You can still use all plugins by publishing your generated site to GitHub Pages, by converting the site locally and pushing the generated static files to your GitHub repository instead of the Jekyll source files.

## Markdown Engine

Since 01/05/2016, GitHub Pages supports only kramdown as Markdown engine.

See <https://github.com/blog/2100-github-pages-now-faster-and-simpler-with-jekyll-3-0> for more detail.

# Sources

GitHub allows you to set Jekyll sources to either `master` branch, `gh-pages` branch or `/docs` folder in `master` branch.

> A full tutorial is available at https://pages.github.com/ 

## Local machine
For testing purposes, you can host your blog on your local machine. After [setting up](http://jekyllrb.com/docs/quickstart/) and making any changes, Jekyll can server the blog to http://localhost:4000. On the command line in the root directory of the project, run:

    $ bundle exec jekyll serve

The `bundle exec` part is optional, but if you use [Bundler](http://bundler.io/), it ensures the gem dependacies are up-to-date. For a quicker [Edit-Build-Test loop](http://www.joelonsoftware.com/articles/fog0000000023.html), use the `--draft` option to build articles from the `_drafts` directory:

    $ bundle exec jekyll serve --draft --detach

Using `--detach` puts the process in the background so that the command prompt can be used for something else.

([As of version 2.4](http://jekyllrb.com/docs/usage/), the `--watch` option is enabled by default. If, by chance, you have an older version you'll need to add that option so that changes are monitored.)



You can also set the destination directory to a directory on a different web server such as Apache, nginx, or Lighttpd:

    $ jekyll build --destination /path/to/web_server/root

## CloudCannon hosting (and CMS)
CloudCannon offers hosting and a CMS for Jekyll applications. Here are the steps in hosting your Jekyll application on CloudCannon (http://cloudcannon.com).

Without version control:

- Create your blog locally using some local files and `jekyll serve`
- Create a CloudCannon account and create a new site
- Drag your site to the 'File Browser' within CloudCannon

With version control:

- Create a repository on Github or Bitbucket
- Create your blog locally using some local files and `jekyll serve`
- Create a CloudCannon account and create a new site
- Connect your Github or Bitbucket repository

Note that CloudCannon is not completely free. In the [free plan](cloudcannon.com/pricing/#faq) you can use CloudCannon as a graphical CMS, but you will need external hosting. In the paid plan hosting is also included.

