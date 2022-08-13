---
title: "Importing"
slug: "importing"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

More information can be found at http://import.jekyllrb.com/

## Introduction
If you’re switching to Jekyll from another blogging system, Jekyll’s importers can help you with the move. Most methods listed on this page require read access to the database from your old system to generate posts for Jekyll. Each method generates `.markdown` posts in the `_posts` directory based on the entries in the foreign system.

## Installation
Because the importers have many of their own dependencies, they are made available via a separate gem called jekyll-import. To use them, all you need to do is install the gem, and they will become available as part of Jekyll’s standard command line interface.

`$ gem install jekyll-import`

## Usage
You should now be all set to run the importers with the following incantation:
```
$ ruby -rubygems -e 'require "jekyll-import";
    JekyllImport::Importers::MyImporter.run({
      # options for this importer
    })'
```
Where `MyImporter` is the name of the specific importer.

