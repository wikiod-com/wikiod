---
title: "Collections"
slug: "collections"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Configuring a New Collection
To create an albums collection, add the following to your `config.yml` file:

     collections:
    - albums

Create a corresponding folder at the root of your Jekyll install, named exactly what you put in your `config.yml` file with an additional prepended underscore; in our example, `<source>/_albums`. 

Adding documents to this folder will add items to your collection. Any variables included in a file's YAML front matter is read in as data attributes, and everything after it is included in the item's content attribute. If no YAML front matter is provided, Jekyll will not generate the file in your collection.

Collection metadata can be configured in `config.yml`:

    collections:
      albums:
        type: media

In this example, `type: media` could be any arbitrary key-value pair.

Defaults for items within a collection can also be set within `config.yml`.

    defaults:
      - scope:
          path: ""
          type: albums
        values:
          publisher: Me Publishers Inc

Given this default, any item within the `albums` collection that does not explicitly set `publisher` within its front matter will have its `publisher` variable set to `Me Publishers Inc` at build time.

[Official Jekyll Collections Docs][1]

  [1]: https://jekyllrb.com/docs/collections/

## Accessing A Specific Collection Item
[As of Jekyll 3.2](https://jekyllrb.com/news/2016/07/26/jekyll-3-2-0-released/), you can use the filter `where_exp` to filter a collection by any of its properties. 

Say you have the following collection item in an "albums" collection:

    ---
    title: My Amazing Album
    ---
    ...

You can combine the `where_exp` and `first` filters to grab just that one item:

    {% assign album = site.albums 
        | where_exp:"album", "album.title == 'My Amazing Album'" 
        | first %}

The `first` filter is necessary because `where_exp` returns an array of matched items.

You can then use it any way you'd like:

    <h1>{{ album.title }}</h1>

## Looping Through All Items in a Collection
Given an 'albums' collection, you can loop through and output each item:

    {% for album in site.albums %}
        {{ album.content }}
    {% endfor %}

Any custom front matter variables are also available within the loop.

    {% for album in site.albums %}
        {{ album.title }}
        {{ album.content }}
    {% endfor %} 

## Adding an Item to a Collection
Given an 'albums' collection, an item can be added by creating a file in the `<source>/_albums` directory. Note that files not including frontmatter will be ignored.

For instance, adding a file called `my_album.md` to the `_albums` directory would add it to the collection:

    ---
    title: "My Album"
    ---
    ...

Everything after the second set of three dashes is included in `item.content`. Both an item's content and its front matter variables can be accessed like so:

    {% for album in site.albums %}
        {{ album.title }}
        {{ album.content }}
    {% endfor %}

If you don't wish to include any front matter variables in a collection item, two sets of three dashes are sufficient front matter to have an item included:

    ---
    ---
    ...

