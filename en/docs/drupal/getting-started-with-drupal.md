---
title: "Getting started with drupal"
slug: "getting-started-with-drupal"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Drupal Concepts
**Versions**

    Release Date

| Version | Release Date |
| ------ | ------ |
|[8.2.4][1]| December 07, 2016|
|[7.53][2]| December 07, 2016|
|[6.38][3] (unsupported)| February 24, 2016|
|[5.23][4] (unsupported)| August 11, 2010|

**Entity types**

In earlier versions of Drupal, the field system was only used on content types. Now, thanks to the Entity API, we can add fields to other things, like comments. Fieldable entities make Drupal eminently flexible. An entity type is a useful abstraction to group together fields. Below are the Entity types in Drupal core:

- Nodes (content)
- Comments
- Files
- Taxonomy terms
- Taxonomy vocabularies
- Users

You can also build new kinds of entity types where the options above don't suit your needs.

**Bundles**

Bundles are an implementation of an entity type to which fields can be attached. You can consider bundles as subtypes of an entity type. With content nodes (an entity type), for example, you can generate bundles (subtypes) like articles, blog posts, or products. Not all entity types have bundles, however. For example, users do not have separate bundles (subtypes). For the entity types that do allow bundles, you can create as many bundles (subtypes) as you want. Then, using the Field system, you can add different fields to each bundle. Examples include a file download field on Basic Pages and a subtitle field on Articles.

**Fields**

A field is a reusable piece of content. In technical terms, each field is a primitive data type, with custom validators and widgets for editing and formatters for display. You can read further for a developer's guide to using the [Drupal 7 Fields API][5].

What's important to know as it relates to Entities is that Fields can be added to any of the bundles (or entity types) to help organize their data.

Say, for example, you create a content type with an unstructured text field and use HTML to structure parts of it, like a summary section, or prices. That would make it more difficult, then, to control how these were displayed, or to make connections between different types of related content.

This is where using fields is essential. You could create a summary field of type Long Text as well as price fields of type Decimal.

**Entity**

An entity would be one instance of a particular entity type such as a comment, taxonomy term or user profile or of a bundle such as a blog post, article or product.

You can use [entity_load][6] to load any entity. Note, however, that the core does not provide a save or delete function, but thanks to [Entity API][7] module the missing pieces are added (entity_create(), entity_save(), entity_delete(), entity_view() and entity_access()).

**Putting this in Object-Oriented Design/Programming terms...**

If you come from an OOD/P background and are trying to better understand what these key concepts are, the following suggested mapping might help (albeit not strictly true from a purist’s perspective) :-

- An ***entity type*** is a ***base class***
- A ***bundle*** is an ***extended class***
- A ***field*** is a ***class member***, ***property***, ***variable*** or ***field instance*** (depending on your naming preference)
- An ***entity*** is an ***object*** or ***instance*** of a ***base*** or ***extended class***

All these four OOD/P concepts are special in that they are serialisable (stored - e.g. to a database or file). Serialisation takes place via the Entity API.


  [1]: https://www.drupal.org/project/drupal/releases?api_version%5B%5D=7234
  [2]: https://www.drupal.org/project/drupal/releases?api_version%5B%5D=103
  [3]: https://www.drupal.org/node/3060/release?api_version%5B%5D=87&=Apply
  [4]: https://www.drupal.org/node/3060/release?api_version%5B%5D=78&=Apply
  [5]: http://drupal.org/node/443536
  [6]: http://api.drupal.org/api/drupal/includes--common.inc/function/entity_load/7
  [7]: http://drupal.org/project/entity

## Installing Drupal with Drush
    drush dl drupal --drupal-project-rename=example
    cd example
    drush site-install standard --db-url='mysql://[db_user]:[db_pass]@localhost/[db_name]' --site-name=Example

## Installation of Drupal 8 with Drupal Console
[Drupal Console][1]

The new CLI for Drupal. A tool to generate boilerplate code, interact with and debug Drupal.

First, we need to install Drupal Console.

Drupal Console is needed not only for this time, but for future installations.

    # Run this in your terminal to get the latest project version:
    curl https://drupalconsole.com/installer -L -o drupal.phar
    
    # Or if you don't have curl:
    php -r "readfile('https://drupalconsole.com/installer');" > drupal.phar
    
    # Accessing from anywhere on your system:
    mv drupal.phar /usr/local/bin/drupal
    
    # Apply executable permissions on the downloaded file:
    chmod +x /usr/local/bin/drupal
    
    # Copy configuration files to user home directory:
    drupal init --override
    
    # Check and validate system requirements
    drupal check

You may call `drupal list` to see all available commands.

On the next step we'll download Drupal source code

    drupal site:new

Console will prompt you to choose a folder to download Drupal. And on the next step you'll be asked to choose version of Drupal to download. I recommend to select the last one.

So, when Drupal is downloaded you need to install it.

    drupal site:install

After few simple steps your Drupal site will be ready.

With this methodology, a Drupal fresh install take us between 5 to 7 minutes all from the command-line.

  [1]: https://drupalconsole.com/

