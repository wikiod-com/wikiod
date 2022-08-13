---
title: "Directives provided by mod-rewrite in Apache 2.4"
slug: "directives-provided-by-mod-rewrite-in-apache-24"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
 - RewriteBase URL-path
 - RewriteCond TestString CondPattern
 - RewriteEngine on|off
 - RewriteMap MapName MapType:MapSource
 - RewriteOptions Options
 - RewriteRule Pattern Substitution [flags]

## List of directives available in Apache 2.4
Apache 2.4 provides the following 6 directives via the `mod_rewrite` module:

 1. RewriteBase
 1. RewriteCond
 1. RewriteEngine
 1. RewriteMap
 1. RewriteOptions
 1. RewriteRule

The following directives, available previously in Apache 2.2 have been removed:

 1. RewriteLock
 1. RewriteLog
 1. RewriteLogLevel

All the directives (with the exception of `RewriteMap`) defined by `mod_rewrite` can be allowed to override on a per-directory `.htaccess` through the `AllowOverride FileInfo`.

| Directive | Context | Description |
| ---- | ---- | ---- |
| RewriteBase | directory, .htaccess | Sets base URL for per directory rewrite |
| RewriteCond | Everywhere | Defines conditions under which the rewrite action will occur |
| RewriteEngine | Everywhere | Sets status of rewrite engine |
| RewriteMap | server config, virtual host | Defines a key lookup function |
| RewriteOptions | Everywhere | Sets special _options_ for rewrite engine |
| RewriteRule | Everywhere | Defines specific rules for rewrite engine |

The context `Everwhere` means that the directive can be defined in any of the following four locations:

 1. server config
 1. virtual host config
 1. directory context
 1. .htaccess file

The `RewriteLog` and `RewriteLogLevel` directives have been merged with the global `LogLevel` directive and would be used as:

    LogLevel rewrite:<level>

where `<level>` is a value from `trace8` (least significant) to `emerg` (most significant). This list is available [here][1].


  [1]: https://httpd.apache.org/docs/2.4/mod/core.html#loglevel

## RewriteBase and RewriteEngine
| Directive | Default | Context | Description |
| ---- | ---- | ---- | ---- |
| RewriteBase | None | Directory, .htaccess | Sets base URL for per directory rewrite |
| RewriteEngine | off | everywhere | Enable or disable runtime rewrite engine |

# `RewriteBase`

The directive specifies URL prefix to be used for substituting relative paths.

# `RewriteEngine`

The directive, if set to `off`, will perform no runtime rewrite processing. These rules are not inherited by the virtual hosts (from server config), and will have to be defined individually.

## RewriteMap
The directive defines a function which'll lookup a key in the defined map and substitutes the lookup with its replacement from the map.

The mapping function is defined with the `RewriteMap` directive itself as follows:

    RewriteMap MAPNAME Type:Source

and can be referenced in any of the `RewriteCond` or `RewriteRule` directives to act as a substitution guide as follows:

    ${ MAPNAME : KEY | DEFAULT }

The following are valid values for `Type` in the map definition:

 1. `int` - allows `toupper`, `tolower`, `escape` and `unescape` only
 1. `txt` - searches a text file
 1. `dbd` - looks up in a database using SQL `SELECT` statement
 1. `rnd` - random lookups from text file
 1. `dbm` - similar to `txt`, except that the `httxt2dbm` needs to convert the data to hashes
 1. `fastdbd` - looks up in a database using SQL `SELECT` statement with caching

