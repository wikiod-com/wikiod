---
title: "The Config System"
slug: "the-config-system"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

# What is the config system

SilverStripe uses a global config system to store settings for classes and the application. These config variables can be used to define the structure of Models, security settings on Controllers or API keys for third party services.

# How it works

`Config` values are populated by the `SS_ConfigStaticManifest` during a `dev/build` and cache flush (appending `?flush` to any URL`) or on first ever run of the application code.

The `SS_ConfigStaticManifest` will scan all PHP classes and YAML config files for any config values and build a cache of these values.

> When making change to `Config` settings via YAML or `private static` variables, you'll need to flush the cache for these changes to take effect.

## Setting config values
`Config` values can be set in three ways:

 1. Via `private static` variables on any class within a SilverStripe project
 2. Via yaml config files (stored in module-folder/_config/[file].yml)
 3. Via PHP at run time (`Config::inst()->update('Director', 'environment_type', 'dev')`

> Generally it's best to set config values via the first 2 methods as these are statically cached when flushing the cache.

# Setting with private statics


```php

class MyDataObject extends DataObject {

    private static $db = array(
        'Title' => 'Varchar',
    );

}
```

> All `private static` class variables in a SilverStripe project's code (including modules, but not packages in the `vendor/` directory) will be loaded into the `Config`.

# Setting with YAML

You can add this to `mysite/_config/config.yml` (or any other YAML file in that path).

```yml
Director:
  environment_type: dev
```

> Using YAML files is a great way to override default `Config` values for core classes or modules

# Setting at runtime

This would typically be done in `mysite/_config.php`

```php
Config::inst()->update('Director', 'environment_type', 'dev');
```

> Updating the `Config` in PHP should be avoided where possible as it's slower than using the cached values

