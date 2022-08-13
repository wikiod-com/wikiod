---
title: "How to use private repositories with Composer"
slug: "how-to-use-private-repositories-with-composer"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Parameters
| Parameters | Details |
| ------ | ------ |
| repositories | Tells Composer where it can download the required packages. |
| type: vcs | Tells Composer how to treat the repository. |
| url: http://... | Tells Composer where is the repository. |

Use the `type: "vcs"` syntax to [use private repositories][1].

To manage access to the private repository while developing on a local machine, use an [`auth.json` file][2] and don't commit it in you project repository. Instead, give access to each single developer to the private repository so, using each one his/her own NOT COMMITTED `auth.json` file, they can fetch the remote repository with ` composer install` or `composer update`.

Tip: Put the `auth.json` file in the `.gitignore` file of your `git` repository.

If you are using a continuous integration system, use the [`COMPOSER_AUTH`][3] environment variable.


  [1]: https://getcomposer.org/doc/05-repositories.md#using-private-repositories
  [2]: https://getcomposer.org/doc/articles/http-basic-authentication.md
  [3]: https://getcomposer.org/doc/03-cli.md#composer-auth

## composer.json syntax
    {
        "name": "your/package",
        "license": "proprietary",
        "type": "project",
        "description": "How to load an external private Composer package.",
        ...
        "require": {
            "your/private_package": "*"
        },
        ...
        "repositories": [
            {
                "type": "vcs",
                "url": "https://example.com/Your/private-package.git"
            }
        ]
    }

