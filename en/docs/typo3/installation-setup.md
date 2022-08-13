---
title: "Installation & Setup"
slug: "installation--setup"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Installation with composer
TYPO3 can be solely installed with the PHP dependency manager composer. Composer has to be available on the server, then a TYPO3 project can be started by using the base distribution.

    composer create-project typo3/cms-base-distribution .

This will pull the TYPO3 core from the git repository, download it to `vendor/typo3/cms/` and create a folder called `web/` that will be the document root of the project.

The base distribution contains nothing more than acomposer.json and a .gitignore file to get a project startet. These files could also be created by hand. The composer.json could look like this:

    {
        "repositories": [
            { "type": "composer", "url": "https://composer.typo3.org/" }
        ],
        "name": "typo3/cms-base-distribution",
        "description" : "TYPO3 CMS Base Distribution",
        "license": "GPL-2.0+",
        "require": {
            "typo3/cms": "^7.6"
        },
        "extra": {
            "typo3/cms": {
                "cms-package-dir": "{$vendor-dir}/typo3/cms",
                "web-dir": "web"
            }
        }
    }

When this file is created by hand the command `composer update` will also fetch all files needed for a TYPO3 project.

Further information can be found here:

 - [composer.typo3.org][1]
 - [usetypo3.com/typo3-and-composer][2]
 - [Composer in the TYPO3 wiki][3]


  [1]: http://composer.typo3.org
  [2]: https://usetypo3.com/typo3-and-composer.html
  [3]: https://wiki.typo3.org/Composer

