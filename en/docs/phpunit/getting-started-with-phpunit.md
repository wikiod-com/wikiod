---
title: "Getting Started with PHPUnit"
slug: "getting-started-with-phpunit"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Installation on Linux or MacOSX
# Global installation using the PHP Archive

    wget https://phar.phpunit.de/phpunit.phar        # download the archive file
    chmod +x phpunit.phar                            # make it executable
    sudo mv phpunit.phar /usr/local/bin/phpunit      # move it to /usr/local/bin
    phpunit --version                                # show installed version number


# Global installation using Composer

    # If you have composer installed system wide
    composer global require phpunit/phpunit  # set PHPUnit as a global dependency
    phpunit --version                        # show installed version number

    # If you have the .phar file of composer
    php composer.phar global require phpunit/phpunit  # set PHPUnit as a global dependency
    phpunit --version                                 # show installed version number


# Local installation using Composer

    # If you have composer installed system wide
    composer require phpunit/phpunit  # set PHPUnit as a local dependency
    ./vendor/bin/phpunit --version    # show installed version number

    # If you have the .phar file of composer
    php composer.phar require phpunit/phpunit  # set PHPUnit as a local dependency
    ./vendor/bin/phpunit --version             # show installed version number


