---
title: "Testing"
slug: "testing"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## How to mock ActiveRecord
If you want to mock AR that doesn't try to connect to database you can do it in the following way (if using PHPUnit):


```
$post = $this->getMockBuilder('\app\model\Post')
    ->setMethods(['save', 'attributes'])
    ->getMock();
$post->method('save')->willReturn(true);
$post->method('attributes')->willReturn([
    'id',
    'status',
    'title',
    'description',
    'text'
]);
```

The catch is that we need to override attributes() method since ActiveRecord by default is getting attributes list from
database schema which we're trying to avoid.

## Set up testing environment
Install Codeception:

    composer global status
    composer global require "codeception/codeception=~2.0.0" "codeception/specify=*" "codeception/verify=*"

Install Faker:

    cd /var/www/yii                                    // Path to your application
    composer require --dev yiisoft/yii2-faker:*

Create a database, which will be used only for the tests. You can duplicate the existing database or apply migrations:

    cd tests
    codeception/bin/yii migrate

Adjust the `components['db']` configuration in `tests/codeception/config/config-local.php`.

Add the directory `/var/www/yii/vendor/bin` to your path.

Review all configuration and `.yml` files.

Start the webserver, e.g.:

    php -S localhost:8080

Run the tests:

    codecept run

More information:

 - http://www.yiiframework.com/doc-2.0/guide-test-environment-setup.html
 - http://codeception.com/install
 - https://github.com/yiisoft/yii2-app-basic/tree/master/tests
 - https://github.com/yiisoft/yii2-app-advanced/tree/master/tests

**Note:**
These instructions are valid for the Yii2 version 2.0.9. In the version 2.0.10 will be according to Sam Dark the testing part re-factored (and the instructions have to be updated). The version 2.0.10 should be released on 11. September 2016: https://github.com/yiisoft/yii2/milestones

