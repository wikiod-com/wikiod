---
title: "Instantiating models from another datasource"
slug: "instantiating-models-from-another-datasource"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

There will come a time where your CakePHP application will need to query more than one database. The method for requesting Models from non-default databases is not present in the official documentation.

## On the fly database changes for modal
For multiple databases, you have the database.php file where you can set as many databases as you need.

If you want to "switch" a database for a specific model on the fly, use the [setDataSource()][1] method.



For example, if you have two databases, you can define them in the database.php file as "default" and "sandbox", as an example.

Then, in your code:

$this->MyModal->setDataSource('sandbox');

The sandbox is the name of the configuration, and the actual name of the database is written only once in the database.php file.


  [1]: http://api.cakephp.org/2.1/class-Model.html#_setDataSource

## Instantiating uses App::uses
    App::uses('YourModel', 'Model');
    $model_1 = new YourModel(array('ds' => 'default'));
    $model_2 = new YourModel(array('ds' => 'database2'));

