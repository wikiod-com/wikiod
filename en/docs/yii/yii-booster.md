---
title: "Yii Booster"
slug: "yii-booster"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Installation
First of all download Yii Booster latest end user bundle from [here][1].

Download it, unpack its contents to some directory inside your web application. Its recomended to unpack it to the extensions directory. Rename the folder from yiibooster-<version_number> to just yiibooster for convenience.

Then we need to configure it. Add below line before return array

    before return array
Inside components array add: 
    
    'bootstrap' => array(
        'class' => 'booster.components.Booster',
    ),

Then preload the yii booster by adding below snippet in the config preload section

    'preload' => array(
        ... probably other preloaded components ...
        'bootstrap'
    ),


  [1]: https://sourceforge.net/projects/yiibooster/files/latest/download?source=files

