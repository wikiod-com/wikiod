---
title: "Model"
slug: "model"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Load model
You can load Magento model using the following code:
Mage::getModel('modulename/modelname')

Example:
Mage::getModel('catalog/product')
This will load Mage_Catalog_Model_product

## Create an empty model
To create a new model in your module Add a folder `Model` in your  module root folder and create a file Modelname.php in this folder.
for example Rick/Demo/Model/Modelname.php

The class name of your model does matter call it like this:

    <?php
    class Rick_Demo_Model_Modelname {
    
    }

make sure your model is defined in your `config.xml` in the `etc` folder of your module

Here an example:
<?xml version="1.0" encoding="UTF-8"?>
<config>
    <modules>
        <Rick_Demo>
            <version>2.0.4</version>
        </Rick_Demo>
    </modules>
    <global>
        <models>
            <customemodelname>
                <class>Rick_Demo_Model</class>
            </customemodelname>
        </models>
    </global>
</config>

To load your module use the following code:

    Mage::getModel('customemodelname/modelname')

