---
title: "Create Automated Functions For Model"
slug: "create-automated-functions-for-model"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

We often need to run some code automatically during module install. This have many reasons for example configuring `Sale` module settings to meet our project requirements.

In this topic you will learn how to make automated function run on module install.

## First of all you need to create xml file for make function call
    <?xml version="1.0"?>
    <openerp>
        <data noupdate="1">
            <function model="*model_name*" name="_configure_sales"/>
        </data>
    </openerp>

This simple xml file is calls `_configure_sales` function from *model_name* model.

NOTE: this xml file should be on the top of `data` array, because Odoo is processiong xml files from top to bottom.

## Corresponding Python file
    class *model_name*(models.Model):
        _name = *model_name*

        @api.model
        def _configure_sales(self):
            # Do the configuration here

Every time when module will be installed this function will run.

Note: If you remove `noupdate` from xml, function will run on upgrading as well.

