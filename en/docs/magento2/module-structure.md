---
title: "Module structure"
slug: "module-structure"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Catalog Module structure
For now I think the catalog module contains almost everything you can add to a module.  

 - **Api** - Contains the service contracts. A set of interfaces that should not be changed unless the minor version changes. Not mandatory for a custom module but nice to have for comercial extensions.  
    - **Data** - Data interfaces. Each interface must have a model that implements it (example: interface for product model)
    - **ProductRepositoryInterface.php** - interfaces for repositories (must also have an implementation)
    - ... - others as above
 - **Block** - blocks used in the layout for frontend and backend
   - **Adminhtml** - blocks used for backend
   - **Category** - frontend related blocks. Can be nested in as many folders as you like, but not mandatory
   - ...  - same as above
 - **Console** - folder containing cli commands
 - **Controller** - contains frontend and backend controllers 
   - **Adminhtml** - backend controllers
   - **Category** - frontend related controllers. Can be nested in as many folders as you like, but not mandatory
   - ... - same as above.  
 - **Cron** - code that should be executed via cron
 - **etc** - contains module configuration xml files
   - **frontend** - contains configuration files loaded only on frontend
   - **adminhtml** - contains configuration files loaded only on backend
   - **webapi_rest** - contains configuration files loaded only for the rest api
   - **webapi_soapt** - contains configuration files loaded only for the SOAP api
   - **acl.xml** - ACL definitions
   - **catalog_attributes.xml** - default attributes for catalog entities.  
   - **catalog_attributes.xsd** - validation schema for file above.  
   - **config.xml** - default values for config settings
   - **crontab.xml** - cron jobs scheduling
   - **di.xml** - dependency injection preferences. (can also reside in adminhtml, frontend, webapi_*)
   - **events.xml** - observers declaration for events (can also reside in adminhtml, frontend)
   - **indexer.xml** - settings for different indexes that need to be executed when data changes
   - **module.xml** - the module declaration file
   - **product_*** - product related settings.
   - **webapi.xml** - webapi declaration paths.
   - **widget.xml** - widgets declarations.  
 - **Helper** - different module helpers
 - **i18n** - language translation files
 - **Model** - models, simple as that. they can be nested in as many folders as you like, but it's not mandatory.  
 - **Observer** - event observer classes
 - **Plugin** - `around|before|after` plugins for different public methods.  
 - **Pricing** - pricing related classes. This is module specific. You can have as many folders as you like like this if you don't want to place them in the models folder.  
 - **Setup** - install/upgrade related files (installing upgrading schema and data)
 - **Test** - unit tests
 - **Ui** - ui components related classes.  
 - **view** - the html related part. The **V** in MVC. 
   - **adminhtml** - admin related files
      - **layout** - xml layouts for adminhtml
      - **templates** - phtml templates for adminhtml
      - **ui_compoenent** - ui components related files (declaration)
      - **web** - assets (js, images)
      - **requirejs-config.js** - configuration for require.js
   - **base** - files used for both frontend and backend.
      - can have same subfolder structure as adminhtml
   - **frontend** - frontend related files
      - can have same subfolder structure as adminhtml
 - **composer.json** - not mandatory, but nice to have if you distribute your module 
 - **registration.php** - the module registration file.
 - **Licence\*.txt, readme.md** - you know what this means. They are not mandatory

