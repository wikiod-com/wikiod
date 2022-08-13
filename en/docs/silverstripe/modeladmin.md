---
title: "ModelAdmin"
slug: "modeladmin"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Control the DataObject name displayed in the UI
    class MyDataObject extends DataObject {
    
        private static $singular_name = 'My Object';
        private static $plural_name = 'My Objects';
    
        ...
    }


## DataObjects can be sorted by default
    class SortDataObject extends DataObject {
    
        private static $db = array(
            'Name' => 'Varchar',
            'SortOrder' => 'Int'
        );
        
        private static $default_sort = 'SortOrder DESC';
    }



## Control columns displayed for the DataObject
    class MyDataObject extends DataObject {
    
        private static $db = array(
            'Name' => 'Varchar'
        );
    
        private static $has_one = array(
            'OtherDataObject' => 'OtherDataObject'
        );
        
        private static $summary_fields = array(
            'Name',
            'OtherDataObject.Name'
        );
        
        private static $field_labels = array(
            'OtherDataObject.Name' => 'Other Data Object'
        );
    }

ModelAdmin uses the `summary_fields` to generate the columns that it displays. To specify the name of the column, use `field_labels` as shown.

## Simple Example
Given a simple `DataObject` like this:

    class MyDataObject extends DataObject {
        private static $db = array(
            'Name' => 'Varchar(255)'
        );  
    }

To provide full Create-Read-Update-Delete for the objects then this is the `ModelAdmin` code required:

    class MyModelAdmin extends ModelAdmin {
        private static $mangaged_models = array(
            'MyDataObject'
        );  
        private static $url_segment = 'my-model-admin';
        private static $menu_title = 'My Model Admin';
        private static $menu_icon = 'mysite/images/treeicons/my-model-admin.png';
        private static $menu_priority = 9;
    }

## Using searchable_fields to control the filters for that Object in ModelAdmin
    class MyDataObject extends DataObject {
    
        private static $db = array(
            'Name' => 'Varchar'
        );
    
        private static $has_one = array(
            'OtherDataObject' => 'OtherDataObject'
        );
        
        private static $summary_fields = array(
            'Name',
            'OtherDataObject.Name'
        );
        
        private static $searchable_fields = array(
            'Name',
            'OtherDataObjectID' => array(
                'title' => 'Other Data Object'
            )
        );
        
    }

Note the `OtherDataObjectID` which converts a text field into a drop down of the relating object to filter with.

## Remove scaffolded GridField for relationships
    class MyDataObject extends DataObject {
    
        ...
    
        private static $has_many = array(
            'OtherDataObjects' => 'OtherDataObject'
        );
    
        function getCMSFields() {
            $fields = parent::getCMSFields();
    
            if ($gridField = $fields->dataFieldByName('OtherDataObjects')) {
                $gridField->getConfig()
                    ->removeComponentsByType('GridFieldExportButton');
            }
    
            return $fields;
        }
    }



## To remove the export button from ModelAdmin
    class MyAdmin extends ModelAdmin {
    
        ...
    
        function getEditForm($id = null, $fields = null) {
            $form = parent::getEditForm($id, $fields);
    
            if ($this->modelClass == 'MyDataObjectName') {
                $form->Fields()
                    ->fieldByName($this->sanitiseClassName($this->modelClass))
                    ->getConfig()
                    ->removeComponentsByType('GridFieldExportButton');
            }
            return $form;
        }
    }

