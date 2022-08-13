---
title: "Custom Validations"
slug: "custom-validations"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Yii2 has some built-in validators which can be used while solving programming related or general puprose validations. When you need to create a new business logic validation you need to create your own validators.

## Types of Validations
Let's initially understand basic types of custom validators:

 1. Inline Validator
 2. Standalone Validator

**Inline Validator**:
    It is the type of the validator we create inside the class which is basically a method we define just like other methods but with extra parameters which is passed in by Yii2.
    
    ....
    public function ValidateMyBusiness($attr, $params){
        // adding an error here means our validation is failed.
        if ($this->{$attr} > 1100) {
            $this->addError($attr, "Some error occured");
        }
    }
    ...
    // calling above validator is simple as below:
    public function rules(){
      return [
         ['money', 'validateMyBusiness', 'params' => ['targetAccount' => $this->account]];
      ]
    }

    # params array will be passed to our inline parameter as a second argument.


            

