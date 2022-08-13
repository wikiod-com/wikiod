---
title: "Components"
slug: "components"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Creating and using application components
Steps to Create component:

- Create a folder named `components` in your project root folder
- Create your component inside components folder e.g.: `MyComponent.php`

```
namespace app\components;
    
use Yii;
use yii\base\Component;
use yii\base\InvalidConfigException;
    
    
class MyComponent extends Component
{
    public function demo()
    {
        return "welcome";
    }
}
```

- Register your component inside the `config/web.php` file

```
components' => [
    'mycomponent' => [
        'class' => 'app\components\MyComponent',
    ],
]
```

Now you can use your component method:


```
namespace app\controllers;

use Yii;
     
class DemoController extends \yii\web\Controller
{
    public function actionTest()
    {
        echo Yii::$app->mycomponent->demo();
    }
}
```


## Dropdown List using component function
Create function in MyComponent.php

        namespace app\components;
    
        use Yii;
        use yii\base\Component;
        use yii\base\InvalidConfigException;
        use yii\helpers\Url;
        use yii\helpers\ArrayHelper;
        
        use app\models\User;
        
            class MyComponent extends Component
            {
            
              // Function return list of  id & user Names,used for dropdownlist
              public function getUserID()
              {
                $code = User::find()->select('id,name')
                ->where(['is_deleted'=>'n'])
                ->all();
            
                $result = ArrayHelper::map($code, 'id', 'name');
                if($result)
                    return $result;
                else
                    return ["null"=>"No User"];
              }
            }

-> Register Component in web.php

    components' => [
            'mycomponent' => [
                 'class' => 'app\components\MyComponent',
                ],
             ]

-> use it in your view

    <?= $form->field($model, 'user_id')->dropDownList(Yii::$app->mycomponent->getUserID())?>



