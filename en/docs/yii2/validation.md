---
title: "Validation"
slug: "validation"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Disable Validation Error Message On Focus / Key Up
By default error message appears below `textbox` in `<div class="help-block"></div>` on *keyUp* or *after pressing submit button* if any validation constraints aren't met.

Sometimes we want a message on submit only i.e. no validation at `onKeyup` event.

Let's check `yii2/widgets/ActiveForm.php` file:

    <?php
    
    namespace yii\widgets;
    
    use Yii;
    use yii\base\InvalidCallException;
    use yii\base\Widget;
    use yii\base\Model;
    use yii\helpers\ArrayHelper;
    use yii\helpers\Url;
    use yii\helpers\Html;
    use yii\helpers\Json;
    
    class ActiveForm extends Widget
    {
      public $action = '';
      public $method = 'post';
      public $options = [];
      .
      .
      .
      public $validateOnSubmit = true;
      public $validateOnChange = true;
      public $validateOnBlur = true;
      public $validateOnType = false;
      
      .
      .
      .
      
    }

There we see that `$validateOnBlur` is set to `true` by default. Changing framework files is a very bad thing to do so we need to override it when using the form:
      
       
    <?php $form = ActiveForm::begin([ 'id' => 'register-form','validateOnBlur' => false]); ?>



## Scenario in Validation
Using scenario you can perform validation on different situation

Define scenario in model class 

        class User extends \yii\db\ActiveRecord 
        {
          public static function tableName()
          {
              return 'user_master';
          }
    
        // define validation in rule() function
      public function rules()
      {
        return [
          [['email_id'],'email'],
          [['first_name',],'required','on'=>['create','update']],  // create scenario
          [['email_id',],'required','on'=> ['admin','create','update','forgotpassword']],
          [['mobile',],'required','on'=>['admin','create','update']],
        ];
      }
    }

Use Scenario in Controller 

    public function actionCreate()
    {
      $model = new User();
      $model->scenario="create";  // use create scenario, create scenario validaion applied in this model
                
      
    }
    public function actionUpdate()
    {
        $model = new User();
        $model->scenario="update";  // use update scenario, update scenario validaion applied in this model
    }


    

    

## Validate unique value from database in Yii2


## Validating Unique Value From Database : Unique Validation
Some people have issues regarding error messages not being displayed if an existing value is being entered. For example I'm not allowing a user signup with an existing email.


**View**

    <?php
    ......................    
        
        <?= $form->field($modelUser, 'email')->textInput(['class'=>'form-control','placeholder'=>'Email']) ?>
    ......................    


**Controller**


    <?php
    use yii\web\Response; // important lines
    use yii\widgets\ActiveForm; // important lines
            
    .
    .// Your code
    .
            
    public function actionSignup() 
    {
                
        $modelUser = new User();
            
        //Add This For Ajax Validation 
        if(Yii::$app->request->isAjax && $modelUser->load(Yii::$app->request->post())){
            Yii::$app->response->format = Response::FORMAT_JSON;
            return ActiveForm::validate($modelUser);
        } 
        if ($modelUser->load(Yii::$app->request->post()) && $modelUser->save()) {
            return $this->redirect(['someplace nice']);
        }
        return $this->render('update', [
            'modelUser' => $modelUser,
        ]);            
    }
    
    
**Model**

    public function rules()
    {
        return [
            ..............
            ['email', 'unique', 'message'=>'Email already exist. Please try another one.'],
            ...........
        ]
    }

## Validate array
Since Yii2 version 2.0.4 there is the EachValidator used to validate each item in an array.

    [
        // ... other rules
        ['userIDs', 'each', 'rule' => ['integer']],
    ]

The `['integer']` part can be every other validator object that Yii2 offers and can hold the specific arguments for the validator. Like: `['integer', 'min' => 1337]`.           If the userIDs doesn't contain an array the rule validation will fail.


If you just want to see if an attribute contains an array without validating the contents you can write your own validator.

    [
        ['myAttr', function($attribute, $params) {
            if (!is_array($this->$attribute)) {
                $this->addError($attribute, "$attribute isn't an array!");
            }
        }]
    ]

