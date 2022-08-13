---
title: "Yii2 ActiveForm"
slug: "yii2-activeform"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Form Fields In Yii2
**Showing Basic Example of the View Page in Yii2 For New Learners**               

These are basic classes you must to add to create form using yii2 ActiveForm

    <?php
    
    Use yii\helpers\Html;
    Use yii\widgets\ActiveForm;

The Below line will start the form tag for our form below showing example shows that how to specify id for the form and how to apply any classes for the form..

    $form =ActiveForm::begin([   'id'=> 'login-form',   'options'=> ['class' => 'form-horizontal'],]) ?>

Here $model Specify which database table field we want to bind with form that model object stored here in this variable which has been passed from the relevant controller.

       <?= $form->field($model, 'username') ?>
       <?= $form->field($model, 'password')->passwordInput() ?>

'username' and 'password' is the name of the table field with which our value will be bound.

   

Here in below code we are putting submit button for form submission and applying 'Login' as Button Text and basic css classes to it.

     <div class="form-group">
           <div class="col-lg-offset-1 col-lg-11">
               <?= Html::submitButton('Login', ['class' => 'btn btn-primary']) ?>
           </div>
       </div>

Here in below code we are Closing out form 

    <?php ActiveForm::end() ?>

**Create Password Field :**

    <?= $form->field($model, 'password')->passwordInput() ?>

**Create TextField :**

    <?= $form->field($model, 'username') ?>

**Create Hidden Form Field :**

    echo $form->field($model, 'hidden1')->hiddenInput()->label(false);

**Create Dropdown :**

    <?php echo $form->field($model, 'name')
    ->dropdownList(
    Stud::find()->select(['name'])
    ->indexBy('name')->column(),
    ['prompt'=>'Select no']); ?>

**Dropdown list with Id And Name**

    <?= $form->field($model, 'name')->dropDownList(
                ArrayHelper::map(Stud::find()->all(), 'no', 'name'),['prompt' => 'Select Car Name']
     ) ?>

**Create FileUploader :**

    echo $form->field($model, 'imagepath')->fileInput();

**Adding A Placeholder and Customized Label**

     <?= $form->field($model, 'username')->textInput()->hint('Please enter your  name')->label('Name') ?>










## ActiveForm Validations
You can enable/disable ajax and client validations in active form. 

    $form = ActiveForm::begin([
        'id' => 'signup-form',
        'enableClientValidation' => true,
        'enableAjaxValidation' => true,
        'validationUrl' => Url::to('signup'),
    ]);

1. `enableClientValidation` is by default enabled in ActiveForm. If you don't need client validation in form we can disable by assigning as false.
2. `enableAjaxValidation` is by default disabled in ActiveForm. If you want to enable it we have to add manually in  ActiveForm like above.
3. `validationUrl` - if you want to keep all the validation coding in separate controller action for this form we can configure the activeform using `validationUrl`. If we didn't set this, it will take the form's action value.

The above two arguments will affect for whole form. If you want to check ajax validation only for particular field in the form you can add `enableAjaxValidation` for that particular field. It will work only for that field not whole form.

For example in registration form you want to check the username already exist validation on time of user enter in the form. you can use this `enableAjaxValidation` argument for that field.

    echo $form->field($model, 'username', ['enableAjaxValidation' => true]);

