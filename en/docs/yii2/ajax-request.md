---
title: "Ajax Request"
slug: "ajax-request"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Submitting Ajax form
##### View file:

   
    <?php 
    use yii;
    use yii\bootstrap\ActiveForm;
    use yii\helpers\Html;
    ?>
    
    <?php 
    $form = ActiveForm::begin([
        'action' => ['comments/ajax-comment'],
        'options' => [
            'class' => 'comment-form'
        ]
    ]); 
    ?>
        <?= $form->field($model, 'comment'); ?>

        <?= Html::submitButton("Submit", ['class' => "btn"]); ?>
    
    <?php ActiveForm::end(); ?>


##### Javascript:

     jQuery(document).ready(function($) {
           $(".comment-form").submit(function(event) {
                event.preventDefault(); // stopping submitting
                var data = $(this).serializeArray();
                var url = $(this).attr('action');
                $.ajax({
                    url: url,
                    type: 'post',
                    dataType: 'json',
                    data: data
                })
                .done(function(response) {
                    if (response.data.success == true) {
                        alert("Wow you commented");
                    }
                })
                .fail(function() {
                    console.log("error");
                });
            
            });
        });
    

   ##### Controller Action:
    
    public function actionAjaxComment()
    {
        $model = new Comments();
        if (Yii::$app->request->isAjax) {
            Yii::$app->response->format = \yii\web\Response::FORMAT_JSON;

            if ($model->load(Yii::$app->requset->post()) && $model->save()) {
                return [
                    'data' => [
                        'success' => true,
                        'model' => $model,
                        'message' => 'Model has been saved.',
                    ],
                    'code' => 0,
                ];
            } else {
                return [
                    'data' => [
                        'success' => false,
                        'model' => null,
                        'message' => 'An error occured.',
                    ],
                    'code' => 1, // Some semantic codes that you know them for yourself
                ];
            }
        }
    }

## Render Ajax view
`Controller::renderAjax()` method can be used to respond to an Ajax request. This method is similar to renderPartial() except that it will inject into the rendering result with JS/CSS scripts and files which are registered with the view

Assume we have login form in a view file:

    <?php
    use yii\helpers\Html;
    use yii\bootstrap\ActiveForm;

    \yii\bootstrap\BootstrapAsset::register($this);
    
    <div class="site-login">
    
        <?php $form = ActiveForm::begin(); ?>
    
            <?= $form->field($model, 'username')->textInput() ?>
    
            <?= $form->field($model, 'password')->passwordInput() ?>
    
            <?= Html::submitButton('Login',['class' => 'btn btn-primary btn-block']) ?>
    
        <?php ActiveForm::end(); ?>
    </div>

When we use `renderPartial()` in a controller action:

    public function actionLogin()
    {
        $model = new LoginForm();
        if ($model->load(Yii::$app->request->post()) && $model->login()) {
            return $this->goBack();
        }
        return $this->renderPartial('login', [
            'model' => $model,
        ]);
    }

Action output:

    <div class="site-login">
        <form id="w0" action="/site/login" method="post" role="form">
            <div class="form-group field-loginform-username required">
                <label class="control-label" for="loginform-username">Имя пользователя</label>
                <input type="text" id="loginform-username" class="form-control" name="LoginForm[username]">
            </div>
            <div class="form-group field-loginform-password required">
                <label class="control-label" for="loginform-password">Пароль</label>
                <input type="password" id="loginform-password" class="form-control" name="LoginForm[password]">
            </div>
            <button type="submit" class="btn btn-primary btn-block">Login</button>
        </form>
    </div>

When we use `renderAjax()` in a controller action:

        ...
        return $this->renderAjax('login', [
            'model' => $model,
        ]);
        ...

Action output (JS,CSS injected):

    <link href="/assets/f1759119/css/bootstrap.css" rel="stylesheet">
    <div class="site-login">
        <form id="w0" action="/site/login" method="post" role="form">
            <div class="form-group field-loginform-username required">
                <label class="control-label" for="loginform-username">Имя пользователя</label>
                <input type="text" id="loginform-username" class="form-control" name="LoginForm[username]">
            </div>
            <div class="form-group field-loginform-password required">
                <label class="control-label" for="loginform-password">Пароль</label>
                <input type="password" id="loginform-password" class="form-control" name="LoginForm[password]">
            </div>
            <button type="submit" class="btn btn-primary btn-block">Login</button>
        </form>
    </div>
    <script src="/assets/13aa7b5d/jquery.js"></script>
    <script src="/assets/302a2946/yii.js"></script>
    <script src="/assets/302a2946/yii.validation.js"></script>
    <script src="/assets/302a2946/yii.activeForm.js"></script>

If we want to exclude some assets from view (to prevent dublicates):

    ...
    Yii::$app->assetManager->bundles = [
        'yii\bootstrap\BootstrapAsset' => false,
    ];
    return $this->renderAjax('login', [
        'model' => $model,
    ]);
    ...

Action output (no bootstrap.css):

    <div class="site-login">
        <form id="w0" action="/site/login" method="post" role="form">
            <div class="form-group field-loginform-username required">
                <label class="control-label" for="loginform-username">Имя пользователя</label>
                <input type="text" id="loginform-username" class="form-control" name="LoginForm[username]">
            </div>
            <div class="form-group field-loginform-password required">
                <label class="control-label" for="loginform-password">Пароль</label>
                <input type="password" id="loginform-password" class="form-control" name="LoginForm[password]">
            </div>
            <button type="submit" class="btn btn-primary btn-block">Login</button>
        </form>
    </div>
    <script src="/assets/13aa7b5d/jquery.js"></script>
    <script src="/assets/302a2946/yii.js"></script>
    <script src="/assets/302a2946/yii.validation.js"></script>
    <script src="/assets/302a2946/yii.activeForm.js"></script>

