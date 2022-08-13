---
title: "Pjax"
slug: "pjax"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Step 1 Add Structure
In views\site\form-submission.php

    <?php Pjax::begin(['id'=>'id-pjax']); ?>
    <?= Html::beginForm(['site/form-submission'], 'post', ['data-pjax' => '', 'class' => 'form-inline']); ?>
        <?= Html::input('text', 'string', Yii::$app->request->post('string'), ['class' => 'form-control']) ?>
        <?= Html::submitButton('Hash String', ['class' => 'btn btn-lg btn-primary', 'name' => 'hash-button']) ?>
    <?= Html::endForm() ?>
    <h3><?= $stringHash ?></h3>
    <?php Pjax::end(); ?>



## how to use pjax
Add this line at the beginning of your view.

    <?php
    use yii\widgets\Pjax;
    ?>

Add the following two lines around the content that needs partial updating.

    <?php Pjax::begin(['id'=>'id-pjax']); ?>
    Content that needs to be updated
    <?php Pjax::end(); ?>

## reload pjax

    $.pjax.reload({container: '#id-pjax'});

## use timeout argument in pjax

    <?php Pjax::begin(['id'=>'id-pjax', 'timeout' => false]); ?>

you can specify an integer value for the timeout argument, which would be the number of  milliseconds to wait (its default value is 1000). If the execution time in the server is greater than this timeout value, a full page load will be triggered. 

By default pjax will submit the form using GET method. You can change the form submission method to POST like in the following example

    <?php Pjax::begin(['id'=>'id-pjax', 'timeout' => false, 'clientOptions' => ['method' => 'POST']]); ?>

## Step 2 Server Side Code 

    public function actionFormSubmission()
    {
        $security = new Security();
        $string = Yii::$app->request->post('string');
        $stringHash = '';
        if (!is_null($string)) {
            $stringHash = $security->generatePasswordHash($string);
        }
        return $this->render('form-submission', [
            'stringHash' => $stringHash,
        ]);
    }

## Pjax advanced usage
Yii Framework 2.0 ships with built-in support for [Pjax][1], a JavaScript library that reduces page load times. It accomplishes this by only updating the part of the page that has changed through Ajax, which can translate into substantial savings if you have many other assets on your pages. A few of our projects use this functionality and we wanted to share some lessons learned.

**Problem**: Page 1 is a simple static page that contains few elements. Page 2 includes an ActiveForm as well as other widgets. The ActiveForm JavaScript resources need to be loaded in order for the inline JavaScript to run, but since Page 1 did not include those assets, Page 2 ran into a JavaScript error when trying to execute the activeform line: ‘Uncaught TypeError: undefined is not a function’.

**Solution**: Include ActiveForm assets in a shared asset bundle that will be loaded across all pages, ensuring that any entry page will allow the correct scripts to be available.

    class AppAsset extends AssetBundle
    {
        ...
        public $depends = [
            'yii\widgets\ActiveFormAsset',
            'yii\validators\ValidationAsset',
        ];
        ...
    }

**Problem**: In the same example above, Page 1 includes a few widgets (NavBar, etc.). Page 2 includes the same widgets plus a few more (ActiveForm, etc.). When loading the page via Pjax, some custom inline JavaScript was running, but the inline script placed by the ActiveForm widget didn’t seem to work, as the validation code was not working. In debug, we found that the ActiveForm init function was running, but the ‘this’ variable didn’t seem to correspond to the ActiveForm. It actually corresponded to the NavBar div. Investigating the div IDs, we saw that the ActiveForm was expecting to have the ID of #w1, but the NavBar was already assigned that ID on the Page 1 since that was the first widget encountered on that page.

**Solution**: Do not rely on Yii to auto-generate the widget IDs for you. Instead, always pass in an ID when creating the widget to maintain control of those IDs.

**Problem**: Pjax request was getting canceled exactly 1,000 ms after the request was initiated.

**Solution**: Increase the Pjax timeout setting. It defaults to 1 second, which should be acceptable for production sites. However, in development, while using xdebug, our page load times are regularly over this limit.

**Problem**: Web application implements the [Post-Redirect-Get (PRG)][2] pattern. Pjax reloads entire page instead of just the redirection.

**Solution**: This is intended behavior of Pjax. The redirect doesn’t serve its purpose when using Pjax, so you can determine if a request is Pjax, and if so, render the content instead of redirecting. An example may look like:

    $endURL = "main/endpoint";
    if (Yii::$app->request->isPjax) {
        return $this->run($endURL);
    } else {
        return $this->redirect([$endURL]);
    }

What has your experience been with Pjax and Yii? Comment below if you’ve found any gotchas or have better solutions than ours!


  [1]: https://github.com/defunkt/jquery-pjax
  [2]: http://en.wikipedia.org/wiki/Post/Redirect/Get

