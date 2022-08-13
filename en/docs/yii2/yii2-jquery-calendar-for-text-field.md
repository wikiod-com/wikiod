---
title: "Yii2 Jquery Calendar For Text Field"
slug: "yii2-jquery-calendar-for-text-field"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Add jquery calendar for a text field with max as current date
If we want to display a jquery calendar for end user who can pick maximum date as current date in the calendar. The below code will useful to this scenario.

    <?php
    use yii\jui\DatePicker;
    use yii\widgets\ActiveForm;
    ?>
    
    <?php $form = ActiveForm::begin(['id' => 'profile-form']); ?>
    .....
    <?= $form->field($model, 'date_of_birth')->widget(DatePicker::classname(), ['dateFormat' => 'php:M d, Y', 'options' => ['readonly' => true], 'clientOptions' => [ 'changeMonth' => true, 'changeYear' => true, 'yearRange' => '1980:'.date('Y'), 'maxDate' => '+0d']]) ?>
    .....
    <?php ActiveForm::end(); ?>

## Add jquery calendar for a text field with min date
For some forms you want display the days from future/past days and other days need to disabled, then this scenario will help. 
    
    <?php
    use yii\jui\DatePicker;
    use yii\widgets\ActiveForm;
    ?>
    
    <?php $form = ActiveForm::begin(['id' => 'profile-form']); ?>
    .....
    
    <?php
    $day = '+5d'; //if you want to display +5 days from current date means for future days.
    #(or)
    $day = '-5d'; //if you want to display -5 days from current date means older days.
    ?>
    <?= $form->field($model, 'date_of_birth')->widget(DatePicker::classname(), ['dateFormat' => 'php:M d, Y', 'options' => ['readonly' => true], 'clientOptions' => [ 'changeMonth' => true, 'changeYear' => true, 'yearRange' => '1980:'.date('Y'), 'maxDate' => $day]]) ?>
    .....
    <?php ActiveForm::end(); ?>



## Add jquery calendar with from date and to date
If you want to have calendar for from date and to date and also to date calendar days always will be greater than from date field, then below scenario will help.
    
<?php
    use yii\jui\DatePicker;
    use yii\widgets\ActiveForm;
    ?>
    
    <?php $form = ActiveForm::begin(['id' => 'profile-form']); ?>
    .....
    <?= $form->field($model, 'from_date')->widget(DatePicker::classname(), ['dateFormat' => 'php:M d, Y', 'options' => ['readonly' => true], 'clientOptions' => [ 'changeMonth' => true, 'changeYear' => true, 'yearRange' => '1980:'.date('Y'), 'onSelect' => new yii\web\JsExpression('function(selected) { var dt = new Date(selected); dt.setDate(dt.getDate() + 1); $("#filter-date-to").datepicker("option", "minDate", dt); }')]]) ?>
    
    <?= $form->field($model, 'to_date')->widget(DatePicker::classname(), ['dateFormat' => 'php:M d, Y', 'options' => ['readonly' => true, 'id' => 'filter-date-to'], 'clientOptions' => [ 'changeMonth' => true, 'changeYear' => true, 'yearRange' => '1980:'.date('Y')]]) ?>
    .....
    <?php ActiveForm::end(); ?>

