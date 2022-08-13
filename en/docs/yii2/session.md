---
title: "Session"
slug: "session"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Session in yii2

## import Session Class

    use  yii\web\Session;

## Create a session

    $session = Yii::$app->session;
    $session->open(); // open a session
    $session->close();  // close a session

## Store the value in session variable.

    $session = Yii::$app->session;
    
    $session->set('name', 'stack');
    OR
    $session['name'] = 'stack';
    OR
    $_SESSION['name'] = 'stack';

## Get the value from the session variable.

    $name = $session->get('name');
    OR
    $name = $session['name'];

## Remove the session variable

    $session->remove('name');
    OR
    unset($session['name']);
    OR
    unset($_SESSION['name']);

    $session->destroy(); // destroy all session 

## Remove all session variables

    $session->removeAll();

## Check Session variable

    $session->has('name')
    OR
    isset($session['name'])
    //both function return boolean value [true or false]


## Session Flash

Set session flash

    $session = Yii::$app->session;
    $session->setFlash('error', 'Error in login');

Get session flash

    echo $session->getFlash('error');

Check session flash

    $result = $session->hasFlash('error');

Remove session flash

    $session->removeFlash('error');

Remove all session flash variables

    $session->removeAllFlashes();

## Directly use session variable
Set and get session variable

    \Yii::$app->session->set('name','stack');
    \Yii::$app->session->get('name');

Session flash

    \Yii::$app->getSession()->setFlash('flash_msg','Message');
    \Yii::$app->getSession()->getFlash('flash_msg');

## Remember URL to revisit later
Use case: remember the current URL to return to after adding a new record in a different (related) controller, for instance create a new contact to add to an invoice being edited.

InvoiceController / actionUpdate:

    Url::remember(Url::current(), 'returnInvoice');


ContactController / actionCreate:

    if ($model->save()) {
        $return = Url::previous('returnInvoice');
        if ($return) {
            return $this->redirect($return);
        }
        // ...
    }

You can reset the remembered URL once you're done:

InvoiceController / actionUpdate:

    if ($model->save()) {
        Url::remember(null, 'returnInvoice');
        // ...
    }

The key name - `returnInvoice` in this example - is optional.


## Creating and editing session variables that are arrays
Save the session variable as a variable.

    $session = Yii::$app->session;
    
    $sess = $session['keys'];

Then create or update the array value you want

    $sess['first'] = 'abc';

And finally save to the session variable

    $session['keys'] = $sess

