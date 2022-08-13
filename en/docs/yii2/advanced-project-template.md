---
title: "Advanced Project Template"
slug: "advanced-project-template"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Deployment in shared hosting environment
Deploying an advanced project template to shared hosting is a bit trickier than a basic one because it has two webroots, which shared hosting webservers don't support. We will need to adjust the directory structure so frontend URL will be http://site.local and backend URL will be http://site.local/admin.

## Move entry scripts into single webroot
First of all we need a webroot directory. Create a new directory and name it to match your hosting webroot name, e.g., www or public_html or the like. Then create the following structure where www is the hosting webroot directory you just created:

    www
        admin
    backend
    common
    console
    environments
    frontend
    ...

> **www** will be our frontend directory so move the contents of **frontend/web** into it. Move the contents of **backend/web** into **www/admin**. In each case you will need to adjust the paths in **index.php** and **index-test.php**.

## Adjust sessions and cookies

Originally the backend and frontend are intended to run at different domains. When we’re moving it all to the same domain the frontend and backend will be sharing the same cookies, creating a clash. In order to fix it, adjust backend application **config backend/config/main.php** as follows:

    'components' => [
        'request' => [
            'csrfParam' => '_csrf-backend',
            'csrfCookie' => [
                'httpOnly' => true,
                'path' => '/admin',
            ],
        ],
        'user' => [
            'identityClass' => 'common\models\User',
            'enableAutoLogin' => true,
            'identityCookie' => [
                'name' => '_identity-backend',
                'path' => '/admin',
                'httpOnly' => true,
            ],
        ],
        'session' => [
            // this is the name of the session cookie used for login on the backend
            'name' => 'advanced-backend',
            'cookieParams' => [
                'path' => '/admin',
            ],
        ],
    ],

Hope this helps for the shared hosting users to deploy advanced application. 

> credits : https://github.com/yiisoft/yii2-app-advanced/blob/master/docs/guide/topic-shared-hosting.md

## Sharing uploaded files between the frontend and backend using symlinks
So you've uploaded your files to a folder say `/backend/web/uploads/` and you want these uploads to be visible on the frontend too. The easiest option is to create a symlink in the frontend that links to the backend:

    ln -s /path/to/backend/web/uploads/ /path/to/frontend/web/uploads

In your views you can use relative links to the files now:

    <img src='/uploads/<?= $model->image?>' alt='My Image goes here'>
    <a href='/uploads/<?= $model->filename?>' target='_blank'>Download File</a>

Ensure that your webserver allows symlinks to be followed.

