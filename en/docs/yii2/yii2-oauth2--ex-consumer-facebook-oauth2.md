---
title: "Yii2 OAuth2 - Ex consumer facebook OAuth2"
slug: "yii2-oauth2---ex-consumer-facebook-oauth2"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Create an app on facebook developer
Go to the [https://developers.facebook.com/](https://developers.facebook.com/) and create your app.
[![Interface after create a new app][1]][1]

Click `Add product` and choose `Facebook Login`

[![Setting for app facebook login][2]][2]


  [1]: https://i.stack.imgur.com/lkEkc.png
  [2]: https://i.stack.imgur.com/qRbxb.png

## Install yii2-authclient
Before install this extension, you must install yii2-app. In this example, I use yii2-basic template. Guide for installation in [here](http://www.yiiframework.com/doc-2.0/guide-start-installation.html).

Run
```
composer require --prefer-dist yiisoft/yii2-authclient
```
or add
```
"yiisoft/yii2-authclient": "~2.1.0"
```
to the `require` section of your `composer.json`.

Add config `authClientCollection` to your config `components`:

```
return [
    'components' => [
        'authClientCollection' => [
            'class' => 'yii\authclient\Collection',
            'clients' => [
                'facebook' => [
                    'class' => 'yii\authclient\clients\Facebook',
                    'clientId' => 'facebook_client_id',
                    'clientSecret' => 'facebook_client_secret',
                ],
            ],
        ]
    ],
    // ...
];
```
`facebook_client_id` is application id and `facebook_client_secret` is app secret.[![Info facebook client id and client secret][1]][1]


  [1]: https://i.stack.imgur.com/PCt5Z.png

## Add auth action and set up callback
1. Add button `Login as facebook account` to your login view:

Edit `site/login.php` in views folder, add theses line to content of page login:
```php
<?= yii\authclient\widgets\AuthChoice::widget([
     'baseAuthUrl' => ['site/auth'],
     'popupMode' => false,
]) ?>
```

Above, we set that `auth` action in `SiteController` will handler OAuth2 flow.

Now we create it.

```
class SiteController extends Controller
{
    public function actions()
    {
        return [
            'auth' => [
                'class' => 'yii\authclient\AuthAction',
                'successCallback' => [$this, 'onAuthSuccess'],
            ],
        ];
    }

    public function onAuthSuccess($client)
    {
        // do many stuff here, save user info to your app database
    }
}
```

We use `yii\authclient\AuthAction` for create url and redirect to facebook login page.

Function `onAuthSuccess` used to get user info, login to your app.



## Add redirect_url to facebook app setting
If you enable prettyUrl in your yii2-app, your redirect_uri will be:
```
http://<base_url>/web/site/auth
```
And disable pretty url:
```
http://<base_url>/web/index.php?r=site%2Fauth
```
Example:

[![Setup redirect_uri][1]][1]


  [1]: https://i.stack.imgur.com/Y8wtx.png

## Example for onAuthSuccess function
```
/**
 * @param $client ClientInterface
 */
public function onAuthSuccess($client)
{
    //Get user info
    /** @var array $attributes */
    $attributes = $client->getUserAttributes();
    $email = ArrayHelper::getValue($attributes, 'email'); //email info
    $id = ArrayHelper::getValue($attributes, 'id'); // id facebook user
    $name = ArrayHelper::getValue($attributes, 'name'); // name facebook account

    //Login user
    //For demo, I will login with admin/admin default account
    $admin = User::findByUsername('admin');
    Yii::$app->user->login($admin);
}
```

