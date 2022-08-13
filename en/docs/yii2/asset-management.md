---
title: "Asset management"
slug: "asset-management"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
- the depended assets will be loaded before this assets in given order
    - public $depends = [
            'yii\web\YiiAsset',
            'yii\bootstrap\BootstrapAsset',
            'yii\bootstrap\BootstrapPluginAsset',
        'cinghie\fontawesome\FontAwesomeAsset',
        ];





this example is based on the advanced template
https://github.com/yiisoft/yii2-app-advanced

The cinghie asset in this example is the asset package for adminLTE
https://github.com/cinghie/yii2-admin-lte

## This is part of the layout file
    <?php
    /* this example is based on the advanced template
     * This file is located in 
     * backend/views/layouts/main.php 
     */
    
    use yii\helpers\Html;
    
    // here the asset is registered
    use cinghie\adminlte\AdminLTEAsset;
    AdminLTEAsset::register($this);
    
    
    ?>
    <?php $this->beginPage() ?>
    <!DOCTYPE html>
    <html lang="<?= Yii::$app->language ?>">
    <head>
        <meta charset="<?= Yii::$app->charset ?>">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <?= Html::csrfMetaTags() ?>
        <title><?= Html::encode($this->title) ?></title>
        <?php $this->head() ?>
    </head>
       
     <body class="hold-transition skin-blue sidebar-mini">
        
    <?php $this->beginBody() ?>
    
    <?= $content ?>
    
    <?php $this->endBody() ?>
                
    </body>
    </html>
    <?php $this->endPage() ?>



## This is the Asset File
    <?php
    
    /**
    * This file is the Asset Bundle File located in 
    *  vendor/cinghie/yii2-admin-lte/AdminLTEAsset.php
    * @copyright Copyright &copy; Gogodigital Srls
    * @company Gogodigital Srls - Wide ICT Solutions 
    * @website http://www.gogodigital.it
    * @github https://github.com/cinghie/yii2-admin-lte
    * @license GNU GENERAL PUBLIC LICENSE VERSION 3
    * @package yii2-AdminLTE
    * @version 1.3.10
    */
    
    namespace cinghie\adminlte;
    
    use yii\web\AssetBundle;
    
    /**
     * Class yii2-AdminLTEAsset
     * @package cinghie\adminlte
     */
    class AdminLTEAsset extends AssetBundle
    {
    
        /**
         * @inherit
         */
        public $sourcePath = '@bower/';
    
        /**
         * @inherit
         */
        public $css = [ 
            'ionicons/css/ionicons.css',
            'admin-lte/dist/css/AdminLTE.css',
            'admin-lte/dist/css/skins/_all-skins.css'
        ];
        
        /**
         * @inherit
         */
        public $js = [
            'admin-lte/dist/js/app.js'
        ];
        
        /**
         * @inherit
         */
        public $depends = [
            'yii\web\YiiAsset',
            'yii\bootstrap\BootstrapAsset',
            'yii\bootstrap\BootstrapPluginAsset',
        'cinghie\fontawesome\FontAwesomeAsset',
        ];
    
    }



## The generated HTML with automaticaly loaded assets
    <!DOCTYPE html>
    <html lang="en-EN">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <meta name="csrf-param" content="_csrf">
        <meta name="csrf-token" content="M01tTVZLdlBlBQEqGyYcYHc5PwI1CRknfB1bBiAaPTNBfyk0Ehg8EQ==">
        <title>Profil</title>
        <link href="/assets/f3e48cde/css/bootstrap.css?v=1473788138" rel="stylesheet">
    <link href="/assets/24e44190/css/font-awesome.css?v=1473866258" rel="stylesheet">
    <link href="/assets/fa4335a5/ionicons/css/ionicons.css?v=1473866258" rel="stylesheet">
    <link href="/assets/fa4335a5/admin-lte/dist/css/AdminLTE.css?v=1473866258" rel="stylesheet">
    <link href="/assets/fa4335a5/admin-lte/dist/css/skins/_all-skins.css?v=1473866258" rel="stylesheet"></head>
    <body class="hold-transition skin-blue sidebar-mini">
    
    ....
    
    </script><script src="/assets/69b4ffbe/jquery.js?v=1473788138"></script>
    <script src="/assets/6aa8a809/yii.js?v=1473788138"></script>
    <script src="/assets/f3e48cde/js/bootstrap.js?v=1473788138"></script>
    <script src="/assets/fa4335a5/admin-lte/dist/js/app.js?v=1473866258"></script>
    
    </body>
    </html>

