---
title: "Installing Extension Manually"
slug: "installing-extension-manually"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Install Extension without Composer
> Note: it's strongly advised to use Composer. The instruction below is basically what Composer does for you.

- Download archive extension file of needed version from Github
- Open `composer.json`
- Find `PSR-4` autoload section and remember it for e.g. `kmit/select2`
- Extract files to corresponding folder in vendor folder, like `vendor/kmit/select2`
- Add following code to `vendor/composer/autoload_psr4.php`

```
'kmit\\select2\\' => array($vendorDir . '/kmit/select2'),
```

- Add following code to `vendor/yiisoft/extensions.php`:

```
'kmit/select2'(name of extension from composer.json file of extension) =>
    array (
        'name' => 'kmit/select2',
        'version' => '1.0.0.0',
        'alias' => array (
            '@vendor/kmit/select2'(path of extension folder alias) => $vendorDir . '/kmit/select2' (path of extension folder),
        ),
    ),
```
[Video Tutroial][1]


  [1]: https://www.youtube.com/watch?v=vTX3Amt89I0

