---
title: "Routing and URLs"
slug: "routing-and-urls"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

All URLs should be created via helper `yii\helpers\Url` it helps you to much if you decide to change url rules in urlManager.

## Creating URLs
Helper [yii\helpers\Url][1] provides a set of static methods for managing URLs. This helper may used in views/controllers code.

URL to a route:    

    echo Url::to(['post/index']);

URL to a route with parameters:

    echo Url::to(['post/view', 'id' => 100]);

anchored URL:

    echo Url::to(['post/view', 'id' => 100, '#' => 'content']);

absolute URL:

    echo Url::to(['post/index'], true);

absolute URL using the https scheme:

    echo Url::to(['post/index'], 'https');

**Note:** The route passed to the `Url::to()` method is context sensitive. It may use current module and current controller.
For example, assume the current module is `admin` and the current controller is `post`:

relative route with action ID only (contains no slashes at all): 

    echo Url::to(['index']);    // -->> '/index.php?r=admin%2Fpost%2Findex'

relative route (has no leading slash): 

    echo Url::to(['post/index']);    // -->> '/index.php?r=admin%2Fpost%2Findex'

absolute route (starts with slash): 

    echo Url::to(['/post/index']);    // -->> '/index.php?r=post%2Findex'

current requested URL:

    echo Url::to();
    echo Url::to(['']);

To create URL based on the **current route** and the **GET parameters** use [Url::current()][2].

Assume `$_GET = ['id' => 10, 'page' => 7]`, current route is `post/view`.

current URL:

    echo Url::current();    // -->> '/index.php?r=post%2Fview&id=10&page=7'

current URL without `page` parameter:

    echo Url::current(['page' => null]);  // -->> '/index.php?r=post%2Fview&id=10'

current URL with changed `page` parameter:

    echo Url::current(['page' => 12]);    // -->> '/index.php?r=post%2Fview&id=10&page=12'





  [1]: http://www.yiiframework.com/doc-2.0/yii-helpers-url.html
  [2]: http://www.yiiframework.com/doc-2.0/yii-helpers-baseurl.html#current()-detail

