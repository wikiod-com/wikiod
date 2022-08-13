---
title: "Basic routing"
slug: "basic-routing"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Annotation-based routing
By default, all the controllers you generate with Symfony's built-in `generate:controller` command will make use of Symfony annotations for routing:

```
namespace AppBundle\Controller;

// You have to add a use statement for the annotation
use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;

class AcmeController
{
    /**
     * @Route("/index")
     */
    public function indexAction()
    {
        // ...
    }
}
```

In order for the framework to handle these routes, you need to import them in your `routing.yml` as follows (notice the `annotation` type):

```
app:
    resource: "@AppBundle/Controller"
    type:     annotation
```

## YAML routes
Instead of annotations, you can also specify your routes as YAML:

```
app_index:
    path: /index
    defaults: { _controller: AppBundle:Acme:index }
```

The same options apply to both annotations and YAML configurations. To import a YAML routing configuration in your root routing configuration, you don't need to specify a type:

```
app:
    prefix: /app
    resource: "@AppBundle/Resources/config/routing.yml"
```

