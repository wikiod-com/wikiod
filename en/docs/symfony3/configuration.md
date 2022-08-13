---
title: "Configuration"
slug: "configuration"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Examples and good practices for configuring your Symfony application that aren't in the official documentation.

## Use fully qualified class name  (FQCN) as service id
In many examples, you will find a service id like 'acme.demo.service.id' (a string with dots). You `services.yml` will look like this:
 
    services:
        acme.demo.service.id:
            class: Acme\DemoBundle\Services\DemoService
            arguments: ["@doctrine.orm.default_entity_manager", "@cache"]

In your controller, you can use this service:

    $service = $this->get('acme.demo.service.id');

While there is no issue with this, you can use a Fully Qualified Class Name (FQCN) as service id:

    services:
        Acme\DemoBundle\Services\DemoService:
            class: Acme\DemoBundle\Services\DemoService
            arguments: ["@doctrine.orm.default_entity_manager", "@cache"]

In your controller you can use it like this:

    use Acme\DemoBundle\Services\DemoService;
    // ..
    $this->get(DemoService::class);

This makes your code better to understand. In many cases it makes no sense to have a service id that isn't just the class name.

As of Symfony 3.3, you can even remove the `class` attribute if you service id is a FQCN.


## Include all configuration files from a directory
After a while, you end up with many configuration items in your config.yml. It can make you configuration easier to read if you split your configuration across multiple files. You can easily include all files from a directory this way:

config.yml:

    imports:
        - { resource: parameters.yml }
        - { resource: "includes/" }

In the `includes` directory you can put for example doctrine.yml, swiftmailer.yml, etc.

## No HTTP interface needed ?
If your application does not need any HTTP interface (for example for a console only app), you will want to disable at least `Twig` and `SensioFrameworkExtra`

Just comment out those lines:

**app/AppKernel.php**

<!-- language: php -->

    $bundles = [
    //...
    //    new Symfony\Bundle\TwigBundle\TwigBundle(),
    //    new Sensio\Bundle\FrameworkExtraBundle\SensioFrameworkExtraBundle(),
    //...
    if (in_array($this->getEnvironment(), ['dev', 'test'], true)) {
    //...
    //    $bundles[] = new Symfony\Bundle\WebProfilerBundle\WebProfilerBundle();

**app/config/config.yml**

<!-- language: lang-yaml -->

    framework:
    #    ...
    #    router:
    #        resource: '%kernel.root_dir%/config/routing.yml'
    #        strict_requirements: ~
    #    ...
    #    templating:
    #        engines: ['twig']
    #...
    #twig:
    #    debug: '%kernel.debug%'
    #    strict_variables: '%kernel.debug%'

**app/config/config_dev.yml**

<!-- language: lang-yaml -->

    #framework:
    #    router:
    #        resource: '%kernel.root_dir%/config/routing_dev.yml'
    #        strict_requirements: true
    #    profiler: { only_exceptions: false }
    
    #web_profiler:
    #    toolbar: true
    #    intercept_redirects: false

You can also remove the related vendor requirements from **composer.json**:

<!-- language: lang-json -->

    "sensio/framework-extra-bundle": "x.x.x",
    "twig/twig": "x.x"

Using Symfony at all in such case is arguable, but at least it can be temporary.

