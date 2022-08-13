---
title: "Twig"
slug: "twig"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Twig is the template engine that is part of Drupal 8. In Drupal 8, Twig files have the extension `.html.twig` and are used in every aspect of Drupal theming. Entities, fields, views can all be rendered using `.html.twig` files.

In this topic the goal is to have a cookbook on how work with Twig in the context of Drupal. If you want to learn more about the syntax or functions available [consult the documentation][1].


  [1]: http://twig.sensiolabs.org/doc/1.x/

## Twig Filter
Contrary to Drupal 7 you cannot call regular PHP functions in your templates. In Drupal 8 the way to go is by creating filters and functions.

You should use a **filter** when: you want to transform the data you want to display. Imagine you have a title that you want to always be uppercase. For example, twig has the `capitalize` filter by default that allows you to transform any text into its uppercase equivalent.

For this example we will create a filter that will allow us to shuffle a string. The way to create filters and functions is exactly [the same as regular Twig][1].

The main difference between regular Twig and Drupal 8 Twig is that in Drupal 8 you must create a service definition of the class your are creating and the class must also belong to a namespace otherwise it will not be registered as a Twig filter within the Drupal environment.

This example assumes that you have a module called `twig_shuffle_extension`.

This will be the basic service definition inn `twig_shuffle_extension.services.yml`

    services:
      twig_shuffle_extension.twig_extension:
        class: Drupal\twig_shuffle_extension\TwigExtension\TwigShuffleExtension
        tags:
          - { name: twig.extension }

The `tags` key is also absolutely required and is what tells Drupal what this class is supposed to do (i.e. register it as a Twig extension).

And now the source code that must be placed in the path defined in the `class` key of the service definition.

    // Don't forget the namespace!
    namespace Drupal\twig_shuffle_extension\TwigExtension;
    
    use Twig_Extension;
    use Twig_SimpleFilter;
    
    class TwigShuffleExtension extends Twig_Extension  {
      /**
       * This is the same name we used on the services.yml file
       */
      public function getName() {
        return 'twig_shuffle_extension.twig_extension';
      }
    
      // Basic definition of the filter. You can have multiple filters of course.
      // Just make sure to create a more generic class name ;)
      public function getFilters() {
        return [
          new Twig_SimpleFilter('shuffle', [$this, 'shuffleFilter']),
        ];
      }
    
      // The actual implementation of the filter.
      public function shuffleFilter($context) {
        if(is_string($context)) {
          $context = str_shuffle($context);
        }
        return $context;
      }
    }

Clear your caches and now, if everything goes according to plan, you can use the filter in your templates.

    {{ "shuffle me!" | shuffle }}

  [1]: https://www.wikiod.com/twig/extending-twig#Adding custom filters/functions


## Dependency Injection Into Twig Extensions
This example will show you how to use Dependency Inject to use other services registered in the Drupal environment.

Imagine you have an SVG image file that changes colors depending on some random CSS/Javascript thing in your project. To be able to target the SVG with CSS you have to actually have the SVG file in the DOM. So you create a base SVG file without any colors and place it in your theme folder.

Of course you could just paste the contents of the file in the Twig template but that wouldn't be nice. You can create a Twig extension but you also don't want to hardcode your theme path in the extension source code.

This means we have to get the path dynamically. You have two options:
1. Use the equivalent to a global variable by calling `\Drupal::theme()->getActiveTheme()->getPath();`
2. Inject the `ThemeManager` (given by `\Drupal::theme()`) in your extension class

In this example we will take the second example because it can be widely applicable to any service (you import the Request or the Database Connection if you want).

This assumes that you have a module called `twig_svg_extension` and a `twig_svg_extension.services.yml` file:

    services:
      twig_svg_extension.twig_extension:
        class: Drupal\twig_svg_extension\TwigExtension\TwigSvgExtension
        arguments: ['@theme.manager']
        tags:
          - { name: twig.extension }

Please not the `arguments` key that tells Drupal the service to inject.

    namespace Drupal\twig_svg_Extension\TwigExtension;
    
    use Drupal\Core\Theme\ThemeManager;
    use Twig_Extension;
    use Twig_SimpleFilter;
    
    class TwigSvgExtension extends Twig_Extension  {
      private $theme;
      
      // Dependency injection at work!
      public function __construct(ThemeManager $theme) {
        $this->theme = $theme;
      }
    
      public function getFilters() {
        return [
          'svg' =>new Twig_SimpleFilter('svg', [$this, 'svgFilter']),
        ];
      }

      public function getName() {
        return 'twig_svg_extension.twig_extension';
      }
    
      public function svgFilter(string $filepath) {
        $realpath = realpath($this->theme->getActiveTheme()->getPath().DIRECTORY_SEPARATOR.$filepath);
        $pathinfo = pathinfo($realpath);
    
        if($realpath !== false && strtolower($pathinfo['extension']) === 'svg') {
          return file_get_contents($realpath);
        }
    
        return '"'.$filepath.'" does not exist or is not an SVG';
      }
    }

Please note the constructor which contains the dependency we injected in the service configuration as well as the `svgFilter` that gets the current active theme path.

`$filepath` should be a relative path to your themes folder. The extension will convert the file path into the contents of the file it points to.

