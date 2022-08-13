---
title: "Symfony Twig Extensions"
slug: "symfony-twig-extensions"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## A Simple Twig Extension -- Symfony 2.8
Before creating any extension, always check if it has already **[been implemented][1]**.

The first thing one would have to do is define the extension class which will house the twig filters and/or functions.

    <?php
    
    namespace AppBundle\Twig;
    
    class DemoExtension extends \Twig_Extension {
        /**
         * A unique identifier for your application
         * 
         * @return  string
         */
        public function getName()
        {
            return 'demo';
        }
    
        /**
         * This is where one defines the filters one would to use in their twig 
         * templates
         * 
         * @return  Array
         */
        public function getFilters()
        {
            return array (
                new \Twig_SimpleFilter (
                    'price',                      // The name of the twig filter
                    array($this, 'priceFilter')   
                ),
            );
        }
    
        public function priceFilter($number, $decimals = 0, $decPoint = '.', $thousandsSep = ',')
        {
            return '$' . number_format($number, $decimals, $decPoint, $thousandsSep);
        }
    
        /**
         * Define the functions one would like availed in their twig template
         * 
         * @return  Array
         */
        public function getFunctions() {
            return array (
                new \Twig_SimpleFunction (
                    'lipsum',                      // The name of the twig function
                    array($this, 'loremIpsum')   
                )
            );
        }
    
        public function loremIpsum($length=30) {
            $string = array ();
            $words = array (
                'lorem',        'ipsum',       'dolor',        'sit',
                'amet',         'consectetur', 'adipiscing',   'elit',
                'a',            'ac',          'accumsan',     'ad',
                'aenean',       'aliquam',     'aliquet',      'ante',
                'aptent',       'arcu',        'at',           'auctor',
                'augue',        'bibendum',    'blandit',      'class',
                'commodo',      'condimentum', 'congue',       'consequat',
                'conubia',      'convallis',   'cras',         'cubilia',
                'cum',          'curabitur',   'curae',        'cursus',
                'dapibus',      'diam',        'dictum',       'dictumst',
                'dignissim',    'dis',         'donec',        'dui',
                'duis',         'egestas',     'eget',         'eleifend',
                'elementum',    'enim',        'erat',         'eros',
                'est',          'et',          'etiam',        'eu',
                'euismod',      'facilisi',    'facilisis',    'fames',
                'faucibus',     'felis',       'fermentum',    'feugiat',
                'fringilla',    'fusce',       'gravida',      'habitant',
                'habitasse',    'hac',         'hendrerit',    'himenaeos',
                'iaculis',      'id',          'imperdiet',    'in',
                'inceptos',     'integer',     'interdum',     'justo',
                'lacinia',      'lacus',       'laoreet',      'lectus',
                'leo',          'libero',      'ligula',       'litora',
                'lobortis',     'luctus',      'maecenas',     'magna',
                'magnis',       'malesuada',   'massa',        'mattis',
                'mauris',       'metus',       'mi',           'molestie'
            );
    
            for ( $i=0; $i<$length; $i++ )
                $string[] = $words[rand(0, 99)];
    
            return implode(" ", $string);
        }
    }

One then alerts the service container of the newly created twig extension.

    # app/config/services.yml
    services:
        app.twig.demo_extension:
            class: AppBundle\Twig\DemoExtension
            tags:
                - { name: twig.extension }

With this you have all you need to be able to use your newly created twig filter or function in your twig templates

    <p>Price Filter test {{ '5500' | price }}</p>
    <p>{{ lipsum(25) }}</p>

[1]:  http://twig.sensiolabs.org/doc/extensions/index.html

## Make a number short e.g. 1 000 -> 1k, 1 000 000 -> 1M etc.
Symfony 2.8

    # AppBundle\Twig\AppExtension.php
    <?php
    namespace AppBundle\Twig;
    
    class AppExtension extends \Twig_Extension
    {
        /**
         * This is where one defines the filters one would to use in their twig 
         * templates
         * 
         * @return  Array
         */
        public function getFilters()
        {
            return array(
                new \Twig_SimpleFilter('shortNumber', array($this, 'shortNumber')),
            );
        }
    
        /**
         * Shorten the number
         * 
         * @param integer
         * @return string
         */
        public function shortNumber($number)
        {
            $k   = pow(10,3);
            $mil = pow(10,6);
            $bil = pow(10,9);
    
            if ($number >= $bil)
                return number_format((float)$number / $bil, 1, '.', '').'Billion';
            else if ($number >= $mil)
                return number_format((float)$number / $mil, 1, '.', '').'M';
            else if ($number >= $k)
                return number_format((float)$number / $k, 1, '.', '').'K';
            else
                return (int) $number;
        }
    
        /**
         * Get name
         */
        public function getName()
        {
            return 'app_extension';
        }
    }

Add your extension to services.yml

    # app/config/services.yml
    services:
        app.twig_extension:
            class: AppBundle\Twig\AppExtension
            public: false
            tags:
                - { name: twig.extension }

Use it in TWIG

    <span>{{ number|shortNumber }}</span>
    e.g.
    <span>{{ 1234|shortNumber }}</span> -> <span>1.2k</span>

