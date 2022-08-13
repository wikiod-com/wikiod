---
title: "Symfony Twig Extenstion Example"
slug: "symfony-twig-extenstion-example"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Parameters
| Parameters | Description |
| ------ | ------ |
| $doctrine   | doctrine object that we pass from the service.   |

## Symfony Twig Extension Basic Example
In this example I define two custom function.
1 - countryFilter function get's the country short code as input and return the country full name.
2 - _countPrinterTasks is used to calculate the no of tasks assigned to a particular user.




    <?php
    
    namespace DashboardBundle\Twig\Extension;
    
    use Symfony\Bridge\Doctrine\RegistryInterface;
    use Symfony\Component\HttpKernel\HttpKernelInterface;
    use Symfony\Component\HttpKernel\Event\GetResponseEvent;
    use Symfony\Component\Security\Core\SecurityContext;
    
    /**
     * Class DashboardExtension
     * @package DashboardBundle\Twig\Extension
     */
    class DashboardExtension extends \Twig_Extension
    {
        protected $doctrine;
        private $context;
       
    
        /**
         * DashboardExtension constructor.
         * @param RegistryInterface $doctrine
         * @param SecurityContext $context
         */
        public function __construct(RegistryInterface $doctrine,SecurityContext $context)
        {
            $this->doctrine = $doctrine;
            $this->context = $context;
        }
    
        /**
         * @return mixed
         */
        public function getUser()
        {
            return $this->context->getToken()->getUser();
        }
    
        /**
         * @return array
         */
        public function getFilters()
        {
            return array(
                new \Twig_SimpleFilter('country', array($this, 'countryFilter')),
                new \Twig_SimpleFilter('count_printer_tasks', array($this, '_countPrinterTasks')),
            );
        }
    
    
     /**
         * @param $countryCode
         * @param string $locale
         * @return mixed
         */
        public function countryFilter($countryCode,$locale = "en")
        {
            $c = \Symfony\Component\Intl\Intl::getRegionBundle()->getCountryNames($locale);
    
            return array_key_exists($countryCode, $c)
                ? $c[$countryCode]
                : $countryCode;
        }
    
    

        /**
         * Returns total count of printer's tasks.
         * @return mixed
         */
        public function _countPrinterTasks(){
            $count = $this->doctrine->getRepository('DashboardBundle:Task')->countPrinterTasks($this->getUser());
            return $count;
        }



    
        /**
         * {@inheritdoc}
         */
        public function getName()
        {
            return 'app_extension';
        }
    }




To call it from the Twig, We just have to used as below;

    {% set printer_tasks = 0|count_printer_tasks() %}
    
     <tr>
        <td>Nationality</td>
        <td>
            {{ user.getnationality|country|ucwords }}
        </td>
    </tr>

And Declare this extenstion as a service in your `bundle/resource/config/service.yml` file.

    services:
    
        app.twig_extension:
            class: DashboardBundle\Twig\Extension\DashboardExtension
            arguments: ["@doctrine", @security.context]
            tags:
                - { name: twig.extension }

