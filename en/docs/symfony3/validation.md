---
title: "Validation"
slug: "validation"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

In fact, form validation is based from a component, named "**[Validator Component][1]**".

You can often use the dedicated service if you did't have to show a form in a template. Like APIs. You may validate datas in the same way, like this :

For example, *[based on symfony doc][2]* :

    $validator = $this->get('validator');
    $errors = $validator->validate($author);
    
    if (count($errors) > 0) {
        /*
         * Uses a __toString method on the $errors variable which is a
         * ConstraintViolationList object. This gives us a nice string
         * for debugging.
         */
        $errorsString = (string) $errors;
    }


  [1]: http://symfony.com/doc/current/components/validator.html
  [2]: http://symfony.com/doc/current/validation.html

## Symfony validation using annotations
* Enable validation using annotations in `app/config/config.yml` file



    framework:
        validation: { enable_annotations: true }


* Create an Entity in `AppBundle/Entity` directory. The validations are made with `@Assert` annotations.


    <?php
    # AppBundle/Entity/Car.php


    namespace AppBundle\Entity;


    use Doctrine\ORM\Mapping as ORM;
    use Symfony\Component\Validator\Constraints as Assert;

    /**
     * Car
     *
     * @ORM\Table(name="cars")
     * @ORM\Entity(repositoryClass="AppBundle\Repository\CarRepository")
     */
    class Car
    {
        /**
         * @var int
         *
         * @ORM\Column(name="id", type="integer")
         * @ORM\Id
         * @ORM\GeneratedValue(strategy="AUTO")
         */
        private $id;

        /**
         * @var string
         *
         * @ORM\Column(name="name", type="string", length=50)
         * @Assert\NotBlank(message="Please provide a name")
         * @Assert\Length(
         *     min=3,
         *     max=50,
         *     minMessage="The name must be at least 3 characters long",
         *     maxMessage="The name cannot be longer than 50 characters"
         * )
         * @Assert\Regex(
         *     pattern="/^[A-Za-z]+$/",
         *     message="Only letters allowed"
         * )
         */
        private $name;

        /**
         * @var string
         *
         * @ORM\Column(name="number", type="integer")
         * @Assert\NotBlank(message="Please provide a number")
         * @Assert\Length(
         *     min=1,
         *     max=3,
         *     minMessage="The number field must contain at least one number",
         *     maxMessage="The number field must contain maximum 3 numbers"
         * )
         * @Assert\Regex(
         *     pattern="/^[0-9]+$/",
         *     message="Only numbers allowed"
         * )
         */
        private $number;


        /**
         * Get id
         *
         * @return int
         */
        public function getId()
        {
            return $this->id;
        }

        /**
         * Set name
         *
         * @param string $name
         *
         * @return Car
         */
        public function setName($name)
        {
            $this->name = $name;

            return $this;
        }

        /**
         * Get name
         *
         * @return string
         */
        public function getName()
        {
            return $this->name;
        }

        /**
         * Set number
         *
         * @param integer $number
         *
         * @return Car
         */
        public function setNumber($number)
        {
            $this->number = $number;

            return $this;
        }

        /**
         * Get number
         *
         * @return integer
         */
        public function getNumber()
        {
            return $this->number;
        }
    }


* Create a new form in `AppBundle/Form` directory.


    <?php
    # AppBundle/Form/CarType.php

    namespace AppBundle\Form;

    use Symfony\Component\Form\AbstractType;
    use Symfony\Component\Form\FormBuilderInterface;
    use Symfony\Component\OptionsResolver\OptionsResolver;
    use Symfony\Component\OptionsResolver\OptionsResolverInterface;
    use Symfony\Component\Form\Extension\Core\Type\TextType;
    use Symfony\Component\Form\Extension\Core\Type\IntegerType;

    class CarType extends AbstractType
    {
        /**
         * @param FormBuilderInterface $builder
         * @param array $options
         */
        public function buildForm(FormBuilderInterface $builder, array $options)
        {
            $builder
                ->add('name', TextType::class, ['label'=>'Name'])
                ->add('number', IntegerType::class, ['label'=>'Number'])
            ;
        }

        /**
         * @param OptionsResolver $resolver
         */
        public function configureOptions(OptionsResolver $resolver)
        {
            $resolver->setDefaults(array(
                'data_class' => 'AppBundle\Entity\Car'
            ));
        }

        public function setDefaultOptions(OptionsResolverInterface $resolver)
        {
            // TODO: Implement setDefaultOptions() method.
        }

        public function getName()
        {
            return 'car_form';
        }
    }


* Create a new route and a new action method in `AppBundle/Controller/DefaultController.php`. The route will be declared with annotations too, so make sure you've imported this route in the main route file (`app/config/routing.yml`).



    <?php

    namespace AppBundle\Controller;

    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Symfony\Component\HttpFoundation\Request;
    use Symfony\Component\Routing\Annotation\Route;
    use AppBundle\Entity\Car;
    use AppBundle\Form\CarType;

    class DefaultController extends Controller
    {
        /**
         * @Route("/car", name="app_car")
         */
        public function carAction(Request $request)
        {
            $car = new Car();

            $form = $this->createForm(
                CarType::class,
                $car,
                [
                    'action' => $this->generateUrl('app_car'),
                    'method'=>'POST',
                    'attr'=>[
                        'id'=>'form_car',
                        'class'=>'car_form'
                    ]
                ]
            );

            $form->handleRequest($request);

            return $this->render(
                'AppBundle:Default:car.html.twig',[
                    'form'=>$form->createView()
                ]
            );
        }
    }


* Create the view in `AppBundle/Resources/views/Default/car.html.twig`.


    {% extends '::base.html.twig' %}

    {% block body %}
        {{ form_start(form, {'attr': {'novalidate':'novalidate'}}) }}
        {{ form_row(form.name) }}
        {{ form_row(form.number) }}
        <button type="submit">Go</button>
        {{ form_end(form) }}
    {% endblock %}


* Start Symfony's built-in server (`php bin/console server:run`) and access `127.0.0.1:8000/car` route in your browser. There should be a form consisting of two input boxes and a submit button. If you press the submit button without entering any data into the input boxes, then the error messages will be displayed.

## Symfony validation using YAML
* Create an Entity in `AppBundle/Entity` directory. You can do this manually, or by using Symfony's command `php bin/console doctrine:generate:entity` and filling in the required information in each step. You must specify `yml` option at `Configuration format (yml, xml, php or annotation)` step.



    <?php
    # AppBundle/Entity/Person.php

    namespace AppBundle\Entity;

    /**
     * Person
     */
    class Person
    {
        /**
         * @var int
         */
        private $id;

        /**
         * @var string
         */
        private $name;

        /**
         * @var int
         */
        private $age;


        /**
         * Get id
         *
         * @return int
         */
        public function getId()
        {
            return $this->id;
        }

        /**
         * Set name
         *
         * @param string $name
         *
         * @return Person
         */
        public function setName($name)
        {
            $this->name = $name;

            return $this;
        }

        /**
         * Get name
         *
         * @return string
         */
        public function getName()
        {
            return $this->name;
        }

        /**
         * Set age
         *
         * @param integer $age
         *
         * @return Person
         */
        public function setAge($age)
        {
            $this->age = $age;

            return $this;
        }

        /**
         * Get age
         *
         * @return int
         */
        public function getAge()
        {
            return $this->age;
        }
    }


* Create the Entity mapping information for the Entity class. If you are using Symfony's command `php bin/console doctrine:generate:entity`, then the following code will be auto-generated. Otherwise, if you don't use the command, you can create the following code by hand.


    # AppBundle/Resources/config/doctrine/Person.orm.yml

    AppBundle\Entity\Person:
    type: entity
    table: persons
    repositoryClass: AppBundle\Repository\PersonRepository
    id:
        id:
            type: integer
            id: true
            generator:
                strategy: AUTO
    fields:
        name:
            type: string
            length: '50'
        age:
            type: integer
    lifecycleCallbacks: {  }


* Create the validation for the Entity class.


    # AppBundle/Resources/config/validation/person.yml

    AppBundle\Entity\Person:
        properties:
            name:
                - NotBlank:
                    message: "Name is required"
                - Length:
                    min: 3
                    max: 50
                    minMessage: "Please use at least 3 chars"
                    maxMessage: "Please use max 50 chars"
                - Regex:
                    pattern: "/^[A-Za-z]+$/"
                    message: "Please use only letters"
            age:
                - NotBlank:
                    message: "Age is required"
                - Length:
                    min: 1
                    max: 3
                    minMessage: "The age must have at least 1 number in length"
                    maxMessage: "The age must have max 3 numbers in length"
                - Regex:
                    pattern: "/^[0-9]+$/"
                    message: "Please use only numbers"

* Create a new form in `AppBundle/Form` directory.


    <?php
    # AppBundle/Form/PersonType.php

    namespace AppBundle\Form;

    use Symfony\Component\Form\AbstractType;
    use Symfony\Component\Form\FormBuilderInterface;
    use Symfony\Component\OptionsResolver\OptionsResolver;
    use Symfony\Component\OptionsResolver\OptionsResolverInterface;
    use Symfony\Component\Form\Extension\Core\Type\TextType;
    use Symfony\Component\Form\Extension\Core\Type\IntegerType;

    class PersonType extends AbstractType
    {
        /**
         * @param FormBuilderInterface $builder
         * @param array $options
         */
        public function buildForm(FormBuilderInterface $builder, array $options)
        {
            $builder
                ->add('name', TextType::class, ['label'=>'Name'])
                ->add('age', IntegerType::class, ['label'=>'Age'])
            ;
        }

        /**
         * @param OptionsResolver $resolver
         */
        public function configureOptions(OptionsResolver $resolver)
        {
            $resolver->setDefaults(array(
                'data_class' => 'AppBundle\Entity\Person'
            ));
        }

        public function setDefaultOptions(OptionsResolverInterface $resolver)
        {
            // TODO: Implement setDefaultOptions() method.
        }

        public function getName()
        {
            return 'person_form';
        }
    }


* Create a new route in `AppBundle/Resources/config/routing.yml`


    app_person:
        path: /person
        defaults: { _controller: AppBundle:Default:person }


* Now create a new action method for that route.


    <?php
    # AppBundle/Controller/DefaultController.php

    namespace AppBundle\Controller;

    use Symfony\Bundle\FrameworkBundle\Controller\Controller;
    use Symfony\Component\HttpFoundation\Request;
    use AppBundle\Entity\Person;
    use AppBundle\Form\PersonType;

    class DefaultController extends Controller
    {
        public function personAction(Request $request)
        {
            $person = new Person();

            $form = $this->createForm(
                PersonType::class,
                $person,
                [
                    'action' => $this->generateUrl('app_person'),
                    'method'=>'POST',
                    'attr'=>[
                        'id'=>'form_person',
                        'class'=>'person_form'
                    ]
                ]
            );

            $form->handleRequest($request);

            return $this->render(
                'AppBundle:Default:person.html.twig', [
                    'form'=>$form->createView()
                ]
            );
        }
    }


* Create the view in `AppBundle/Resources/views/Default/person.html.twig`


    {% extends '::base.html.twig' %}

    {% block body %}
        {{ form_start(form, {'attr': {'novalidate':'novalidate'}}) }}
            {{ form_row(form.name) }}
            {{ form_row(form.age) }}
            <button type="submit">Go</button>
        {{ form_end(form) }}
    {% endblock %}


* Start Symfony's built-in server (`php bin/console server:run`) and access `127.0.0.1:8000/person` route in your browser. There should be a form consisting of two input boxes and a submit button. If you press the submit button without entering any data into the input boxes, then the error messages will be displayed.

