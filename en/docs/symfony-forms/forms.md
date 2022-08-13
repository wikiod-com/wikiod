---
title: "Forms"
slug: "forms"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Syntax

 - Form createForm(string|FormTypeInterface $type, mixed $data = null,
   array $options = array())
 - FormBuilder createFormBuilder(mixed $data = null, array $options = array())

You can "customize" Event of the process of **[Form][1]** component with a Form Event compatible with **[Event Dispatcher][2]** Component.

[Symfony Docs][3] :

> The Form component provides a structured process to let you customize your forms, by making use of the EventDispatcher component. Using form events, you may modify information or fields at different steps of the workflow: from the population of the form to the submission of the data from the request.

  [1]: http://symfony.com/doc/current/components/form.html
  [2]: http://symfony.com/doc/current/components/event_dispatcher.html
  [3]: http://symfony.com/doc/current/form/events.html

## Create a custom form type
A custom form type is a class which defines a reusable form component. Custom form components can be nested to create complicated forms.

Instead of creating a form in the controller using a form builder, you can use your own type to make the code more readable, reusable and maintanable. 

Create a class which represents your form type

    // src/AppBundle/Form/ExampleType.php
    namespace AppBundle\Form;
    
    use Symfony\Component\Form\AbstractType;
    use Symfony\Component\Form\FormBuilderInterface;
    use Symfony\Component\Form\Extension\Core\Type\TextType;
    use Symfony\Component\Form\Extension\Core\Type\NumberType;
    use Symfony\Component\Form\Extension\Core\Type\SubmitType;
    
    class ExampleType extends AbstractType
    {
        public function buildForm(FormBuilderInterface $builder, array $options) {
            $builder
                 ->add('value', TextType::class, array('required' => false))
                 ->add('number', NumberType::class)
                 ->add('string', TextType::class)
                 ->add('save', SubmitType::class)
            ;
        }
    }

You can now use your form in controller:

    use AppBundle\Form\ExampleType;
    
    // ...
    
    $form = $this->createForm(ExampleType::class, $data)



## Create a simple form in a controller 
A form gives the user a way to change data in your application, in a structured way. To mutate a simple `array` of data, we create a form using a form builder:

    use Symfony\Component\Form\Extension\Core\Type\TextType;
    use Symfony\Component\Form\Extension\Core\Type\NumberType;
    use Symfony\Component\Form\Extension\Core\Type\SubmitType;
    
    // ...
    
    function myAction (Request $request) {
        $data = array(
            'value' => null,
            'number' => 10,
            'string' => 'No value',
        );
        $form = $this->createFormBuilder($data)
                     ->add('value', TextType::class, array('required' => false))
                     ->add('number', NumberType::class)
                     ->add('string', TextType::class)
                     ->add('save', SubmitType::class)
                     ->getForm();
        
        $form->handleRequest($request);
        if ($form->isValid()) {
            
            // $data is now changed with the user input
            // Do something with the data
        }
        
        return $this->render(..., array(
            'form' => $form->createView(),
            // ...
        ));
    }

In your template, render your form using the `form(...)` Twig function:

    {# Render the form #}
    {{ form(form) }}

It will, without styling, look something like below:

[![enter image description here][1]][1]

The labels, IDs, names and form tags are generated automatically. By default, the form submits to the current page with a POST request. 

 


  [1]: http://i.stack.imgur.com/i6Xeb.png

## How to deal with form options
In this example, I created a form which is used to register a new user. In the options passed to the form, I give the different roles a user can have.

Creating a reusable class for my form with configured data class and an extra option that fills the choice field to pick a userrole:

    class UserType extends AbstractType
    {
        public function buildForm(FormBuilderInterface $builder, array $options)
        {
            $builder
                ->add('firstName', TextType::class, array(
                    'label' => 'First name'
                ))
                ->add('lastName', TextType::class, array(
                    'label' => 'Last name'
                ))
                ->add('email', EmailType::class, array(
                    'label' => 'Email'
                ))
                ->add('role', ChoiceType::class, array(
                    'label' => 'Userrole',
                    'choices' => $options['rolechoices']
                ))
                ->add('plain_password', RepeatedType::class, array(
                    'type' => PasswordType::class,
                    'first_options' => array('label' => 'Password'),
                    'second_options' => array('label' => 'Repeat password')
                ))
                ->add('submit', SubmitType::class, array(
                    'label' => 'Register user'
                ));
        }
    
        public function configureOptions(OptionsResolver $optionsResolver)
        {
            $optionsResolver->setDefaults(array(
                'data_class' => 'WebsiteBundle\Entity\User',
                'rolechoices' => array()
            ));
        }
    }

As you can see, there's a default option added to the form named 'roleChoises'. This option is created and passed in the method to create a form object. See next code.

Creating a form-object in my controller:

    $user = new User();
    $roles = array(
        'Admin' => User::ADMIN_ROLE,
        'User' => User::USER_ROLE
    );
    $form = $this->createForm(UserType::class, $user, array(
        'rolechoices' => $roles
    ));



## Check if all fields are rendered in the template
When rendering a form 'by hand', it can be useful to know if there are fields left to render or not.
The function `isRendered()` from the *FormView* class returns `true` if there are still fields left to be rendered to the template.

This snippet prints `<h3>Extra fields</h3>` if there are fields left to be added to the template, followed by the fields themselves.

    {% if not form.isRendered() %}
        <h3>Extra fields</h3>
        {{ form_rest(form) }}
    {% endif %}

## Deal with form events
To be able do deal with form events, it's important to attach the request, that is send to a controller action after submitting a form, to the form created in that action.

    public function registerAction(Request $request)
    {
        $data = new ExampleObject();
        $form = $this->createForm(ExampleObjectType::class, $data);

        $form->handleRequest($request);
        if($form->isSubmitted() && $form->isValid()){
            // do something with form data
            return $this->redirectToRoute('route_name');
        }
        return $this->render('view.html.twig', array(
            'form' => $form->createView()
        ));
    }

The request variable passed to the action is of type `Symfony\Component\HttpFoundation\Request`



