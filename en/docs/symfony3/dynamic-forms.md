---
title: "Dynamic Forms"
slug: "dynamic-forms"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## How to extend ChoiceType, EntityType and DocumentType to load choices with AJAX.
In Symfony, the built-in ChoiceType (and EntityType or DocumentType extending it), basicaly work with a constant choice list.

If you want to make it work with ajax calls, you have to change them to accept any sumitted extra choices.

 - **How to start with an empty choice list ?**
 
   When you build your form, just set the `choices` option to an empty `array()` :

        namespace AppBundle\Form;

        use Symfony\Component\Form\AbstractType;

        use Symfony\Component\Form\FormBuilderInterface;

        use Symfony\Component\Form\Extension\Core\Type\ChoiceType;

        class FooType extends AbstractType
        {
            public function buildForm(FormBuilderInterface $builder, array $options)
            {
                $builder
                    ->add('tag', ChoiceType::class, array('choices'=>array()));
            }
        }

   So you will get an empty select input, without choices.
   This solution works for ChoiceType and all of his children (EntityType, DocumentType, ...).


 - **How to accept submitted new choices** :

    To accept the new choices, you have to make them available in the form field choicelist. You can change your form field depending on submitted data with the FormEvent::PRE_SUBMIT event. 

    This example show how to do it with a basic ChoiceType :

        namespace AppBundle\Form;

        use Symfony\Component\Form\AbstractType;

        use Symfony\Component\Form\FormBuilderInterface;

        use Symfony\Component\Form\Extension\Core\Type\ChoiceType;

        class FooType extends AbstractType
        {
            public function buildForm(FormBuilderInterface $builder, array $options)
            {
                $builder
                    ->add('tag', ChoiceType::class, array('choices'=>array()))
                ;
                
                $builder->addEventListener(
                    FormEvents::PRE_SUBMIT,
                    function(FormEvent $event){
                        // Get the parent form
                        $form = $event->getForm();
                        
                        // Get the data for the choice field
                        $data = $event->getData()['tag'];
                        
                        // Collect the new choices
                        $choices = array();
                        
                        if(is_array($data)){
                            foreach($data as $choice){
                                $choices[$choice] = $choice;
                            }
                        }
                        else{
                            $choices[$data] = $data;
                        }
                        
                        // Add the field again, with the new choices :
                        $form->add('tag', ChoiceType::class, array('choices'=>$choices));
                    }
                );
            }
        }

   Your submitted choices are now allowed choices and Symfony ChoiceType built-in validation won't reject them anymore.
   
    If you wan to do the same with a ChoiceType child (EntityType, DocumentType, ...), you have to inject the entityManager or the documentManager and to do the datatransformation when populating the new choices.

## Populate a select field depending on the value another.
This is an example to show how to change the allowed choices on a subCategory select field depending on the value of the category select field.
To do that you have to make your subCategory choices dynamical for both client and server side.

**1. Make the form dynamic on the client side for display / user interactions**

   Example of client side dynamic form (using Javascript / JQuery) :

        $('#category').change(function(){
            switch($(this).val()){
                case '1': // If category == '1'
                    var choice = {
                        'choice1_1':'1_1',
                        'choice1_2':'1_2',
                        'choice1_3':'1_3',
                    };
                break;
                case '2': // If category == '2'
                    var choice = {
                        'choice2_1':'2_1',
                        'choice2_2':'2_2',
                        'choice2_3':'2_3',
                    };
                break;
                case '3': // If category == '3'
                    var choice = {
                        'choice3_1':'3_1',
                        'choice3_2':'3_2',
                        'choice3_3':'3_3',
                    };        
                break;
            }
            
            var $subCategorySelect = $('#subCategory');
            
            $subCategorySelect.empty();
            $.each(choice, function(key, value) {
                $subCategorySelect.append($('<option></option>')).attr('value',value).text(key);
            });
        });

   Of course you could get the choices from an AJAX call. That's not the purpose of this example.
    


**2. Make the form dynamic on the server side for initialisation / validation**
    
   Example of server side dynamic form :

        namespace AppBundle\Form;

        use Symfony\Component\Form\AbstractType;
        use Symfony\Component\Form\FormBuilderInterface;

        use Symfony\Component\Form\Extension\Core\Type\ChoiceType;

        use Symfony\Component\Form\FormEvent;
        use Symfony\Component\Form\FormEvents;

        class MyBaseFormType extends AbstractType
        {
            /**
             * @param FormBuilderInterface $builder
             * @param array $options
             */
            public function buildForm(FormBuilderInterface $builder, array $options)
            {
                $builder
                    ->add('category',ChoiceType::class,array('choices'=>array(
                            'choice1'=>'1',
                            'choice2'=>'2',
                            'choice3'=>'3',
                        )))
                ;
                
                $addSubCategoryListener = function(FormEvent $event){
                    $form = $event->getForm();
                    $data = $event->getData();
                    
                    switch($data['category']){
                        case '1': // If category == '1'
                            $choices = array(
                                'choice1_1'=>'1_1',
                                'choice1_2'=>'1_2',
                                'choice1_3'=>'1_3',
                            );
                        break;
                        case '2': // If category == '2'
                            $choices = array(
                                'choice2_1'=>'2_1',
                                'choice2_2'=>'2_2',
                                'choice2_3'=>'2_3',
                            );                        
                        break;
                        case '3': // If category == '3'
                            $choices = array(
                                'choice3_1'=>'3_1',
                                'choice3_2'=>'3_2',
                                'choice3_3'=>'3_3',
                            );                        
                        break;
                    }
                    
                    $form->add('subCategory',ChoiceType::class,array('choices'=>$choices));
                };
                
                // This listener will adapt the form with the data passed to the form during construction :
                $builder->addEventListener(FormEvents::PRE_SET_DATA, $addSubCategoryListener);
                
                // This listener will adapt the form with the submitted data :
                $builder->addEventListener(FormEvents::PRE_SUBMIT, $addSubCategoryListener);
            }
        }
    

