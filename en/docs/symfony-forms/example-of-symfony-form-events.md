---
title: "Example of Symfony Form Events"
slug: "example-of-symfony-form-events"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Hang on For the More Symfony Form Events in the above example.

## FormEvents::PRE_SUBMIT
This example is about changing the form depending on decisions the user did with the form previously. 

In my special case, I needed to disable a selectbox, if a certain checkbox wasn't set.

So we have the FormBuilder, where we'll set the `EventListener` on the `FormEvents::PRE_SUBMIT` event. We're using this event, because the form is already set with the submitted data of the form, but we're still able to manipulate the form.

    class ExampleFormType extends AbstractType
    {
        public function buildForm(FormBuilderInterface $builder, array $options)
        {
            $data = $builder->getData();
            $builder
                ->add('choiceField', ChoiceType::class, array(
                    'choices' => array(
                        'A' => '1',
                        'B' => '2'
                    ),
                    'choices_as_values' => true,
                ))
                ->add('hiddenField', HiddenType::class, array(
                    'required' => false,
                    'label' => ''
                ))
                ->addEventListener(FormEvents::PRE_SUBMIT, function(FormEvent $event) {
    
                    // get the form from the event
                    $form = $event->getForm();
    
                    // get the form element and its options
                    $config = $form->get('choiceField')->getConfig();
                    $options = $config->getOptions();
    
                    // get the form data, that got submitted by the user with this request / event
                    $data = $event->getData();
    
                    // overwrite the choice field with the options, you want to set
                    // in this case, we'll disable the field, if the hidden field isn't set
                    $form->add(
                        'choiceField',
                        $config->getType()->getName(),
                        array_replace(
                            $options, array(
                                'disabled' => ($data['hiddenField'] == 0 ? true : false)
                            )
                        )
                    );
                })
            ;
        }
    }



## FormEvents::PRE_SET_DATA
Requirement is to check if in a form, ‘Online_date’ field is blank or filled. If it is blank, then fill it with current date, on form load. <br/><br/>
Controller calls ‘$form->createForm()” with type “folder”. In “FolderType”, event subscriber “FolderSubscriber” is added. 

**Controller**:

                $form = $this->createForm('folder', $folder, array(
                    'action' => $this->generateUrl('folders_edit', array('id' => $folder->getId())),
                    'method' => 'post'
                ));
            

**FolderType**:

    class FolderType extends AbstractType
     {
        public function __construct( FolderSubscriber $folderSubscriber) 
        {
            $this->folderSubscriber = $folderSubscriber;
        }
        
        public function buildForm(FormBuilderInterface $builder, array $options = array())
        {
           $builder ->add("onlineDate", "datetime", array( 'widget' => 'single_text'));       
           $builder->addEventSubscriber($this->folderSubscriber);
         }
        public function getName()
        {
            return 'folder';
        }
    }

**FolderSubscriber**: Gets called from FolderType; where it is registered as Event Subscriber

    
    class FolderSubscriber implements EventSubscriberInterface
    {
    
        public static function getSubscribedEvents()
        {
            return array(
                FormEvents::PRE_SET_DATA => 'onPreSetData',
            );
        }
    
        public function onPreSetData(FormEvent $event)
        {
            $this->setDefaultOnlineDate($event);
        }
    
        protected function setDefaultOnlineDate(FormEvent $event)
        {
            $content = $event->getData();
            if (!$content->getOnlineDate() instanceof \DateTime){
                $content->setOnlineDate(new \DateTime());
            }
        }
    }

## onPostSubmit Event
This is an Education From in Symfony to take user education details. We wanted to apply validation on 2 fields, education end date  and is currently studying.

    On Post Submit Event, We will check two things
    1 -  if the user checks the checkbox of is_currently studying then end date should be empty 
    2 -  On the other side, we have to make sure, if end date is not empty, then is currently studying check box should be unchecked.

     


 
        
        
        /**
         * Class QualificationFormType
         * @package UsersBundle\Form\Type
         */
        class QualificationFormType extends AbstractType
        {

            public function buildForm(FormBuilderInterface $builder, array $options)
            {
        
                $builder
                    ->add('title')
                    ->add('institution')
                 ->add('startDate', 'date', [
                    'label' => 'Start Date',
                    'widget' => 'single_text',
                    'format' => 'dd-MM-yyyy',
                    'required' => true,
                    'constraints' =>  [
                        new Assert\NotBlank(),
                        new Assert\LessThan("today"),
                    ],
                    'trim'    => true,
                    'attr' => [
                        'maxlength' => '12',
                        'minlength' => '10',
                        'placeholder' => 'when did you start this education?',
                        'class' => 'form-control input-inline datepicker datePicker',
                        'data-provide' => 'datepicker',
                        'data-date-format' => 'dd-mm-yyyy',
                        'minViewMode' => '1'
                    ],
                    'label_attr' => [
                        'class' => 'control-label',
                    ],
                ])
                ->add('endDate', 'date', [
                    'label' => 'End Date',
                    'widget' => 'single_text',
                    'format' => 'dd-MM-yyyy',
                    'required' => false,
                    'attr' => [
                        'placeholder' => 'when did you end this education?',
                        'class' => 'form-control input-inline datepicker datePicker',
                        'data-provide' => 'datepicker',
                        'data-date-format' => 'dd-mm-yyyy',
                        'minViewMode' => '1'
                    ],
                    'label_attr' => [
                        'class' => 'control-label',
                    ],
                ])
                ->add('current', null, [
                    'label' => ucfirst('I am currently studying'),
                    'label_attr' => [
                        'class' => 'control-label',
                    ],
                ])
                ->add('save', 'submit')
                    
                ;
        
        
                $builder->addEventListener(FormEvents::POST_SUBMIT, [$this, 'onPostSubmit']);
            }
        

            function onPostSubmit(FormEvent $event) {
                $form = $event->getForm();
                $endDate = $form->get('endDate')->getData();
                $current = $form->get('current')->getData();
                If(!$current){
                   if ($startDate>$endDate ) {
                       $form['startDate']->addError(new FormError("Start Date cannot be greater than end date..."));
                   }
                }
            }
       
        }

