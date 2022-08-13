---
title: "Sending options to a form class"
slug: "sending-options-to-a-form-class"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Syntax
 - $form = $this->createForm(HouseholdType::class, $household, $formOptions);

## Parameters
| Parameter | Definition|
| ------ | ------ |
| HouseholdType::class|  custom form class for the Household entity|
| $household| an instance of the Household entity (usually created by `$household = new Household();`)|
| $formOptions| an array of user-defined options to be passed to the form class, e.g.,`$formOptions = array('foo' => 'bar');`|



When you create a form class the form fields are added in the `public function buildForm(FormBuilderInterface $builder, array $options) {...}` function. The `$options` parameter includes a set of default options such as `attr` and `label`.  To enable your custom options to be available in the form class the options need to be initialized in `configureOptions(OptionsResolver $resolver)`

So for our real-world example:

    public function configureOptions(OptionsResolver $resolver)
    {
        $resolver->setDefaults(array(
            'data_class' => 'AppBundle\Entity\Household',
            'disabledOptions' => [],
        ));
    }



## A real-world example from a Household controller
Background: The Household entity includes a set of options, each of which is an entity that is managed in an admin backend.  Each option has a boolean `enabled` flag.  If a previously enabled option is set to disabled it will need to be persisted in later Household edits, but cannot be edited away.  To accomplish this the field definition in the form class will display field as a disabled choice field if the option has `enabled = false` (but is persisted because the submit button triggers a javascript that removes the `disabled` attribute.)  The field definition also prevents disabled options from being displayed.

The form class then needs to know, for a given Household entity, which of its options have been disabled.  A service has been defined that returns an array of the names of option entities that have been disabled.  That array is `$disabledOptions`.

        $formOptions = [
            'disabledOptions' => $disabledOptions,
            ];
        $form = $this->createForm(HouseholdType::class, $household, $formOptions);


## Housing entity
    /**
     * Housing.
     *
     * @ORM\Table(name="housing")
     * @ORM\Entity
     */
    class Housing
    {
        /**
         * @var int
         *
         * @ORM\Column(name="id", type="integer")
         * @ORM\Id
         * @ORM\GeneratedValue(strategy="AUTO")
         */
        protected $id;
    
        /**
         * @var bool
         *
         * @ORM\Column(name="housing", type="string", nullable=false)
         * @Assert\NotBlank(message="Housing may not be blank")
         */
        protected $housing;
    
        /**
         * @var bool
         *
         * @ORM\Column(name="enabled", type="boolean", nullable=false)
         */
        protected $enabled;
    
        /**
         * Get id.
         *
         * @return int
         */
        public function getId()
        {
            return $this->id;
        }
    
        /**
         * Set housing.
         *
         * @param int $housing
         *
         * @return housing
         */
        public function setHousing($housing)
        {
            $this->housing = $housing;
    
            return $this;
        }
    
        /**
         * Get housing.
         *
         * @return int
         */
        public function getHousing()
        {
            return $this->housing;
        }
    
        /**
         * Set enabled.
         *
         * @param int $enabled
         *
         * @return enabled
         */
        public function setEnabled($enabled)
        {
            $this->enabled = $enabled;
    
            return $this;
        }
    
        /**
         * Get enabled.
         *
         * @return int
         */
        public function getEnabled()
        {
            return $this->enabled;
        }
    
        /**
         * @var \Doctrine\Common\Collections\Collection
         *
         * @ORM\OneToMany(targetEntity="Household", mappedBy="housing")
         */
        protected $households;
    
        public function addHousehold(Household $household)
        {
            $this->households[] = $household;
        }
    
        public function getHouseholds()
        {
            return $this->households;
        }
    }



## How the custom options are used in the form class
            ->add('housing', EntityType::class,
                array(
                'class' => 'AppBundle:Housing',
                'choice_label' => 'housing',
                'placeholder' => '',
                'attr' => (in_array('Housing', $options['disabledOptions']) ? ['disabled' => 'disabled'] : []),
                'label' => 'Housing: ',
                'query_builder' => function (EntityRepository $er) use ($options) {
                    if (false === in_array('Housing', $options['disabledOptions'])) {
                        return $er->createQueryBuilder('h')
                            ->orderBy('h.housing', 'ASC')
                            ->where('h.enabled=1');
                    } else {
                        return $er->createQueryBuilder('h')
                            ->orderBy('h.housing', 'ASC');
                    }
                },
            ))


