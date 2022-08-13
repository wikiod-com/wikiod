---
title: "Declaring Entities"
slug: "declaring-entities"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Declaring a Symfony Entity as YAML
* AppBundle/Entity/Person.php


    <?php

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




* AppBundle/Resources/config/doctrine/Person.orm.yml

 
    AppBundle\Entity\Person:
        type: entity
        repositoryClass: AppBundle\Repository\PersonRepository
        table: persons
        id:
            id:
                type: integer
                id: true
                generator:
                    strategy: AUTO
        fields:
            name:
                type: string
                length: 20
                nullable: false
                column: Name
                unique: true
            age:
                type: integer
                nullable: false
                column: Age
                unique: false
        lifecycleCallbacks: { }

* Create the table


    php bin/console doctrine:schema:update --force

Or use the recommended way (assuming you already have doctrine-migrations-bundle installed in your project):


    php bin/console doctrine:migrations:diff
    php bin/console doctrine:migrations:migrate

* Create the entity automatically from the YAML

    php bin/console doctrine:generate:entities AppBundle

## Declaring a Symfony Entity with annotations
* AppBundle/Entity/Person.php


    <?php
    
    namespace AppBundle\Entity;

    use DoctrineORM\Mapping as ORM;
    use Symfony\Bridge\Doctrine\Validator\Constraints\UniqueEntity;

    /**
     * @ORM\Entity
     * @ORM\Table(name="persons")
     * @ORM\Entity(repositoryClass="AppBundle\Entity\PersonRepository")
     * @UniqueEntity("name")
     */
    class Person
    {
        /**
         * @ORM\Id
         * @ORM\Column(type="integer")
         * @ORM\GeneratedValue(strategy="AUTO")
         */
        protected $id;

        /**
         * @ORM\Column(type="string", name="name", length=20, nullable=false)
         */
        protected $name;

        /**
         * @ORM\Column(type="integer", name="age", nullable=false)
         */
        protected $age;

        public function getId()
        {
            return $this->id;
        }

        public function getName()
        {
            return $this->name;
        }

        public function setName($name)
        {
            $this->name = $name;
            return $this;
        }

        public function getAge()
        {
            return $this->age;
        }

        public function setAge($age)
        {
            $this->age = $age;
            return $this;
        }
    }

* Generate the entity

    
    php bin/console doctrine:generate:entities AppBundle/Person

* To dump the SQL statements to the screen


    php bin/console doctrine:schema:update --dump-sql


* Create the table


    php bin/console doctrine:schema:update --force


Or use the recommended way (assuming you already have doctrine-migrations-bundle installed in your project):


    php bin/console doctrine:migrations:diff
    php bin/console doctrine:migrations:migrate

