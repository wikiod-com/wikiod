---
title: "Doctrine Entity Relationships"
slug: "doctrine-entity-relationships"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## One-To-Many, Bidirectional
This bidirectional mapping requires the `mappedBy` attribute on the `OneToMany` association and the `inversedBy` attribute on the `ManyToOne` association.

A bidirectional relationship has both an [owning and inverse side][1]. `OneToMany` relationships can use join tables, so you have to specify an owning side. The `OneToMany` association is always the inverse side of a bidirectional association.


    <?php
    
    namespace AppBundle\Entity;
    
    use Doctrine\ORM\Mapping as ORM;
    
    /**
     * @ORM\Entity
     * @ORM\Table(name="users")
     */
    class User
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
         * @var string
         *
         * @ORM\Column(name="username", type="string", length=255)
         */
        protected $username;
    
        /**
         * @var Group|null
         *
         * @ORM\ManyToOne(targetEntity="AppBundle\Entity\Group", inversedBy="users")
         * @ORM\JoinColumn(name="group_id", referencedColumnName="id", nullable=true)
         */
        protected $group;
    
        /**
         * @param string $username
         * @param Group|null $group
         */
        public function __construct($username, Group $group = null)
        {
            $this->username = $username;
            $this->group = $group;
        }

        /**
         * Set username
         *
         * @param string $username
         */
        public function setUsername($username)
        {
            $this->username = $username;
        }

        /**
         * Get username
         *
         * @return string 
         */
        public function getUsername()
        {
            return $this->username;
        }

        /**
         * @param Group|null $group
         */
        public function setGroup(Group $group = null)
        {
            if($this->group !== null) {
                $this->group->removeUser($this);
            }
    
            if ($group !== null) {
                $group->addUser($this);
            }
    
            $this->group = $group;
        }
    
        /**
         * Get group
         *
         * @return Group|null
         */
        public function getGroup()
        {
            return $this->group;
        }
    }


<br>

    <?php
    
    namespace AppBundle\Entity;
    
    use Doctrine\ORM\Mapping as ORM;
    use Doctrine\Common\Collections\ArrayCollection;

    /**
     * @ORM\Entity
     * @ORM\Table(name="groups")
     */
    class Group
    {
        /**
         * @ORM\Id
         * @ORM\Column(type="integer")
         * @ORM\GeneratedValue(strategy="AUTO")
         */
        protected $id;
    
        /**
         * @ORM\Column(name="name", type="string", length=255)
         */
        protected $name;
    
        /**
         * @ORM\OneToMany(targetEntity="AppBundle\Entity\User", mappedBy="group")
         */
        protected $users;

        /**
         * @param string $name
         */
        public function __construct($name)
        {
            $this->name = $name;
            $this->users = new ArrayCollection();
        }

        /**
         * @return string 
         */
        public function getName()
        {
            return $this->name;
        }
    
        /**
         * @param string $name
         */
        public function setName($name)
        {
            $this->name = $name;
        }
    
        public function addUser(User $user)
        {
            if (!$this->getUsers()->contains($user)) {
                $this->getUsers()->add($user);
            }
        }
    
        public function removeUser(User $user)
        {
            if ($this->getUsers()->contains($user)) {
                $this->getUsers()->removeElement($user);
            }
        }
    
        public function getUsers()
        {
            return $this->users;
        }
    
        public function __toString()
        {
            return (string) $this->getName();
        }
    }


  [1]: http://doctrine-orm.readthedocs.io/projects/doctrine-orm/en/latest/reference/unitofwork-associations.html

