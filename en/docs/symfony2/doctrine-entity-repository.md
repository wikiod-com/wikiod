---
title: "Doctrine Entity Repository"
slug: "doctrine-entity-repository"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Creating a new Repository
You can create a new Repository where ever you want, but it's recommended to create them in a seperate `Repository` folder.

While you could name the Repository file and class as you wish, it's recommended to name the Repository `EntityNameRepository`, to that you could quickly find those in your folder.

Let's assume we have an `Project` Entity, stored in `AppBundle\Entity`, it would look like this:

    <?php
        
    namespace AppBundle\Entity;
        
    use Doctrine\ORM\Mapping as ORM;
    
    /**
     * Project Entity - some information 
     *
     * @ORM\Table(name="project")
     * @ORM\Entity(repositoryClass="AppBundle\Repository\ProjectRepository")
     */
    class Project
    {
       // definition of the entity with attributes, getters, setter whatsoever
    }
        
    ?>

The important part here is the line `@ORM\Entity(repositoryClass="AppBundle\Repository\ProjectRepository")`, because it connects this Entity with the given Repository class. 

Also you need to use the `\Doctrine\ORM\Mapping` class to use the mapping options.

The repository itself is pretty simple

    <?php
    
    namespace AppBundle\Repository;
    
    class ProjectRepository extends \Doctrine\ORM\EntityRepository
    {
        public function getLastTenProjects()
        {
            // creates a QueryBuilder instance
            $qb = $this->_em->createQueryBuilder()
                ->select('p')
                ->from($this->_entityName, 'p')
                ->orderBy('p.id', 'DESC')
                ->setMaxResults(10)
            ;
            // uses the build query and gets the data from the Database
            return $qb->getQuery()->getResult();
        }
    }
    
    ?>

It's important to notice that the Repository class must extend the `\Doctrine\ORM\EntityRepository`, so that it can work properly. Now you can add as many functions for different querys as you want.

## ExpressionBuilder IN() function
If you want to use the MySQL command `IN()` in the QueryBuilder, you can do it with the `in()` function of the [ExpressionBuilder][1] class.

    // get an ExpressionBuilder instance, so that you
    $expressionBulder = $this->_em->getExpressionBuilder();
    $qb = $this->_em->createQueryBuilder()
    ->select('p')
    ->from($this->_entityName, 'p');
    ->where($expressionBuilder->in('p.id', array(1,2,3,4,5)));
    
    return $qb->getQuery()->getResult();


  [1]: http://www.doctrine-project.org/api/dbal/2.3/class-Doctrine.DBAL.Query.Expression.ExpressionBuilder.html

## Make a Query with a Sub-Query
As example, only for demonstrate HOW-TO use a subquery select statement inside a select statement, suppose we what to find all user that not yet have compile the address (no records exists in the address table):


     // get an ExpressionBuilder instance, so that you
    $expr = $this->_em->getExpressionBuilder();
    
    // create a subquery in order to take all address records for a specified user id
    $sub = $this->_em->createQueryBuilder()
        ->select('a')
        ->from($this->_addressEntityName, 'a')
        ->where('a.user = u.id');
    
    
    $qb = $this->_em->createQueryBuilder()
        ->select('u')
        ->from($this->_userEntityName, 'u')
        ->where($expr->not($expr->exists($sub->getDQL())));
    
    return $qb->getQuery()->getResult();

