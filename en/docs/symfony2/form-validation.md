---
title: "Form Validation"
slug: "form-validation"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Simple Form Validation using constraints
Example Controller action 

    use Symfony\Component\HttpFoundation\Request;
    
    public function exampleAction(Request $request)
    {
        /*
         * First you need object ready for validation.
         * You can create new object or load it from database.
         * You need to add some constraints for this object (next example)
         */
        $book = new Book();
        
        /*
         * Now create Form object. 
         * You can do it manually using FormBuilder (below) or by creating
         * FormType class and passing it to builder.
         */ 
        $form = $this->createFormBuilder($book)
            ->add('title', TextType::class)
            ->add('pages', IntegerType::class)
            ->add('save', SubmitType::class, array('label' => 'Create Book'))
            ->getForm();
    
        /*
         * Handling Request by form.
         * All data submitted to form by POST(default) is mapped to
         * to object passed to FormBuilder ($book object)
         */ 
        $form->handleRequest($request);
    
        /*
         * Form Validation
         * In this step we check if form is submitted = data passed in POST
         * and is your object valid. Object is valid only if it pass form validation
         * in function isValid(). Validation constraints are loaded from config files
         * depending on format (annotations, YAML, XML etc). 
         * IMPORTANT - object passed (book) is validated NOT form object
         * Function isValid() using Symfony Validator component.
         */ 
        if ($form->isSubmitted() && $form->isValid()) {
            /*
             * Now object is valid and you can save or update it
             * Original object ($book) passed into form builder has been updated 
             * but you can also get variable by function getData: 
             * $book = $form->getData();
             */ 
    
            // You can now redirect user to success page
            return $this->redirectToRoute('book_success_route');
        }
    
        /*
         * If form is not submitted you show empty form to user.
         * If validation fail then the form object contains list of FormErrors.
         * Form errors are displayed in form_row template (read about form templates)
         */
        return $this->render('book/create.html.twig', array(
            'form' => $form->createView(),
        ));
    }

**Example constraints for object** 

@Annotations

    namespace AppBundle\Entity;
    
    use Symfony\Component\Validator\Constraints as Assert;
    
    class Book
    {
    

        /**
         * @Assert\Length(
         *      min = 2,
         *      max = 100,
         *      minMessage = "Book title must be at least {{ limit }} characters long",
         *      maxMessage = "Book title cannot be longer than {{ limit }} characters"
         * )
         */
        private $title;
    
        /**
         * @Assert\Range(
         *      min = 3,
         *      max = 10000,
         *      minMessage = "Book must have at least {{ limit }} pages",
         *      maxMessage = "Book cannot have more than {{ limit }} pages"
         * )
         */
        private $pages;

        // [...] getters/setters
    }

@YAML

    # src/AppBundle/Resources/config/validation.yml
    AppBundle\Entity\Book:
        properties:
            title:
                - Length:
                    min: 2
                    max: 50
                    minMessage: 'Book title must be at least {{ limit }} characters long'
                    maxMessage: 'Book title cannot be longer than {{ limit }} characters'
            pages:
                - Range:
                    min: 3
                    max: 10000
                    minMessage: Book must have at least {{ limit }} pages
                    maxMessage: Book cannot have more than {{ limit }} pages
                

Validation Constraints Reference: 
https://symfony.com/doc/current/reference/constraints.html

Form Validation:
http://symfony.com/doc/current/forms.html#form-validation

