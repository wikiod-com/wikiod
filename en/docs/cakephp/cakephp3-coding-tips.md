---
title: "CakePHP3 Coding Tips"
slug: "cakephp3-coding-tips"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Creating new Controller
    namespace App\Controller;
    
    class PostsController extends AppController {

        public function initialize(){
            parent::initialize();
            // code that you want to run before every action
        }
        public function view($id) {
           //Your code here
        }
    }

## Retrieving post data equivalent to $_POST
You can retrieve post data as Array.

    $post_data= $this->request->data;

You can retrieve post data for particular key.

    $this->request->data['field'];

Retrieve specific key value

    $this->request->data('key_name');

Retrieve specific key value of nested array

    $this->request->data('data.subfield');

the difference between the array notation and `data()` method is that `data()` is error safe and returns `null` if the key does not exist in the array

so intead of doing 

    if(isset($this->request->data['field']) && $this->request->data['field']) { ...}

you can do

    if($this->request->data('field')) { ...}
    
for CakePHP 3.4.x +
   
get all data:

    $this->request->getData();
get specific key:

    $this->request->getData('key');
to set data available for getData function you have to do something like:

    $this->request = $this->request->withData('some_key_on_the_fly', 'value');
    $some_key_on_the_fly = $this->request->getData('some_key_on_the_fly');
useful for updating the models in controller with static data

## Add beforeFilter() method in Controller
The beforeFilter() method executes before any other method executes in the controller.

First use the Event namespace before defining the class in your controller file.

    use Cake\Event\Event;

In your controller, add the beforeFilter() method as shown below.

    public function beforeFilter(Event $event) {
        parent::beforeFilter($event);
    }

Or, You can use the `initialize()` method.

    public function initialize(){
        parent::initialize();
    }

## Passing variables to View
Pass each variable to view at a time

    $this->set('color', 'pink');
    $this->set('color', $color);

Pass multiple variables to view together via compact() function

    $color1 = 'pink';
    $color2 = 'red';
    $this->set(compact('color1', 'color2'));



## Passing Variable to Action from URL with redirect
Passing Variable in URL as a **method's parameter**

    return $this->redirect([
        'controller' => 'users',
        'action' => 'profile',
        $id
    ]);

Url should be looks like this http://your_app_url/users/profile/{id}

in UsersController.php file in profile() method

    class UsersController extends Controller {
        public function profile($id=null) {
            $userData=$this->Users->get($id);
        }
    }

Passing variable in URL as a **Query String** 

    return $this->redirect([
        'controller' => 'users',
        'action' => 'profile',
        '?'=>['id'=>$id]
    ]);

Url should be looks like this http://your_app_url/users/profile/?id={id}

in UsersController.php file in profile() method

    class UsersController extends Controller {
        public function profile() {
            $userData=$this->Users->get($this->request->query('id'));
        }
    }


## Redirecting to another page from Controller
Redirect to within application (another action of specific controller).

    return $this->redirect([
        'controller' => 'myController',
        'action' => 'myAction'
    ]);

Redirect to referrer page

    return $this->redirect($this->referer());

Redirect to outside of application or specific URL

    return $this->redirect("http://stackoverflow.com/users/1793428/haresh-vidja");


## Set or Change Layout of application
Set default layout **for entire application**. i.e, created layout file in /src/Template/Layout/admin.ctp

    class AppsController extends Controller {

        public function beforeFilter(Event $event) {
            parent::beforeFilter($event);
            $this->viewBuilder()->layout('admin'); // For Version >= 3.1 or
            $this->layout = 'admin'; // for version < 3.1
            
            // your other code should be here
        }
    }

Set default layout **for specific action in application**. i.e, application have different layout in login page in /src/Template/Layout/login.ctp

    class UsersController extends Controller {

        public function login() {

            $this->viewBuilder()->layout('login'); // For Version >= 3.1 or
            $this->layout = 'login'; // for version < 3.1
            
            //your other code should be here
        }
    }

Change Layout **for specific Controller**. for i.e, you need different layout for all method of specific controller

class UsersController extends Controller {

        public function beforeFilter(Event $event) {
            parent::beforeFilter($event);

            $this->viewBuilder()->layout('user_layout'); // For Version >= 3.1 or
            $this->layout = 'user_layout'; // for version < 3.1
            
            //your other code should be here
        }
    }

## Set Ajax Request Layout
Generally in AJAX request no need of load CSS, JS. Also omitting other HTML code.

Make ajax.ctp file in /src/Template/Layout, and code should be

    <?php 
        $this->fetch('content');

Set AJAX based layout for entire application, in AppsController.php

class AppsController extends Controller {

    public function beforeFilter(Event $event) {
        parent::beforeFilter($event);
        if($this->request->isAjax())
        {
            $this->viewBuilder()->layout('ajax'); // For Version >= 3.1 or
            $this->layout = 'ajax'; // for version < 3.1

        }
        else
        {
            $this->viewBuilder()->layout('admin'); // For Version >= 3.1 or
            $this->layout = 'admin'; // for version < 3.1
        }
        
        // your other code should be here
    }
}


## Load Components in CakePHP
We can load components in two ways.
1. By initialize or override $components property in Controller
2. By using loadComponent() method in initialize() method of Controller.

**Way-1**
It should be override loading component by AppsController.php
load one or more component

    class UsersController extends AppController {
        public $components = ['RequestHandler','Auth','Flash']; 
    }

**Way-2**
Use this way when you need load component dynamically for specific controller.
Load One component

    class UsersController extends AppController {
        public function initialize() {
            parent::initialize();
            $this->loadComponent("RequestHandler"); // load specific component
            $this->loadComponent(["RequestHandler","Auth","Flash"]); // load specific component
        }
    }




## What is initilaize() method?
initialize() is introduced in CakePHP version > 3.0

As a code structure, it looks like same as beforeFilter() method. but there is many differences between beforeFilter() and initialize().

1. initialize() is always called after constructor is called. but beforeFilter() is not calling in case of action method not found in particular controller.
2. initialize() method is generally used for initialize something like add new components and helpers. but in beforeFilter() is generally used for execute some global logic part.

## Retrive query string data equivalent to $_GET
You can retrieve query string data as Array.

    $post_data= $this->request->query;

You can retrieve post data for particular key.

    $this->request->query['field'];

Retrieve specific key value

    $this->request->query('key_name');

Retrieve specific key value of nested array

    $this->request->query('data.subfield');



## Load another Model into Controller
By default CakePHP loads the related model in the controller. In order to load another model in the controller, use the loadModel() method:

    $this->loadModel('Articles');

or load on the fly

    $table = TableRegistry::get('Articles');
    $table->find();

## Creating Table(Model) Class
How to create User Model Class

    namespace App\Model\Table;
    use Cake\ORM\Table;
    
    class UsersTable extends Table {
        public function initialize(array $config) {
            $this->table('users'); //define table name
            $this->displayField('username'); // unique or other special field of users table
            $this->primaryKey('id'); // primary key of users table
            $this->tablePrefix('prefix_'); // if prefix set tablename should be prefix_users

            // your other code here
        }

        // your other methods here
    }

## Model Associations in CakePHP
There are 4 types of associations(relationships) we can define in CakePHP

    class PostsTable extends Table {
        public function initialize(array $config) {
    
            // table initialization code should be here
    
            $this->belongsTo('Authors', [
                'className' => 'Authors',
                'foreignKey' => 'author_id',
                'joinType' => 'INNER',
            ]);
            $this->hasMany('Tags');
            $this->hasOne('Categories');
            $this->hasAndBelongsToMany('Topics');
        }
    }

In above example, you can see 4 types of relationships

Post **belongs to** Author **(One to One)**, it means in `posts` table has one foreign key `author_id` which is associated with `id` of `authors` table.

Post **has many** Tags  **(One to Many)**, it means in `tags` table has one foreign key `post_id` which is associated with `id` of `posts` table.

Post **has one** Tags  **(Many to One or One to One)**, it means in `posts` table has one foreign key `category_id` which is associated with `id` of `categories` table.

Post **has and belongs to** Topics **(Many to Many)**, this is many to many relationship between `posts` and `topics` table. for maintain many to many relationship must need to create third table, table, table name should be `posts_categories`. Fields of this table as mentioned below
1. id (primary key of table)
2. post_id (foreign key of `posts` table)
3. topic_id (foreign key of `topics` table)





