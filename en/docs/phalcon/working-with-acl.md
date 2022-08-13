---
title: "Working with ACL"
slug: "working-with-acl"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Syntax
 - You can use '*' as second and third parameter in `Phalcon\Acl::allow` and `Phalcon\Acl::deny` methods. This will mean any resource and action respectively.
 - Second argument in `Phalcon\Acl::addRole` tells from which role inheritance access.

- You should serialize your ACL to some file or cache backend instead of creating it on each request.
- Also it's good idea to keep acl in seperated file.
- `Phalcon\Acl` is able to send events to event manager, there are two events - `beforeCheckAccess` and `afterCheckAccess`.
- You can use `Phalcon\Acl\AdapterInterface ` to implement your own acl adapter.
- You can protect your routes using acl with combination of proper listener in dispatcher

## Creating an ACL
You can create ACL by using `Phalcon\Acl\Adapter\Memory` class:

```
$acl = new Phalcon\Acl\Adapter\Memory();
```

By default phalcon allows action to resource which has not been defined, to change this you can use:

```
$acl->setDefaultAction(Phalcon\Acl::DENY);
```

Roles can be added in two ways - using `Phalcon\Acl\Role` or just plain string:

```
$roleAdministrator = new Phalcon\Acl\Role('Administrator');
$acl->addRole($roleAdministrator);
$acl->addRole('Customer');
```

Resources can be added in two ways too, you can add actions as single action or as array:

```
$resourceCategories = new Phalcon\Acl\Resource('categories');
$acl->addResource($resourceCategories, 'create');
$acl->addResource('products', ['create', 'update']);
```

## Defining Access Control and querying an ACL
You can allow role to access some action on resource by:

```
$acl->allow('Administrator', 'products', 'create');
```

You can deny role to access some action on resource by:

```
$acl->deny('Customer', 'categories', 'create');
```

You can check if role is allowed to some action on resource by using:

```
$acl->isAllowed('Administrator', 'products', 'create');
```

## Objects as roles and resources
By implementing `Phalcon\Acl\RoleAware` or `Phalcon\Acl\ResourceAware` you can use them as objects in `Phalcon\Acl\Adapter\Memory::isAllowed()`.

```
// Create our class which will be used as roleName
class UserRole implements Phalcon\Acl\RoleAware
{
    protected $id;
    protected $roleName;

    public function __construct($id, $roleName)
    {
        $this->id = $id;
        $this->roleName = $roleName;
    }

    public function getId()
    {
        return $this->id;
    }

    // Implemented function from RoleAware Interface
    public function getRoleName()
    {
        return $this->roleName;
    }
}
```

```
// Create our class which will be used as resourceName
class ModelResource implements Phalcon\Acl\ResourceAware
{
    protected $id;
    protected $resourceName;
    protected $userId;

    public function __construct($id, $resourceName, $userId)
    {
        $this->id = $id;
        $this->resourceName = $resourceName;
        $this->userId = $userId;
    }

    public function getId()
    {
        return $this->id;
    }

    public function getUserId()
    {
        return $this->userId;
    }

    // Implemented function from ResourceAware Interface
    public function getResourceName()
    {
        return $this->resourceName;
    }
}
```
```
$customer = new ModelResource(1, "products", 2);
$administrator = new UserRole(1, "Administrator");
$acl->isAllowed($administrator, $customer, 'create');
```
Also ability to use objects can be combined with additional condition in acl:
```
$acl->allow('Administrator', 'products', 'update', function(UserRole $user, ModelResource $model) {
    return $user->getId == $model->getUserId();
});
$product = new ModelResource(1, 'products', 2);
$administrator = new UserRole(1, 'Administrator');
$anotherAdministrator = new UserRole(2, 'Administrator');
$acl->isAllowed($administrator, $product, 'update'); // this will return false
$acl->isAllowed($anotherAdministrator, $product, 'update'); // this will return true
```
Notice that with additional condition and using objects in `isAllowed` method you don't need to pass those objects as arguments. They are passed automatically only if there are correct types before arguments in function. This gives you huge ability to control if certain users can edit for example certain models in your application and when they can do it.

## Additional condition in ACL
You can add also add some more logic which has to be checked to your ACL using anonymous functions. They will be executed when using `Phalcon\Acl\Adapter\Memory::allow()` or `Phalcon\Acl\Adapter\Memory::deny()`, if they will return true, they role will be allowed to access certain action on resource.

```
$acl->allow('Customer', 'products', 'create', function($parameter) {
    return $parameter % 2 == 0;
});
$acl->isAllowed('Customer', 'products', 'create', ['parameter' => 1]); // this will return false
$acl->isAllowed('Customer', 'products', 'create', ['parameter' => 2]); // this will return true
```

Notice how parameters are passed to function. Your key in array needs to have the same name as in function. Also default parameters parameters can be passed, as well as objects.

