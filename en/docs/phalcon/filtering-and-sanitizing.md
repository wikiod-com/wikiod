---
title: "Filtering and Sanitizing"
slug: "filtering-and-sanitizing"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Convenient in-model sanitizing
Set a convenience method in your base model
```
namespace Base;

class Model extends \Phalcon\Mvc\Model
{
    public function sanitize($attr, $filterName)
    {
        $filter = $this->getDI()->get('filter');
        $this->$attr = $filter->sanitize($this->$attr, $filterName);
    }
}
```
Then use like so
```
class User extends \Base\Model
{
    public function beforeValidation()
    {
        $this->sanitize('id', 'int');
        // input $this->id: 123abc
        // output: 123

        $this->sanitize('email', 'email');
        // input $this->email: youre(-)mail@dom/ain.com
        // output: youremail@domain.com
        
        $this->sanitize('wage', 'float');
        // input $this->wage: +1234ab.56cd
        // output: 1234.56

        $this->sanitize('name', 'string');
        // input $this->name: <john>
        // output: john
    }
}

