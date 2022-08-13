---
title: "DataExtensions"
slug: "dataextensions"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Adding fields to a DataObject
You can use the `DataExtension` mechanism to add extra database fields to an existing DataObject:

    class MyMemberExtension extends DataExtension
    {
        private static $db = [
            'HairColour' => 'Varchar'
        ];
    }

And apply the extension:

    # File: mysite/_config/app.yml
    Member:
      extensions:
        - MyMemberExtension

This will add `HairColour` as a field to `Member` objects.

## Adding methods to a DataObject
You can add public methods to a DataObject using the extension mechanism, for example:

    class MyMemberExtension extends DataExtension
    {
        public function getHashId()
        {
            return sha1($this->owner->ID);
        }
    }

When applied to the `Member` class, the example above would return the `sha1` hash of the `Member` ID by accessing the `Member` via the protected property `$this->owner`. Eg:

    $member = Member::get()->byId(123);
    var_dump($member->getHashId()); // string(40) "40bd001563085fc35165329ea1ff5c5ecbdbbeef"

## Applying a DataExtension to a Class
The most common way is to apply the extension via Config. Example:

```yaml
# File: mysite/_config/config.yml
Member:
  extensions:
    - MyMemberExtension
```

The `extensions` config variable is of type "array", so you can add multiple extensions like this:

```yaml
# File: mysite/_config/config.yml
Member:
  extensions:
    - MyMemberExtension
    - MyOtherMemberExtension
```

If you wrote the class that is to be extended, you can define the extension(s) as static variable:

```php
<?php
class MyClass extends DataObject
{
    private static $extensions = ['MyCustomExtension'];
}
```

