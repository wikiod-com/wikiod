---
title: "Dependency Injection"
slug: "dependency-injection"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Class Preference
    <!-- <moduleDir>/etc/<area>/di.xml -->
    <config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:ObjectManager/etc/config.xsd">
    <!-- ... -->
        <preference
            for="Vendor\Namespace\Model\Example"
            type="Vendor\Namespace\Model\AnotherExample" />
    <!-- ... -->
    </config>

Above Example is a syntax of override core model.

Here is a list of points which will describe you how to make it possible

1. **moduleDir** - Extension directory Like `app/code/custom/extension` here `extension` is your directory in which all the necessary folders of extension will be placed.

2. **area** - area will be `frontend` or `adminhtml`

   - **frontend** - if extension will use functionality of frontend than `di.xml` will goes to in this folder

   - **adminhtml** - if extension will use functionality of adminpanel than `di.xml` will goes to in this folder

   - so it will be `app/code/custom/extension/etc/frontend/di.xml` or `app/code/custom/extension/etc/adminhtml/di.xml`

   - If wants to use both the functionality than `di.xml` file will goes direct in `etc` folder no need to put in `frontend` or `adminhtml` folder. Like - `app/code/custom/extension/etc/di.xml`

3. **for="Vendor\Namespace\Model\Example"** at here, the path of the file which will override functionality of the desired function.

4. **type="Vendor\Namespace\Model\AnotherExample"** at here, the path of the file which will provides functions  which will override by `step - 3`




## Argument Replacement
    <!-- <moduleDir>/etc/<area>/di.xml -->
    <config xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:ObjectManager/etc/config.xsd">
    <!-- ... -->
        <type name="Vendor\Namespace\Model\SomeClass">
            <arguments>
                <argument name="object" xsi:type="object">Vendor\Namespace\Model\SomeOtherClass</argument>
            </arguments>
        </type>
    </config>

## Constructor Injection
    /**
     * @var \Vendor\Module\Helper\Data
     */
    protected $customHelper;

    /**
     * Constructor call
     * @param \Vendor\Module\Helper\Data $customHelper
     */
    public function __construct(
        \Vendor\Module\Helper\Data $customHelper
    )
    {
        $this->customHelper = $customHelper;
        parent::__construct();
    }



