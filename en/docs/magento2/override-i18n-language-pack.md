---
title: "Override i18n language pack"
slug: "override-i18n-language-pack"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Syntax
- **&lt;Vendor Namespace&gt;** - Here namespace of the vendor custom theme or inbuilt theme namespace **I.E.** `Magento/Luma` Here `luma` is `vendor namespace`

- **&lt;language package directory&gt;** - Here language package directory like `en_us` or `nl_nl` or `en_gb`

- **&lt;language package description&gt;** - Here add description of the package like `English Us Package`

- **&lt;language package code&gt;** - Here code of the language package **I.E** `en_US` or `nl_NL` or `en_GB`

After create above files and directories `language_package_code.csv` will goes to `Vendor Namespace` directory

**Example** 

`/app/i18n/luma/en_us/en_US.csv`

or 

`/app/i18n/luma/en_gb/en_GB.csv`

or

`/app/i18n/luma/nl_NL/nl_NL.csv`



## Syntax example of override i18n language package
**/app/i18n/&lt;Vendor Namespace&gt;/&lt;language package directory&gt;/composer.json**

    {
        "name": "<vendor namespance>/<language package directory>",
        "description": "<language package description>",
        "version": "100.0.1",
        "license": [
            "OSL-3.0",
            "AFL-3.0"
        ],
        "require": {
            "magento/framework": "100.0.*"
        },
        "type": "magento2-language",
        "autoload": {
            "files": [
                "registration.php"
            ]
        }
    }

**/app/i18n/&lt;Vendor Namespace&gt;/&lt;language pack&gt;/language.xml**

    <?xml version="1.0"?>
    <language xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="urn:magento:framework:App/Language/package.xsd">
        <code><language package code></code>
        <vendor><vendor namespace></vendor>
        <package><language package directory></package>
    </language>

**/app/i18n/&lt;Vendor Namespace&gt;/&lt;language pack&gt;/registration.php**

    <?php
    \Magento\Framework\Component\ComponentRegistrar::register(
        \Magento\Framework\Component\ComponentRegistrar::LANGUAGE,
        '<vendor namespance>_<language package directory>',
        __DIR__
    );

