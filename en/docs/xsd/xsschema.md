---
title: "xsschema"
slug: "xsschema"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Describes for elements, attributes and types that are valid in an XML instance document. 
An XML Schema (XSD) must contain a single root level xs:schema element. 

## Parameters
| Attributes | Description |
| ---------- | ------------------- |
| attributeFormDefault  | Indicates whether attributes in the XML instance document have to be qualified with a namespace (default unqualified) |
| blockDefault          | The default value of the *block* attribute that is applied to [xs:complexType][1] and xs:element. Defines the rules for blocking derivation/substituion in the instance document (default empty, i.e. block nothing) |
| defaultAttributes     | **(XSD 1.1)** Specifies an xs:attributeGroup that will be associated with all [xs:complexType][1] and xs:element within the schema (optional). |
| elementFormDefault    | Indicates whether element in the XML instance document have to be qualified with a namespace (default unqualified). **Note** : almost without exception all schemas set this to '*qualified*'. |
| finalDefault          | The default *final* attribute value used in [xs:complexType][1] and xs:element. Defines the rules for blocking derivation/substituion in the schema  (default empty, i.e. block nothing)  |
| id                    | The id of the schema item (optional) |
| targetNamespace       | Qualifies all the elements and attributes (and globally defined components) defined within this schema. |
| version               | The version of the schema, this is the version of the document, not this XSD version (i.e. Soap 1.2, FpML 4.2 etc) |
| xpathDefaultNamespace | **(XSD 1.1)** The default value for the the attribute *xpathDefaultNamespace* which is used in xs:selector, xs:field, xs:alternative & xs:assert (specifies the default namespace to be used in XPath expressions) |
| any                   | Any other attributes not in the 'http://www.w3.org/2001/XMLSchema' namespace are allowed. |
| **Elements**          | **Description** |
| xs:annotation         | Provides the ability to add documentation and machine readable data. |
| xs:include            | Used to include a schema with the same targetNamespace, or no targetNamespace (see chameleon schemas). |
| xs:import             | Used to include a schema with a targetNamespace different to the parent. |
| xs:redefine           | Used to include a schema with the same targetNamespace (or no targetNamespace), and modify xs:simpleType, xs:complexType, xs:group or xs:attributeGroup definitions contained within it (here be dragons....)|
| xs:simpleType         | Defines a global (named) simple type which can then be referenced and re-used. |
| [xs:complexType][1]   | Defines a global (named) complex type which can then be referenced and re-used. |
| xs:group              | Defines a global (named) group of elements which can then be referenced and re-used. |
| xs:attributeGroup     | Defines a global (named) group of attributes which can then be referenced and re-used. |
| xs:attribute          | Defines a global (named) attribute which can then be referenced and re-used. |
| xs:element            | Defines a global (named) element which can then be referenced and re-used, or used as the basis of an XML instance document. |
| xs:notation           | \- |
| xs:defaultOpenContent | **(XSD 1.1)** Specifies rules for allowing additional elements to be permitted within every [xs:complexType][1] and xs:element within the schema. | 

  [1]: https://www.wikiod.com/xsd/xscomplextype

## Basic xs:schema
Shows a very basic schema.

**Note:** by convention elementFormDefault is set to '*qualified*', in the really world you will be hard pressed to find a schema that does not set this (so just include it in your schemas!).

    <?xml version="1.0" encoding="utf-8" ?>
    <!--Created with Liquid Studio 2017 - (https://www.liquid-technologies.com)-->
    <xs:schema elementFormDefault="qualified" 
               xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:element name="Person">
            <xs:complexType>
                <xs:sequence>
                    <xs:element name="Forename" type="xs:string" />
                    <xs:element name="Surname" type="xs:string" />
                </xs:sequence>
            </xs:complexType>
        </xs:element>
    </xs:schema>

## xs:schema elementFormDefault attribute
By convention elementFormDefault is always set to *qualified*, but lets look at what it actually does.


----------


First with elementFormDefault set to qualified. 

    <?xml version="1.0" encoding="utf-8" ?>
    <!--Created with Liquid Studio 2017 (https://www.liquid-technologies.com)-->
    <xs:schema elementFormDefault="qualified" 
               targetNamespace="http://base.com" 
               xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:element name="MyBaseElement">
            <xs:complexType>
                <xs:sequence>
                    <xs:element name="ChildA" type="xs:string" />
                </xs:sequence>
            </xs:complexType>
        </xs:element>
    </xs:schema>

Sample XML Document

    <?xml version="1.0" encoding="utf-8"?>
    <!-- Created with Liquid Studio 2017 (https://www.liquid-technologies.com) -->
    <b:MyBaseElement xmlns:b="http://base.com" 
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="http://base.com ElementFormDefault_qualified.xsd">
        <b:ChildA>string</b:ChildA>
    </b:MyBaseElement>

Notice the element ChildA must also be qualified with the namespace 'b'.


----------

Now lets look at it with elementFormDefault set to unqualified. 

    <?xml version="1.0" encoding="utf-8" ?>
    <!--Created with Liquid Studio 2017 (https://www.liquid-technologies.com)-->
    <xs:schema elementFormDefault="unqualified" 
               targetNamespace="http://base.com" 
               xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:element name="MyBaseElement">
            <xs:complexType>
                <xs:sequence>
                    <xs:element name="ChildA" type="xs:string" />
                </xs:sequence>
            </xs:complexType>
        </xs:element>
    </xs:schema>

Sample XML Document

    <?xml version="1.0" encoding="utf-8"?>
    <!-- Created with Liquid Studio 2017 (https://www.liquid-technologies.com) -->
    <b:MyBaseElement xmlns:b="http://base.com" 
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="http://base.com ElementFormDefault_unqualified.xsd">
        <ChildA>string</ChildA>
    </b:MyBaseElement>

Notice this time that only the globally defined element MyBaseElement is qualified with the namespace 'b', the inner element ChildA (which is defined in place within the schema) is not qualified.


----------

In the last example we saw that the globally defined elements must be qualified in the XML instance document, but elements defined inplace do not. But this does not just mean the root element, if you have globally defined elements that are referenced, then they need qualifying as well.

    <?xml version="1.0" encoding="utf-8" ?>
    <!--Created with Liquid Studio 2017 (https://www.liquid-technologies.com)-->
    <xs:schema elementFormDefault="unqualified" 
               targetNamespace="http://base.com" 
               xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:element name="MyBaseElement">
            <xs:complexType>
                <xs:sequence>
                    <xs:element name="ChildA" type="xs:string" />
                    <xs:element xmlns:q1="http://base.com" ref="q1:MyElement" />
                </xs:sequence>
            </xs:complexType>
        </xs:element>
        <xs:element name="MyElement" type="xs:string" />
    </xs:schema>

Sample XML Document

    <?xml version="1.0" encoding="utf-8"?>
    <!-- Created with Liquid Studio 2017 (https://www.liquid-technologies.com) -->
    <b:MyBaseElement xmlns:b="http://base.com" 
                     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                     xsi:schemaLocation="http://base.com ElementFormDefault_unqualified.xsd">
        <ChildA>string</ChildA>
        <b:MyElement>string</b:MyElement>
    </b:MyBaseElement>

Notice that MyElement also need qualifiying as it is globally defined.


----------

In conclusion, if you have elementFormDefault set to qualified, then everything needs to be qualified with a namespace (either via a namespace alias or by setting the default namesapce xmlns="..."). However elementFormDefault is set to unqualified, things get complicated and you need to do some quite indepth examination of the schemas to work out if things should be qualified or not. 

I assume that is why elementFormDefault is always set to qualified!


