---
title: "xscomplexType"
slug: "xscomplextype"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

A xs:complexType provides a description of an XML element's content in the instance document. The definition of the xs:complexType can be made globally in which case it has a name and can be re-used within the schema, or it can be inplace and only used within the context it is declared.



## Parameters

| Attributes | Description|
| --------- | ------ |
| abstract  | When set to true the complex type can not be used directly in an instance XML document via xsi:type. It can however be used as the base type for an element definition. (default false) - Only valid for root level xs:complexType's|
| block     | Limits the types that can be used in an XML instance document (defaults to the value of the xs:schemas blockDefault attribute if set, otherwise defaults to empty, values '#all' \| a list of ('extension', 'list', 'union') separated by whitespace).|
| final     | Limits deriving types from using this type in certain ways within the schema (defaults to the value of the xs:schemas finalDefault attribute if set, otherwise defaults to empty, values '#all' \| or a list of ('extension', 'list', 'union') separated by whitespace) - Only valid for root level xs:complexType's|
| id        | The id of the schema item (optional) |
| mixed     | Indicates the instance XML element may contain mixed content (defaults to false) |
| name      | The name of the xs:complexType - Only valid for root level xs:complexType's|
| any       | Any other attributes not in the 'http://www.w3.org/2001/XMLSchema' namespace are allowed. |
| ----------------- | ------ |
| **Elements**          | **Description**|
| ----------------- | ------ |
| xs:annotation     | Provides the ability to add documentation and machine readable data.|
| xs:simpleContent  | Used when the xs:complexType derives from a xs:simpleType.|
| xs:complexContent | Used when the xs:complexType derives from another xs:complexType. |
| xs:group          | Adds the elements from an xs:group to the xs:complexType definition|
| xs:all            | Adds the elements from an xs:all to the xs:complexType definition  |
| xs:choice         | Adds the elements from an xs:choice to the xs:complexType definition  |
| xs:sequence       | Adds the elements from an xs:sequence to the xs:complexType definition |
| xs:attribute      | Adds the xs:attribute to the xs:complexType definition |
| xs:attributeGroup | Adds the xs:attributeGroup to the xs:complexType definition   |
| xs:anyAttribute   | Adds the xs:anyAttribute   to the xs:complexType definition |










**Deriving from a xs:complexType**

When a xs:complexType derives from another xs:complexType is can do it via *extension* or *restriction*. 

 - extension - the deriving type takes everything defined in the base
   type and adds to it. 
 - restriction - the deriving type takes only
   selected parts from the base type, only allowing the parts it wants,
   no additional items can be added.


**Deriving from a xs:simpleType**

When a xs:complexType derives from a xs:simpleType is can do it via *extension*, in which case it can add attributes to the resulting type, but not elements. 


**Content Type**

Conceptually a xs:complexType either contains *simple* or *complex* content. If the xs:complexType derives from a typed based on xs:anySimpleType (xs:int, xs:string etc) then it is *simple*. If it derives from a xs:complexType which contains *complex* content, then it itself is *complex* (if the xs:complexType does not derive from a type, then it is also complex).




## Global ComplexType with Sequence and attributes
This example shows a simple global definition of a complexType. The definition is considered global as it is a child of the xs:schema. Globally defined types can be used elsewhere in the schema.

This is the most common form for declaring a global xs:complexType, it defines the child elements using a xs:sequence, xs:choice or xs:all, and optionally has attributes as well.

Note : because it is a globally defined it must have a unique name within the schema set.

    <?xml version="1.0" encoding="utf-8" ?>
    <!--Created with Liquid Studio 2017 (https://www.liquid-technologies.com)-->
    <xs:schema elementFormDefault="qualified" 
               xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:complexType name="PersonType">
            <xs:sequence>
                <xs:element name="Forename" type="xs:string" />
                <xs:element name="Surname" type="xs:string" />
            </xs:sequence>
            <xs:attribute name="Gender">
                <xs:simpleType>
                    <xs:restriction base="xs:string">
                        <xs:enumeration value="male" />
                        <xs:enumeration value="female" />
                    </xs:restriction>
                </xs:simpleType>
            </xs:attribute>
        </xs:complexType>
    </xs:schema>

[![ComplexType with Sequence and attributes][1]][1]

  [1]: https://i.stack.imgur.com/MownI.png

## Creating a global xs:complexType by extending an existing xs:complexType
In this example we are creating a new xs:complexType (EmployeeType) based on an existing xs:complexType (PersonType).

The construction of this is slightly more complicated. Because the base xs:complexType  (PersonType) is considered to be *complex* (more about this below) we add the <xs:complexContent> element. Then because we are extending PersonType, we add the element <xs:extension base="PersonType">. Within the xs:extension tag we can add a compositor (xs:all/xs:choice/xs:sequence) and any additional attributes.


    <?xml version="1.0" encoding="utf-8" ?>
    <!--Created with Liquid Studio 2017 (https://www.liquid-technologies.com)-->
    <xs:schema elementFormDefault="qualified" 
               xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:complexType name="PersonType">
            <xs:sequence>
                <xs:element name="Forename" type="xs:string" />
                <xs:element name="Surname" type="xs:string" />
            </xs:sequence>
            <xs:attribute name="Gender">
                <xs:simpleType>
                    <xs:restriction base="xs:string">
                        <xs:enumeration value="male" />
                        <xs:enumeration value="female" />
                    </xs:restriction>
                </xs:simpleType>
            </xs:attribute>
        </xs:complexType>
        <xs:complexType name="EmployeeType">
            <xs:complexContent>
                <xs:extension base="PersonType">
                    <xs:sequence>
                        <xs:element name="Salary" type="xs:decimal" />
                    </xs:sequence>
                    <xs:attribute name="EmployeeID" type="xs:int" use="required" />
                </xs:extension>
            </xs:complexContent>
        </xs:complexType>
    </xs:schema>

[![Creating a global xs:complexType by extending an existing xs:complexType][1]][1]


  [1]: https://i.stack.imgur.com/yVUGi.png

## Creating a global xs:complexType by restricting an existing xs:complexType
This is where things get a little tricky. We are now restricting an existing xs:complexType. Our SolidStateDriveType derives from HardDiskType but removes the spinUpTime attribute and the RotationSpeed element. 

Notice the approach for dealing with attributes and elements is different. To remove an attribute you need to re-declare it and set its *use* to *prohibited*. For elements simply not re-declaring them will cause them to be excluded, in fact you need to re-declare any elements you want to keep in the new type.

***Key concept for restricted types** : It must be possible to load an XML instance element resulting from the restricted type into the base type, put another way the restricted type needs to be able to 'fit' into the base type. So you can not exclude a mandatory attribute or element, in order to exclude it in the restricted type it must be optional in the base type. If you change the type/facet rules of an element or attribute in the restricted type, the new type/facet rules must be compatible with the base type, so if the base type was a short, the restricted type could be a byte, but not a long.*


    <?xml version="1.0" encoding="utf-8" ?>
    <!--Created with Liquid Studio 2017 - Developer Bundle Edition (Trial) 15.0.2.7192 (https://www.liquid-technologies.com)-->
    <xs:schema elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">
        <xs:complexType name="HardDiskType">
            <xs:sequence>
                <xs:element name="Capacity" type="xs:long" />
                <xs:element name="RotationSpeed" type="xs:int" minOccurs="0" />
            </xs:sequence>
            <xs:attribute name="name" type="xs:string" />
            <xs:attribute name="spinUpTime" type="xs:time" />
        </xs:complexType>
        <xs:complexType name="SolidStateDrive">
            <xs:complexContent>
                <xs:restriction base="HardDiskType">
                    <xs:sequence>
                        <xs:element name="Capacity" type="xs:long" />
                    </xs:sequence>
                    <xs:attribute name="spinUpTime" use="prohibited" />
                </xs:restriction>
            </xs:complexContent>
        </xs:complexType>
    </xs:schema>

[![Creating a global xs:complexType by restricting an existing xs:complexType][1]][1]


  [1]: https://i.stack.imgur.com/oe6dc.png

