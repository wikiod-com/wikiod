---
title : xsd Tutorial
slug : xsd-tutorial
weight : 9989
draft : false
images : []
type : docs
---

XML Schema is a language and framework for validating XML documents.

An XML document that is **well-formed**, in the sense that it is syntactically conformant to the XML specification, can be tested for **validity** against a schema. The distinction between well-formedness, which is absolute, and validity, which is relative to a schema, is paramount.

Validation encompasses:
- Checking whether the XML document fulfils additional requirements such as the elements having certain names, restrictions on the content of elements, consistency constraints (primary keys, uniqueness, etc), attribute values or text matching certain types.
- Upon success, conversion of the input data model instance (called XML Infoset) to an output instance (PSVI: Post-Schema-Validation Infoset), where elements and attributes are annotated with type information, where default values have been populated, etc.

XML Schema was introduced to address requirements that DTD validation failed to address, among others a more complete type system including a rich set of builtin types, type restriction and extension capabilities, and more control on the restriction of element layout.

