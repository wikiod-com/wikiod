---
title: "Doctypes"
slug: "doctypes"
draft: false
images: []
weight: 9830
type: docs
toc: true
---

Doctypes - short for 'document type' - help browsers to understand the version of HTML the document is written in for better interpretability. Doctype declarations are not HTML tags and belong at the very top of a document. This topic explains the structure and declaration of various doctypes in HTML.

## Syntax
- \<!DOCTYPE [version-specific string]\>

The `<!DOCTYPE>` declaration is not an HTML tag. It is used for specifying which version of HTML the document is using. This is referred to as the document type declaration (DTD).

The `<!DOCTYPE>` declaration is NOT case sensitive. To check if the HTML of your Web pages is valid, go to [W3C's validation service][1].
- Some old versions of IE don't support some HTML tags unless a proper doctype is available.
- It's _vital_ that a doctype is declared as to make sure the browser doesn't use quirks mode. [More info on MDN.](https://developer.mozilla.org/en-US/docs/Quirks_Mode_and_Standards_Mode)


  [1]: http://validator.w3.org/

## Adding the Doctype
The `<!DOCTYPE>` declaration should always be included at the top of the HTML document, before the `<html>` tag.

<!-- if version [gte 5] -->

See https://www.wikiod.com/html/doctypes#HTML 5 Doctype for details on the HTML 5 Doctype.

---

    <!DOCTYPE html>

<!-- end version if -->

<!-- if version [eq 4.01] -->

See https://www.wikiod.com/html/doctypes#HTML 4.01 Doctypes for details on how these types differ from each other.

---

**Strict**

    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">

---

**Transitional**

    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

---

**Frameset**

    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN" "http://www.w3.org/TR/html4/frameset.dtd">

<!-- end version if -->


## HTML 5 Doctype
HTML5 is not based on SGML, and therefore does not require a reference to a DTD.

HTML 5 Doctype declaration:

    <!DOCTYPE html>

# Case Insensitivity

Per the [W3.org HTML 5 `DOCTYPE` Spec](https://www.w3.org/TR/html5/syntax.html#the-doctype):

> A DOCTYPE must consist of the following components, in this order:
>
> 1. A string that is an ASCII **case-insensitive** match for the string `"<!DOCTYPE"`.

therefore the following `DOCTYPE`s are also valid:

    <!doctype html>
    <!dOCtyPe html>
    <!DocTYpe html>

This SO article discusses the topic extensively: http://stackoverflow.com/questions/7020961/uppercase-or-lowercase-doctype

## HTML 4.01 Doctypes
The HTML 4.01 specification provides several different types of doctypes that allow different types of elements to be specified within the document.

### HTML 4.01 Strict ###

    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">

Includes all HTML elements and attributes, but **does not include presentational or deprecated elements** and **framesets are not allowed**.

### HTML 4.01 Transitional ###

    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

Includes all HTML elements and attributes and presentational and deprecated elements, but **framesets are not allowed**.

### HTML 4.01 Frameset ###

    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN" "http://www.w3.org/TR/html4/frameset.dtd">

Includes all HTML elements and attributes, presentational and deprecated elements. Framesets are allowed. 


## Old Doctypes
### HTML 3.2

    <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">

HTML 3.2 is well supported by most browsers in use. However, HTML 3.2 has limited support for style sheets and no support for HTML 4 features such as frames and internationalization.

---

### HTML 2.0

    <!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">

HTML 2.0 is widely supported by browsers but lacks support for tables, frames, and internationalization, as well as many commonly used presentation elements and attributes.

