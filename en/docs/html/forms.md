---
title: "Forms"
slug: "forms"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

In order to group input elements and submit data, HTML uses a form element to encapsulate input and submission elements. These forms handle sending the data in the specified method to a page handled by a server or handler. This topic explains and demonstrates the usage of HTML forms in collecting and submitting input data.

## Syntax
- `<form method="post|get" action="somePage.php" target="_blank|_self|_parent|_top|framename">`

## Parameters
| Attribute&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; | Description |
|-----------|-------------|
| `accept-charset` | Specifies the character encodings that are to be used for the form submission. |
| `action` | Specifies where to send the form-data when a form is submitted. |
| `autocomplete` | Specifies whether a form should have autocomplete on or off. |
| `enctype` | Specifies how the form-data should be encoded when submitting it to the server (only for method="post"). |
| `method` | Specifies the HTTP method to use when sending form-data (POST or GET). |
| `name` | Specifies the name of a form. |
| `novalidate` | Specifies that the form should not be validated when submitted. |
| `target` | Specifies where to display the response that is received after submitting the form. |

The `<form>` element represents a section that contains form-associated elements (e.g. `<button>` `<fieldset>` `<input>` `<label>` `<output>` `<select>` `<textarea>`) that submits information to a server. Both starting (`<form>`) and ending (`</form>`) tags are required.



## Submitting
The Action Attribute
--------------------
The action attribute defines the action to be performed when the form is submitted, which usually leads to a script that collects the information submitted and works with it. if you leave it blank, it will send it to the same file

    <form action="action.php">

The Method Attribute
--------------------

The method attribute is used to define the HTTP method of the form which is either GET or POST.

    <form action="action.php" method="get">
    <form action="action.php" method="post">


The GET method is mostly used to _get_ data, for example to receive a post by its ID or name, or to submit a search query. The GET method will append the form data to the URL specified in the action attribute.

    www.example.com/action.php?firstname=Mickey&lastname=Mouse

The POST method is used when submitting data to a script. The POST method does not append the form data to the action URL but sends using the request body.

To submit the data from the form correctly, a name attribute name must be specified.  
As an example let's send the value of the field and set its name to _lastname_:

    <input type="text" name="lastname" value="Mouse">


## More attributes ##

    <form action="action.php" method="post" target="_blank" accept-charset="UTF-8" 
    enctype="application/x-www-form-urlencoded" autocomplete="off" novalidate>

    <!-- form elements -->

    </form>


## Target attribute in form tag
The target attribute specifies a name or a keyword that indicates where to display the response that is received after submitting the form.

The target attribute defines a name of, or keyword for, a browsing context (e.g. tab, window, or inline frame).

From Tag with a target attribute:

    <form target="_blank">

------------------------------------

**Attribute Values**

<table> 
  <tbody><tr>
    <th>Value</th>
    <th>Description</th>
  </tr>  
  <tr>
    <td>_blank</td>
    <td>The response is displayed in a new window or tab</td>
  </tr>
  <tr>
    <td>_self</td>
    <td>The response is displayed in the same frame (this is default)</td>
  </tr>
    <tr>
    <td>_parent</td>
    <td>The response is displayed in the parent frame</td>
  </tr>
    <tr>
    <td>_top</td>
    <td>The response is displayed in the full body of the window</td>
  </tr>
  <tr>
    <td><i>framename</i></td>
    <td>The response is displayed in a named iframe</td>
  </tr>
  </tbody>
</table>

> Note: The target attribute was ***deprecated*** in **HTML 4.01**. The target
> attribute is ***supported*** in ***HTML5***.
> 
> Frames and framesets are not supported in ***HTML5***, so the ***_parent, _top
> and framename values are now mostly used with iframes***.

## Uploading Files
Images and files can be uploaded/submitted to server by setting `enctype` attribute of `form` tag to `multipart/form-data`.
`enctype` specifies how form data would be encoded while submitting to the server.

**Example**

    <form method="post" enctype="multipart/form-data" action="upload.php"> 
        <input type="file" name="pic" />
        <input type="submit" value="Upload" />
    </form>
    
    

## Grouping a few input fields
While designing a form, you might like to group a few input fields into a group to help organise the form layout. This can be done by using the tag <fieldset>. Here is an example for using it.

For each fieldset, you can set a legend for the set using the tag <legend>LEGEND TEXT</legend>

**Example**

    <form>
      <fieldset>
         <legend>1st field set:</legend>
         Field one:<br>
         <input type="text"><br>
         Field two:<br>
         <input type="text"><br>
      </fieldset><br>
      <fieldset>
         <legend>2nd field set:</legend>
         Field three:<br>
         <input type="text"><br>
         Field four:<br>
         <input type="text"><br>
      </fieldset><br>
      <input type="submit" value="Submit">
    </form>

**Result**

[![The result for the above code][1]][1]


**Browser Support**

Chrome, IE, Edge, FireFox, Safari and Opera's latest versions also supports the tag <fieldset>

  [1]: https://i.stack.imgur.com/VxCxp.png

