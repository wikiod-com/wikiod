---
title: "How to activate OpenERP Developer Mode"
slug: "how-to-activate-openerp-developer-mode"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

**Developer Mode**

Odoo developer mode allows you to make substantial modifications to the Odoo database such as adding fields to your documents and views. You change the default views of your actions and can even create dynamic forms based on other fields within your models.

**Advantage**

While Odoo is a powerful application framework the development cycle can be brutal to test changes to your application. By utilizing the developer mode you can test expressions and solve many functional problems without having to restart the server over and over to test simple changes.

Additionally the Odoo developer tool is great for looking at the architecture of forms and views to see how fields are tied to modules, their domains, contexts and other attributes. In this video we explore exactly how we put these tools to use in modifying and creating Odoo applications.

**Limitations**

While it can be very tempting to use developer mode to make a great deal of changes to your application there are some drawbacks. Depending on what you modify and change you can lose these changes with future module updates or when you install additional applications into Odoo. This is particularly true for changes to views.

> To activate developer mode you just simply write down
> 
> for version v7
> 
> &debug=
> 
> before # sign you just add it.
> 
> http://localhost:8069/?db=test_db&debug=#
> 
> for version > v7
> 
> http://localhost:8069/web?debug=

You may not see **About Odoo** menu because there might be **odoo debranding module** installed.

## Activate developer mode in Odoo 10
Activate Developer Mode:

 1. Login to odoo application.
 2. After login user may see several odoo menu's. Click on setting menu.

[![enter image description here][1]][1]

 1. Click on 'Activate the developer mode' which is located at right-bottom corner of settings page.
 2. Developer mode now activated.

[![enter image description here][2]][2]




  [1]: https://i.stack.imgur.com/JjM8R.png
  [2]: https://i.stack.imgur.com/Wj01f.png

## Activating developer mode in Odoo 8
When you logged into Odoo application you will find an option to see who is the current logged in person in the top right corner. This user information have a dropdown button. Click on the dropdown, then you will find a list. In that list select about Odoo.com option. Clicking on that will open a **About** popup window. In that window, over the top right corner you will find an option like **Acivate developer mode**. Clicking on that link will reload the webpage. 

After reloading it will be in Developer mode. Then the link will change to something like this *http://localhost:8069/web?debug=#id=23&view_type=form&model=res.partner*


## Activate developer mode

To activate the developer mode:

 1. Log in to the ODOO front end
 2. Click on the User Name drop down at the top-right side
 3. Select 'About'
 4. Click on 'Activate developer mode' from the pop-up window.

[![Image for step 3][1]][1]

[![Image for step 4][2]][2]



  [1]: http://i.stack.imgur.com/uYtrP.png
  [2]: http://i.stack.imgur.com/GIXrV.png

