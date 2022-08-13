---
title: "Add CSS and Javascript files to Odoo module"
slug: "add-css-and-javascript-files-to-odoo-module"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Syntax
 - Note about XML syntax: As the record is made inside of XML file, you can not leave any tag unclosed as you could in a plain HTML, like: &lt;link rel='stylesheet' href="..." **&gt;**, Close the link tag instead, like:
   - &lt;link rel='stylesheet' href="..." **/**&gt;  

## Parameters
|Possible values of *inherit_id* parameter|meaning|
|-------------------|-------|
|*web.assets_backend*|Used in internal pages only, NOT included in a public website.|
|*website.assets_frontend*|Used in a public website only (via "*website*" module).|
|*web.assets_common*|Used in both, public website and internal pages.|

If you are not sure about which option is suitable for you, then try the first option (backend) as it is used in most cases and nearly in all cases if you have not installed the "website" module. Odoo differentiates between "backend" and "frontend" assets because the public website provided by the "website" module uses different styling and JS code than internal pages meant to be used for ERP tasks, i.e. "frontend" is associated with the public website and "backend" is associated with internal pages for ERP (meaning of "frontend" and "backend" are Odoo specific here, but they are both "frontend" in more general sense).  

You can not only choose and use one of the options, but also use any combination of them (two of them or all of them) in the same module. Factor a backend, a frontend and a common JS/CSS code into separated files to better adhere to DRY and have suitable code in the public website and in the internal pages.

Do not forget to add "web" (when using *option 1*) or "website" (when using *option 2*) to the dependency list in the `__openerp__.py` manifest.

## Store CSS and JS files correctly in Odoo module
CSS and JS files should be reside under 'static' directory in the root directory of module (the rest of subdirectory tree under 'static' is an optional convention):

 - static/src/css/**your_file.css**
 - static/src/js/**your_file.js**

Then add links to these files unsing one of the 3 ways listed in the following examples.

## Option 1: [BACKEND] Add CSS and Javascript files to use in internal pages
Odoo v8.0 way is to add corresponding record in the XML file:
 - ​Add XML file to the manifest (i.e. `__openerp__.py` file.):

    ...  
    'data' : [ 'your_file.xml' ],  
    ​...  

 - Then add following record in `'your_file.xml'`:
<pre>
    &lt;openerp&gt;
        &lt;data&gt;  
            &lt;template id="assets_backend" name="<strong>your_module_name</strong> assets" inherit_id="web.assets_backend"&gt;  
                &lt;xpath expr="." position="inside"&gt;  
                    &lt;link rel='stylesheet' href="/<strong>your_module_name</strong>/static/src/css/<strong>your_file.css</strong>"/&gt;  
                    &lt;script type="text/javascript" src="/<strong>your_module_name</strong>/static/src/js/<strong>your_file.js</strong>"&gt;&lt;/script&gt;  
                &lt;/xpath&gt;  
            &lt;/template&gt;  
        ....  
        ....  
        &lt;/data&gt;  
     &lt;/openerp&gt;  
</pre>


## Option 2: [FRONTEND] Add CSS and Javascript files to use in a public website
Note: you should use this way if you've installed a "website" module and you have a public website available.


 - Add following record in `'your_file.xml'`:
<pre>
    &lt;openerp&gt;
        &lt;data&gt;  

            &lt;template <i>id="assets_frontend"</i> name="<strong>your_module_name</strong> assets" <i>inherit_id="website.assets_frontend"</i>&gt;  
                &lt;xpath expr="link[last()]" position="after"&gt;  
                    &lt;link rel='stylesheet' href="/<strong>your_module_name</strong>/static/src/css/<strong>your_file.css</strong>"/&gt;  
                &lt;/xpath&gt;  
                &lt;xpath expr="script[last()]" position="after"&gt;  
                    &lt;script type="text/javascript" src="/<strong>your_module_name</strong>/static/src/js/<strong>your_file.js</strong>"&gt;&lt;/script&gt;  
                &lt;/xpath&gt;  
            &lt;/template&gt;  

        &lt;/data&gt;  
     &lt;/openerp&gt;  
</pre>


## Option 3: [COMMON] Add CSS and Javascript files to use in all pages (backend & frontend)

 - Add following record in `'your_file.xml'`:
<pre>
    &lt;openerp&gt;
        &lt;data&gt;  

            &lt;template id="assets_common" name="<strong>your_module_name</strong> assets" inherit_id="web.assets_common"&gt;  
                &lt;xpath expr="." position="inside"&gt;  
                    &lt;link rel='stylesheet' href="/<strong>your_module_name</strong>/static/src/css/<strong>your_file.css</strong>"/&gt;  
                    &lt;script type="text/javascript" src="/<strong>your_module_name</strong>/static/src/js/<strong>your_file.js</strong>"&gt;&lt;/script&gt;  
                &lt;/xpath&gt;  
            &lt;/template&gt;  
  
        &lt;/data&gt;  
     &lt;/openerp&gt;  
</pre>


