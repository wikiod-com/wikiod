---
title: "Optimizing Magento For Speed"
slug: "optimizing-magento-for-speed"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Optimizing Magento Changing The .htaccess File
Magento is a very popular eCommerce application. It offers a great deal of customization and abilities from initial install. Here are a few suggestions for optimizing a Magento installation.

**Enabling Output Compression**

In your .htaccess file for Magento you will find a section of text starting with the line,

    <IfModule mod_deflate.c> and ending at </IfModule>

This section of code can be used to turn on Apache’s mod_deflate module, which provides compression for text, css, and javascript. You will want to uncomment (remove the # symbol) multiple lines so that it looks like this:

<IfModule mod_deflate.c>

############################################
## enable apache served files compression
## http://developer.yahoo.com/performance/rules.html#gzip

    # Insert filter on all content
    SetOutputFilter DEFLATE
    # Insert filter on selected content types only
    AddOutputFilterByType DEFLATE text/html text/plain text/xml text/css text/javascript

    # Netscape 4.x has some problems...
    BrowserMatch ^Mozilla/4 gzip-only-text/html

    # Netscape 4.06-4.08 have some more problems
    BrowserMatch ^Mozilla/4\.0[678] no-gzip

    # MSIE masquerades as Netscape, but it is fine
    BrowserMatch \bMSIE !no-gzip !gzip-only-text/html

    # Don't compress images
    SetEnvIfNoCase Request_URI \.(?:gif|jpe?g|png)$ no-gzip dont-vary

    # Make sure proxies don't deliver the wrong content
    Header append Vary User-Agent env=!dont-vary
    </IfModule>


## Enabling Expires Headers
First-time visitors to any web page has to make several HTTP requests. By using the “Expires” header you make the components of the requests cacheable. This avoids unnecessary HTTP requests on subsequent page views.

You want to find the area of the .htaccess file that starts with <IfModulemod_expires.c> and ends with the first </IfModule> you see after it, and make it look like this:

    <IfModule mod_expires.c>
    
    ############################################
    ## Add default Expires header
    ## http://developer.yahoo.com/performance/rules.html#expires
        ExpiresActive On
        ExpiresDefault "access plus 1 year"
    
    </IfModule>



## Admin Settings
**Merge JS and CSS Files**

This particular tweak will reduce the number of HTTP requests on your eCommerce site.
[box type=”alert” border=”full”]Note: This can sometimes break some applications. After performing the following steps, please ensure that the site still performs as it did before enabling this feature.[/box]

 1. Login to your administration area and go to – System > Configuration > Developer
 2. Under “JavaScript Settings”, change “Merge JavaScript Files” to yes.
 3. Under “CSS Settings”, change “Merge CSS Files” to yes.
 4. Finally you will want to clear your Magento cache.



## Enable Flat Catalogs
The model Magento uses to store customer and product data results in longer than average SQL queries and more reads. Enabling the Flat Catalog option for Categories and Products will merge product data into one table, therefore improving performance.

Login to your administration area and go to – System > Configuration > Catalog
Under “Frontend”, change “Use Flat Catalog Category” to yes.
Under “Frontend”, change “Use Flat Catalog Product” to yes – this is optional.
Next, you will want to clear your Magento cache.
Finally, you will need to reindex the tables.
Enable Compilation

[box type=”alert” border=”full”]Note: This can sometimes break some applications. After performing the following steps, please ensure that the site still performs as it did before enabling this feature.[/box]

 1. Login to your administration area and go to – System > Tools >
    Compilation 
 2. Next, simply click the Run Compilation Process Button
        
 3. After the compilation has run, it should enable itself automatically



## Enable System Cache

 1. Login to your administration area and go to – System > Cache   
    Management
 2. Next, click on the Select All link
 3. Finally, make sure the Actions is set to Enable and click submit

**Disable Error Logging**

Login to your administration area and go to – System > Configuration > Developer
Under the Log Settings section, be sure that Enabled is set to No
Database Maintenance Tips

There are several tables used by Magento for logging. While logging is very important regarding knowing what has and is going on with your store, the logs can become large very quickly, so regularly maintenance can be of great assistance.

Here are the tables for logging:

    log_customer
    log_visitor
    log_visitor_info
    log_url
    log_url_info
    log_quote
    report_viewed_product_index
    report_compared_product_index
    report_event
    catalog_compare_item

