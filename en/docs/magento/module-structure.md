---
title: "Module structure"
slug: "module-structure"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

Modules exist to be extended. You cannot change `the app/code/` files without prohibiting any future updates. Instead we add a module to the `app/code/local` directory (the local directory may be missing, if so, it needs to be manually created. This is common in later versions of Magento) for added custom local functionality. 

All Module config files begin with a `<config>` tag. The new module is declared inside of the `<modules>` tag. We will be calling a module named YOUR_COMPANY_HelloWorld, therefore the `<YOUR_COMPANY_HelloWorld>` tags are used. In here we define the version (very first = 0.0.1)

A list of **XML events** can be found at: http://www.magentocommerce.com/wiki/5_-_modules_and_development/reference/module_config.xml

If you are having any trouble then check out: http://coding.smashingmagazine.com/2012/11/30/introducing-magento-layout/ 






## Creating a module from scratch (Hello World)
Magento custom module development is a core part of any Magento development or Magento project, because at any stage you may want to integrate your own functionality/module in your existing Magento project. 

The first thing developers should disable is the system cache. Otherwise developing will become a pain as any changes will require a flush.
From the magento admin panel: navigate to 
`System > Cache Management > Select All > Actions : Disable.`
Use the following guide to creating a new module:

 - Create a folder in `app/code/local/` - naming conventions usually
   take the name of your company e.g. `app/code/local/<YOUR_COMPANY>`.
 - Now we have a custom location for our modules. Create another directory, call it something related to the type of module you want to create e.g. app/code/local/<YOUR_COMPANY>/HelloWorld/ - “HelloWorld” is what I will call this Module.
 - This directory needs a `config.xml` so Magento recognises it as a new module. Create another folder named `etc`. Followed by a an xml file called config.xml. The directory should look like `app/code/local/<YOUR_COMPANY/HelloWorld/etc/config.xml`
This is the structure of the xml file:

       <?xml version="1.0" encoding="UTF-8" ?>
       <config>
          <modules>
             <YOUR_COMPANY_HelloWorld>
                <version>0.0.1</version>
             </YOUR_COMPANY_HelloWorld>
          </modules>
       </config>

 - Next, Modules need to be declared to Magento. Proceed to `app/etc/modules`. Create another XML document and give it chosen names of your tags: `YOUR_COMPANY_HelloWorld` in my case. In this document write:

       <?xml version="1.0" encoding="UTF-8"?>
       <config>
          <modules>
             <YOUR_COMPANY_HelloWorld>   
                <!-- Whether our module is active: true or false -->
                <active>true</active>
                <!-- Which code pool to use: core, community or local -->
                <codePool>local</codePool>
             </YOUR_COMPANY_HelloWorld>
          </modules>
       </config>

 - Again config and module tags are used to declare a new module to Magento. Active is the default value which can be accessed in the `Admin Panel under System > Configuration > Advanced`. `codePool` tells Magento which directory to look in. `local` in our case
 - This module has now been set up, thus the Model of our MVC structure. you should be able to see your new module in Admin Panel under `System > Configuration > Advanced`. **However, it does not do anything yet!** You will need to go back to our config.xml file and define XML elements.
 - Following on with the tutorial; We will use some of these XML Elements to create classes and manipulate all pages in the frontend of our site. Back to the `config.xml` file write the following under the `</modules>` tag:

       <global>
          <!-- adding a new block definition -->
          <blocks>
              <!-- A unique short name for our block files -->
              <helloworld>
                 <!-- the location of our modules block -->
                 <class>YOUR_COMPANY_HelloWorld_Block</class>
              </helloworld>
          </blocks>
       </global>
       <!-- We are making changes to the frontend -->
       <frontend>
          <!-- We are making changes to the layout of the front end -->
          <layout>
             <!-- we are adding a new update file -->
             <updates>
                <!-- Creating the name of our update and linking it the module -->
                <helloworld module="YOUR_COMPANY_HelloWorld">
                    <!-- The name of the layout file we are adding -->
                    <file>helloworld.xml</file>
                </helloworld>
            </updates>
         </layout>
       </frontend>

 - As you can see we are constantly extending rather than manipulating core files. The `helloworld` tag is lower case because that will point to a Handle, and for continuity we will name it as closely as possible. We then link this to the `YOUR_COMPANY_HelloWorld` module.
- We are changing the layout. Therefore we need to create this Handle in the layout directory. proceed to `app/design/frontend/base/default/layout`. We told the module to look for the `helloworld.xml` file. Therefore we must create it in this directory. What are you waiting for. Do it!! and populate it with:

      <?xml version="1.0" encoding="UTF-8"?>
      <!-- All Layout files begin with this code -->
      <layout>
         <!-- this is the layout handle. Default is handled on all pages. We want this module to execute on all pages -->
         <default>
            <!-- This is the block we want to bolt onto the existing before_body_end block -->
            <reference name="before_body_end">
                <!-- We define our new block and template to be added to before_body_end -->
                <block name="helloworld_footer" template="helloworld/footer.phtml" type="helloworld/footer"/>
            </reference>
         </default>
      </layout>

 - Now those of you who have a little Magento experience, or have read any more noteworthy Magento tutorials, may be gasping at the fact we are making changes in base/default since this is where Magento core files are located. However, we are not modifying any files here, we are creating new ones, and furthermore we are prefixing our file name with “helloworld,” so there is very little chance of this conflicting with other modules or causing any issues with upgrading Magento in the future. Happy days!
 - Because we want to affect all pages, we use the default tag and Reference it to the `before_body_end` Structural Block. This will act the role of the Action and trigger the View section of our MVC structure.
 - Now we understand that we are bolting onto the `before_body_end` block. and linking it to our custom block. This is called a Reference and is a **hook**. We currently are not hooking it to anything existing therefore we must create the necessary files.
 - In `helloworld.xml` we stated in template a `footer.phtml`. Proceed to `app/design/frontend/base/default/template` and create a directory `helloworld`.
 - Inside this directory create the `footer.phtml` file and fill in with HTML, this tutorial simply writes this to display some PHP functionality linked with our PHTML file:

       <p>Hello Everyone! Todays date is <?php echo $this->getDate() ?></p>

 - We now need to create our own block object to couple the template with our block functionality. Create the directory app/code/local/YOUR_COMPANY/HelloWorld/Block/ and create the file `Footer.php` inside of this. This was referenced in our zeta_layout.xml in the type “helloworld/footer”. Populate this file with:

       <?php
       class YOUR_COMPANY_HelloWorld_Block_Footer extends Mage_Core_Block_Template {
          public function getDate() {
             $date = date('Y-m-d');
             return urlencode($date);
          }
       }
       ?>

 - This is the functionality that will populate our call `getDate()` we called from our `.phtml` file. We extend the `Mage_Core_Block_Template`.

- This functionality is now complete. Test this out by going to your home page where you should see your module within the footer of every page! 




