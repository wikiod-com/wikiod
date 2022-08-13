---
title: "CodeIgniter - Internationalization"
slug: "codeigniter---internationalization"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

The language class in CodeIgniter provides an easy way to support multiple languages for internationalization. To some extent, we can use different language files to display text in many different languages.

## Example
**Creating files Language**

To create a language file, you must end it with `_lang.php`. For example, you want to create a language file for French language, then you must save it with `french_lang.php`. Within this file you can store all your language texts in key, value combination in `$lang` array as shown below.

    $lang['key'] = 'val';

**Loading Language file**

To use any of the language in your application, you must first load the file of that particular language to retrieve various texts stored in that file. You can use the following code to load the language file.

    $this->lang->load('filename', 'language');

**filename**  :  It is the name of file you want to load. Don’t use extension of file here but only name of file.
**Language** : It is the language set containing it.

**Fetching Language Text**

    $this->lang->line('language_key');

To fetch a line from the language file simply execute the following code.
Where **language_key** is the key parameter used to fetch value of the key in the loaded language file.

**Autoload Languages**

If you need some language globally, then you can autoload it in `application/config/autoload.php` file as shown below.

    | -----------------------------------------------------------------------
    |  Auto-load Language files
    | -----------------------------------------------------------------------
    | Prototype:
    |   $autoload['language'] = array('lang1', 'lang2');
    |
    | NOTE: Do not include the "_lang" part of your file. For example
    | "codeigniter_lang.php" would be referenced as array('codeigniter');
    |
    */
    $autoload['language'] = array();

Simply, pass the different languages to be autoloaded by CodeIgniter.

Create a controller called `Lang_controller.php` and save it in `application/controller/Lang_controller.php`

    <?php
   class Lang_controller extends CI_Controller {

    public function index(){
         //Load form helper
         $this->load->helper('form');

         //Get the selected language
         $language = $this->input->post('language');
        
         //Choose language file according to selected lanaguage
         if($language == "french")
            $this->lang->load('french_lang','french');
         else if($language == "german")
            $this->lang->load('german_lang','german');
         else
         $this->lang->load('english_lang','english');
        
         //Fetch the message from language file.
         $data['msg'] = $this->lang->line('msg');
        
         $data['language'] = $language;
         //Load the view file
         $this->load->view('lang_view',$data);
         }
      }
    ?>

Create a view file called `lang_view.php` and save it at `application/views/lang_view.php`

    <!DOCTYPE html>
    <html lang = "en"> 
      <head>
         <meta charset = "utf-8">
         <title>CodeIgniter Internationalization Example</title>
      </head>  
      <body>
         <?php
            echo form_open('/lang');
         ?>
         <select name = "language" onchange = "javascript:this.form.submit();">
            <?php
               $lang = array('english'=>"English",'french'=>"French",'german'=>"German");
                
               foreach($lang as $key=>$val) {
                  if($key == $language)
                     echo "<option value = '".$key."' selected>".$val."</option>";
                  else
                     echo "<option value = '".$key."'>".$val."</option>";
               }           
            ?>          
         </select>
         <?php
            form_close();
            echo $msg;
         ?>     
      </body>
    </html>

Create three folders called **English**, **French**, and **German** in **application/language** as shown in the figure below.

[![internationalization][1]][1]


  [1]: https://i.stack.imgur.com/icOOT.png

Copy the below given code and save it in `english_lang.php` file in `application/language/english` folder.

    <?php
       $lang['msg'] = "CodeIgniter Internationalization example.";
    ?>

Copy the below given code and save it in `french_lang.php` file in `application/language/French` folder.

    <?php
       $lang['msg'] = "Exemple CodeIgniter internationalisation.";
    ?>

Copy the below given code and save it in `german_lang.php` file in `application/language/german` folder.

    <?php
        $lang['msg'] = "CodeIgniter Internationalisierung Beispiel.";
    ?>
Change the `routes.php` file in `application/config/routes.php` to add route for the above controller and add the following line at the end of the file.

    $route['lang'] = "Lang_controller";

Execute the following URL in the browser to execute the above example.

    http://yoursite.com/index.php/lang
Then Check in Your Browser. Thank you.





