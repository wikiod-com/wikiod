---
title: "ImageFile Uploader In CodeIgniter"
slug: "imagefile-uploader-in-codeigniter"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

It is not necessary that you have to use the same names for the (Controller,File,Class,ID) or whatever it might be. All the things what I have used is for the understanding purpose of the coding flow and my assumptions. It is up to the developer who takes the code and edits the code/name according to their wish and then host the code and succeed. 

## Single File/ Image Uploader
We shall now see how the Image/File Uploading code works in the native CI method with the help of the forms that has been proposed by the CI way. 

File uploading in PHP has Two Scenarios. It is mentioned below as follows.

 - Single Image/File uploader - This can be saved with the help of the normal variable in the form attribute. (E.g.) `<input type="file" name="image" />`
 - Multi-image/File Uploader - This can be saved only with the help of the array variable for the name in the file type. (E.g.) `<input type="file" name="image[]" />`.

The array variable namely `name="profile[]"` can also be kept for the `single image` uploader as well as the `multi-image` **uploader** too. 

>Hence the Single Image/File Uploader Code in the Native CodeIgnitor format is as follows:

**View Part:**

    <?php
    echo form_open_multipart('employee/addemployee', array('name' => 'addemployee', 'class'=>'form-horizontal'));
    ?>
    <div class="form-group">
       <label class="control-label col-sm-4" for="pwd">Profile:</label>
       <div class="col-sm-8">
          <input type="file" class="" id="profile" name="userimage">
       </div>
    </div>
    <div class="form-group">
       <div class="col-sm-offset-2 col-sm-10">
            <input type="submit" class="btn btn-primary pull-right" name="save" value="Save Employee" /> 
       </div>
    </div>
    <?php
    echo form_close();
    ?>

Hence if we submit the form it will be going to the 

 - `Employee` - Controller and search for the function named `addemployee`
 - If you need the required attribute for the file uploader code you can add up the HTML5 attribute called `required` to the input tag. 

Below is the two examples of how to use the required attribute but both the methods are the same as well.

 1. Method One: `<input type="file" name="photo" required="required" />`
 2. Method Two: `<input type="file" name="photo" required />`

Hence these are the some of the important tips that are to be followed in the view part of the image/file uploader.

**Controller Part:**


    <?php if ( ! defined('BASEPATH')) exit('No direct script access allowed');
    
    class Employee extends CI_Controller {
    
        function __construct() {
        parent::__construct();
        $this->load->model('employee_model');
        $this->load->helper('url'); //This will load up all the URL parameters from the helper class
        $this->load->helper('form'); //This will load up all the form attributes that are need by the form.
        }
    
        public function addemployee()
        {
            if($_FILES["userimage"]['name']=='')            
            {
                // Here you can directly redirect to the form page itself with the Error Message
            }
            else
            {
                $new_name = time().$_FILES["userimage"]['name']; //This line will be generating random name for images that are uploaded       
                $config['upload_path'] = FCPATH ."assets/fileupload/";
                $config['allowed_types'] = 'gif|jpg|png';
                $config['file_name'] = $new_name;
                $this->load->library('upload', $config); //Loads the Uploader Library
                $this->upload->initialize($config);        
                if ( ! $this->upload->do_upload('userimage'))  {}
                else
                { 
                $data = $this->upload->data(); //This will upload the `image/file` using native image upload 
                }
                $data_value = array(
                'profile'=>$new_name,
                ); //Passing data to model as the array() parameter    
                $this->employee_model->saveemployee($data_value); //save_employee is the function name in the Model            
            }    
        }
    ?>

**Note:** By default the upload routine expects the file to come from a form field called `userfile`, and the `form` must be of type `multipart`.
 - Hence it will go to the `employee_model` with the `$data_value` - array and it will be saving the data under the function called `saveemployee`.
 - If you would like to set your own field name simply pass its value to the `do_upload()` method
 - Using File Uploading class, we can upload files and we can also, restrict the type and size of the file to be uploaded.
 - `display_errors()` - Retrieves any error messages if the `do_upload()` method returned false. The method does not echo automatically, it returns the data so you can assign it however you need

**Notations:**

These are the notations that are available in the CI and we can define it in the `index.php` as a Short Definition and we can use it in the Entire project. 

    EXT: The PHP file extension
    FCPATH: Path to the front controller (this file) (root of CI)
    SELF: The name of THIS file (index.php)
    BASEPATH: Path to the system folder
    APPPATH: The path to the "application" folder

**Model Part:**

    public function saveemployee($data_value)
    {
        $this->db->insert('employee',$data_value);
    }

 - It will be saving the data on the `employee` table with the uploaded image name.
 - And the image uploaded will be saved into the directory that we have created at the root folder or any other folder that we specify.

