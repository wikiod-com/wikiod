---
title: "Getting started with jquery-select2"
slug: "getting-started-with-jquery-select2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Jquery - Select2 Installation and Setup
You can include/install Select2 in one of the two ways
1) Directly using CDN's in your project under the head section of your project.

link href="https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.3/css/select2.min.css" rel="stylesheet"/>

    <script src="https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.3/js/select2.min.js"/>

2) Download the code to your local machine and include it into your project. it should look like this [![enter image description here][1]][1]

Then include these lines into head section of your html page 
  
      <link href="select2.min.css" rel="stylesheet" />
      <script src="select2.min.js"></script>


> Note: You need to have Jquery  included into your project for Select2
> to work correctly.
> 
> PITFALL: if Current version of jquery and Select2 are conflicting or
> Select2 features are not working. Then move the select2 variables and 
> implementations in separate block of   document body
> $(document).ready(function () {   //your select2 code ..... }); 

**How to use it:** 
   You can define a `<select>` as follows:

    <select id="select2_example">
        <option>Test</option>
    </select>

   **Approach1:**  
  
          var _mSelect2 = $("#select_example").select2();

  **Approach2:**    

      <script type="text/javascript">
          $('select').select2();
      </script>
> Please note that using this approach all the defined 'select' control of page
> will inherit features of Select2. If you only want to use Select2 for
> certain controls then skip this step and write your own code to use select2 feature directly and jquery/javascript feature separately for other select control. However, you can use the value of  manipulated through Select2 for Jquery or vice-versa.

Further examples of 'The Basics' can be found in the Select2 GIT documentation [here][2]
 
Select2 GIT project is [here][3].  Please go through the Select2 Github site for detail feature of this product.


  [1]: https://i.stack.imgur.com/htxjr.png
  [2]: https://select2.github.io/examples.html
  [3]: https://github.com/select2/select2


## To clear the selected elements of Select2 dropdown.
In order to clear the selection of those values which are selected using a `Select2` drop down,we can use the empty() function.

    <select id="select2_example">
    <option>Option1</option>
    <option>Option2</option>
    <option>Option3</option>
    </select>

The dropdown can be reset using Jquery.
 
    $("#select2_example").empty();

It can also be reset as below.

    $("#select2_example").select2("val", "");

    

