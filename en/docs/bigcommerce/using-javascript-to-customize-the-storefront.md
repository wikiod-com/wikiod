---
title: "Using javascript to customize the storefront"
slug: "using-javascript-to-customize-the-storefront"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Javascript examples to interact with existing store variables or scrape data that isn't easily exposed

## Obtaining product data and order details from finishorder.php
It parses the data from the %%GLOBAL_ConversionCode%% template variable, and as such this script should be inserted in order.html immediately after the %%GLOBAL_ConversionCode%% variable. This was originally intended for the Blueprint theme framework and, as such, may not work on Stencil. 

    <script>
    //-------------- Main --------------//

    //** Create the order data array from analytics script **//
    var data = parseAnalyticsData(getAnalyticsScript());
    //console.log(data);

    /**
     * Retrieve the order details as an object, properties are:
     * id          - The order ID.
     * shipping    - The order shipping cost. 
     * tax         - The order tax cost. 
     * shippingTax - The order shipping tax cost.
     * city        - The order shipping city.
     * state       - The order shipping state. 
     * country     - The order shipping country.
     */
    var orderDetails = getOrderDetails(data);

    console.log("Order ID = %d", orderDetails.id);
    console.log("Order shipping city = %s", orderDetails.city);
    console.log("Order subtotal = %f", orderDetails.subtotal);

    /**
     * Retrieve the order product details, as an array of product objects. 
     * Properties are:
     * id          - The product ID. 
     * description - The product description.
     * tax         - The product tax cost.
     * price       - The product price per product. 
     * qty         - The product quantity purchased. 
     */
    var products = getOrderProducts(data);

    //** Loop through the products array to access each product **//
    console.log("Total number of products = %d", products.length);
    for (x=0; x<products.length; x++) {
      console.log("--------");
      console.log("Item # ", x+1);
      console.log("Product ID = %f", products[x].id);
      console.log("Product QTY = %f", products[x].qty);
      console.log("Product Price = %f", products[x].price);
      console.log("--------");
    }



    //-------------- Functions --------------//

    /**
     * Parses the DOM to retrieve the order data analytics script.
     */
    function getAnalyticsScript() {
      var scripts = document.getElementsByTagName('script');
      var thisScriptTag = scripts[scripts.length - 2];
      var data = thisScriptTag.textContent || thisScriptTag.innerText;
      return data;
    }

    /**
     * Parses the raw analytics script element to remove all script
     * text, and parse just the order related data into an array.
     * @param script <String> - The raw order analytics script.
     * @return <mixed> - Array containing the order data. 
     */
    function parseAnalyticsData(data) {
      String.prototype.replaceAll = function(search, replacement) {
        var target = this;
        return target.split(search).join(replacement);
      };
      // This is hacky, and probably inefficient, but it removes all
      // script related text, so the end result is just a comma separated
      // array of the order and product data. 
      data = data.replace("if(typeof(pageTracker) != 'undefined') {", '');
      data = data.replaceAll( 'pageTracker._addTrans(', '');
      data = data.replaceAll( ' pageTracker._trackTrans();', '');
      data = data.replaceAll( 'pageTracker._addItem(', '');
      data = data.replaceAll(');', '');
      data = data.replace('}', '');
      data = data.replace( /\n/g, ",").replaceAll( ",,",",");
      data = data.replace(/\s/g,'');
      data = data.split(',');
      data = cleanArray(data); // Remove all empty values from array. 
      return data;
    }

    /**
     * Removes all empty data from array.
     * @param array <mixed> - The array to clean. 
     */
    function cleanArray(array) {
      var newArray = new Array();
      for (var i = 0; i < array.length; i++) {
        if (array[i]) {
          newArray.push(array[i]);
        }
      }
      return newArray;
    }

    /**
     * Parse Analytics Data for Order Details
     * @param data <mixed> - The order analytics data.
     * @return <mixed>     - Object containing the order details. 
     */
    function getOrderDetails(data) {
      String.prototype.replaceAll = function(search, replacement) {
        var target = this;
        return target.split(search).join(replacement);
      };
      return {
        id          : parseFloat(data[0].replaceAll("'",'')),
        subtotal    : ( parseFloat(data[2].replaceAll("'",'')) - (parseFloat(data[3].replaceAll("'",'')) + parseFloat(data[4].replaceAll("'",'')) ) ),
        total       : parseFloat(data[2].replaceAll("'",'')),
        tax         : parseFloat(data[3].replaceAll("'",'')),
        shipping    : parseFloat(data[4].replaceAll("'",'')),
        city        : data[5].replaceAll("'",''),
        state       : data[6].replaceAll("'",''),
        country     : data[7].replaceAll("'",'')
      }
    }

    /**
     * Parse Analytics Data for All Order Product Details.
     * @param data <mixed> - The order analytics data.
     * @return <mixed>     - Array containing individual product details.
     */
    function getOrderProducts(data) {
      String.prototype.replaceAll = function(search, replacement) {
        var target = this;
        return target.split(search).join(replacement);
      };
      var counter = -1;        // Keep index of details per product.
      var productsArray = []; // Init empty array to hold all products. 
      var product = {};       // Init empty object to hold single product data. 
      //** Product data starts at index 8 **//
      for (x=8; x<data.length; x++) {
        counter++;
        switch (counter) {
          case 1:
            product.id = parseFloat(data[x].replaceAll("'",''));
            break;
          case 2:
            product.description = data[x].replaceAll("'",'');
            break;
          case 3:
            product.tax = parseFloat(data[x].replaceAll("'",''));
            break;
          case 4:
            product.price = parseFloat(data[x].replaceAll("'",''));
            break;
          case 5:
            product.qty = parseFloat(data[x].replaceAll("'",''));
            counter = -1;                 // reset counter
            productsArray.push(product); // push product to products array
            product = {};
            break;
        }
      }
      return productsArray;
    }

 </script>

## Change product image on hover on a category page in Stencil
This was added to `assets/js/theme/category.js` in `loaded()`. You will also need to add `{{inject "categoryProducts" category.products}}` to `templates/pages/category.html`

    var mainImages = [];
      var rollOvers = [];
      this.context.categoryProducts.forEach(function(e, i) {
        if (e.images[0]) {
          mainImages[e.id] = e.images[0].data;
        }
        if (e.images[1]) {
          rollOvers[e.id] = e.images[1].data;
        }
      });
    
      rollOvers.forEach(function(image, id) {
        image = image.replace('{:size}', '500x659');
    
        $('a[data-product-id="' + id + '"]').closest('li.product').find('.card-image')
          .on('mouseover', function() {
          $(this).attr('src', image);
        }).on('mouseout', function() {
          $(this).attr('src', mainImages[id].replace('{:size}', '500x659'));
        });
      });

