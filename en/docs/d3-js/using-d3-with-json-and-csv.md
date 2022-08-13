---
title: "Using D3 with JSON and CSV"
slug: "using-d3-with-json-and-csv"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

## Syntax
 - d3.csv(url[[, row], callback])
 - d3.tsv(url[[, row], callback])
 - d3.html(url[, callback])
 - d3.json(url[, callback])
 - d3.text(url[, callback])
 - d3.xml(url[, callback])

## Loading data from CSV files
There are several ways of getting the data that you will bind to the DOM elements. The simpler one is having your data in your script as an array...

    var data = [ ... ];

But **D3.js** allows us to load data from an external file. In this example, we will see how to properly load and deal with data from an CSV file.

CSV files are *comma-separated values*. In this kind of file, each line is a data record, each record consisting of one or more fields, separated by commas. Its important to know that the function we are about to use, `d3.csv`, uses the first line of the CSV as the **header**, that is, the line that contains the names of the fields.

So, consider this CSV, named "data.csv":

    city,population,area
    New York,3400,210
    Melbourne,1200,350
    Tokyo,5200,125
    Paris,800,70

To load "data.csv", we use the function `d3.csv`. To make it easier, suppose that "data.csv" is in the same directory of our script, and its relative path is simply "data.csv". So, we write:

    d3.csv("data.csv", function(data){
        //code dealing with data here
    });

Notice that, in the callback, we used `data` as an argument. That's a common practice in D3, but you can use any other name.

What `d3.csv` does with our CSV? It converts the CSV in an array of objects. If, for instance, we `console.log` our data:

    d3.csv("data.csv", function(data){
        console.log(data)
    });

This is what we are going to see:

    [
        {
            "city": "New York",
            "population": "3400",
            "area": "210"
        },{
            "city": "Melbourne",
            "population": "1200",
            "area": "350"
        },{
            "city": "Tokyo",
            "population": "5200",
            "area": "125"
        },{
            "city": "Paris",
            "population": "800",
            "area": "70"
        }
    ]

Now we can bind that data to our DOM elements.

Notice that, in this example, `population` and `area` are strings. But, probably, you want to deal with them as numbers. You can change them in a function inside the callback (as a `forEach`), but in `d3.csv` you can use an "accessor" function:

    d3.csv("data.csv", conversor, function(data){
        //code here
    });

    function conversor(d){
        d.population = +d.population;
        d.area = +d.area;
        return d;
    }

You can also use accessors in `d3.tsv`, but not in `d3.json`.

**Note:** `d3.csv` is an asynchronous function, meaning that the code after it will execute immediately, even before the CSV file is loaded. So, special attention for using your `data` inside the callback.



## One or two parameters in callback—error handling in d3.request()
When using [`d3.request()`][1] or one of the convenience constructors ([d3.json][2], [d3.csv][3], [d3.tsv][4], [d3.html][5] and [d3.xml][6]) there are many sources for error. There may be problems with the issued request or its response due to network errors, or the parsing might fail if the content is not well-formed.

Within the callbacks passed to any of the above mentioned methods it is, therefore, desirable to implement some error handling. For this purpose the callbacks may accept two arguments, the first being the error, if any, the second being the data. If any error occurred during loading or parsing information about the error will be passed as the first argument `error` with the `data` being `null`.

    d3.json{"some_file.json", function(error, data) {
      if (error) throw error;   // Exceptions handling
      // Further processing if successful
    });
      
You are, whatsoever, not obliged to provide two parameters. It is perfectly fine to use the request methods with a callback featuring only one parameter. To handle these kinds of callbacks there is a private function [`fixCallback()`][7] in request.js, which adjusts the way information is passed to the method's single argument.

    function fixCallback(callback) {
      return function(error, xhr) {
        callback(error == null ? xhr : null);
      };
    }

This will be invoked by D3 for all callbacks having only one parameter, which by definition is the data.

No matter how many parameters are supplied to the request method's callback, the rule for the `data` parameter is:
* if the request fails, `data` will be `null`
* if the request succeeds, `data` will contain the loaded (and parsed) contents

The only difference between the one-parameter vs. the two-parameter version is in the 
way information is provided about the error which may occur. If the `error` parameter is 
omitted, the method will fail silently leaving `data` as `null`. If, on the other hand, the
callback is defined to have two arguments, information about an error during loading or parsing will be passed to the first parameter enable you to handle it appropriately.

The following four calls to `d3.json` demonstrate the scenarios possible for existing/non-existing files vs. one paramter/two parameter callbacks:

    // FAIL: resource not available or content not parsable
    // error contains information about the error
    // data will be null because of the error
    d3.json("non_existing_file.json", function(error, data) {
      console.log("Fail, 2 parameters: ", error, data);
    });
    
    // FAIL: resource not available or content not parsable
    // no information about the error
    // data will be null because of the error
    d3.csv("non_existing_file.json", function(data) {
      console.log("Fail, 1 parameter: ", data);
    });
    
    // OK: resource loaded successfully
    // error is null
    // data contains the JSON loaded from the resource
    d3.json("existing_file.json", function(error, data) {
      console.log("OK, 2 parameters: ", error, data);
    });
    
    // OK: resource loaded successfully
    // no information about the error; this fails silently on error
    // data contains the JSON loaded from the resource
    d3.json("existing_file.json", function(data) {
      console.log("OK, 1 parameter: ", data);
    });

  [1]: https://github.com/d3/d3-request#request
  [2]: https://github.com/d3/d3-request#json
  [3]: https://github.com/d3/d3-request#csv
  [4]: https://github.com/d3/d3-request#tsv
  [5]: https://github.com/d3/d3-request#html
  [6]: https://github.com/d3/d3-request#xml
  [7]: https://github.com/d3/d3-request/blob/master/src/request.js#L140-L144


