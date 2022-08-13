---
title: "Array Helper"
slug: "array-helper"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

The Array Helper file contains functions that assist in working with arrays.

## Loading this Helper
This helper is loaded using the following code:

    $this->load->helper('array');

The following functions are available:

**element()**

Lets you fetch an item from an array. The function tests whether the array index is set and whether it has a value. If a value exists it is returned. If a value does not exist it returns FALSE, or whatever you've specified as the default value via the third parameter. Example:

    $array = array('color' => 'red', 'shape' => 'round', 'size' => '');
    
    // returns "red"
    echo element('color', $array);
    
    // returns NULL
    echo element('size', $array, NULL);
**random_element()**

Takes an array as input and returns a random element from it. Usage example:

    $quotes = array(
                "I find that the harder I work, the more luck I seem to have. - Thomas Jefferson",
                "Don't stay in bed, unless you can make money in bed. - George Burns",
                "We didn't lose the game; we just ran out of time. - Vince Lombardi",
                "If everything seems under control, you're not going fast enough. - Mario Andretti",
                "Reality is merely an illusion, albeit a very persistent one. - Albert Einstein",
                "Chance favors the prepared mind - Louis Pasteur"
                );
    
    echo random_element($quotes);
**elements()**

Lets you fetch a number of items from an array. The function tests whether each of the array indices is set. If an index does not exist it is set to FALSE, or whatever you've specified as the default value via the third parameter. Example:

    $array = array(
        'color' => 'red',
        'shape' => 'round',
        'radius' => '10',
        'diameter' => '20'
    );
    
    $my_shape = elements(array('color', 'shape', 'height'), $array);
The above will return the following array:

    array(
        'color' => 'red',
        'shape' => 'round',
        'height' => FALSE
    );
You can set the third parameter to any default value you like:

    $my_shape = elements(array('color', 'shape', 'height'), $array, NULL);
The above will return the following array:

    array(
        'color' => 'red',
        'shape' => 'round',
        'height' => NULL
    );
This is useful when sending the `$_POST` array to one of your Models. This prevents users from sending additional POST data to be entered into your tables:

    $this->load->model('post_model');
    
    $this->post_model->update(elements(array('id', 'title', 'content'), $_POST));
This ensures that only the id, title and content fields are sent to be updated.

