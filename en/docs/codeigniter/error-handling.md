---
title: "Error Handling"
slug: "error-handling"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

CodeIgniter lets you build error reporting into your applications using the functions described below. In addition, it has an error logging class that permits error and debugging messages to be saved as text files.


## log_message()
This function lets you write messages to your log files. You must supply one of three "levels" in the first parameter, indicating what type of message it is (debug, error, info), with the message itself in the second parameter. 

Example:

    if ($some_var == "") {
        log_message('error', 'Some variable did not contain a value.'); 
    } 
    else {
        log_message('debug', 'Some variable was correctly set'); 
    }
    
    log_message('info', 'The purpose of some variable is to provide some value.');


----------


Syntax
======

    log_message($level, $message);

**Parameters:**   

 - `$level (string)` – Log level: ‘error’, ‘debug’ or ‘info’
 - `$message (string)` – Message to log

**Return type:**    void


----------


There are three message types:
==============================

- **Error Messages**. These are actual errors, such as PHP errors or user errors.
- **Debug Messages.** These are messages that assist in debugging. For example, if a class has been initialized, you could log this as debugging info.
- **Informational Messages**. These are the lowest priority messages, simply giving information regarding some process. CodeIgniter doesn't natively generate any info messages but you may want to in your application.

> **Note:** In order for the log file to actually be written, the "logs"
 the folder must be writable. In addition, you must set the "threshold" for
 logging in **`application/config/config.php`**. You might, for example, only want error messages to be logged, and not the other two types. If you set it to zero logging will be disabled.

## show_error()
This function will display the error message supplied to it using the following error template:

**Path -** `application/errors/error_general.php`

The optional parameter $status_code determines what HTTP status code should be sent with the error.


----------

Syntax
======

    show_error($message, $status_code, $heading = 'An Error Was Encountered')

**Parameters:**   

- `$message (mixed)` – Error message
- `$status_code (int)` – HTTP Response status code
- `$heading (string)` – Error page heading

**Return type:**    void


----------


Source
======

 1. [`show_error` in codeigniter.com][1]


  [1]: https://www.codeigniter.com/user_guide/general/errors.html#show_error

## show_404()
This function will display the 404 error message supplied to it using the following error template:

**Path -**  `application/errors/error_404.php`

The function expects the string passed to it to be the file path to the page that isn't found. Note that CodeIgniter automatically shows 404 messages if controllers are not found.

CodeIgniter automatically logs any `show_404()` calls. Setting the optional second parameter to FALSE will skip logging.


----------

Syntax
======

    show_404($page = '', $log_error = TRUE)

**Parameters:**   

- $page (string) – URI string
- $log_error (bool) – Whether to log the error

**Return type:**    void

----------

Source
======

 1. [`show_404` in codeigniter.com][1]


  [1]: https://www.codeigniter.com/user_guide/general/errors.html#show_404

