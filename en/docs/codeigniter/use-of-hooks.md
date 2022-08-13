---
title: "Use of hooks"
slug: "use-of-hooks"
draft: false
images: []
weight: 9550
type: docs
toc: true
---

## Hook Points
## `pre_system`
Called very early during system execution. Only the benchmark and hooks class have been loaded at this point. No routing or other processes have happened.

## `pre_controller`
Called immediately prior to any of your controllers being called. All base classes, routing, and security checks have been done.

## `post_controller_constructor`
Called immediately after your controller is instantiated, but prior to any method calls happening.

## `post_controller`
Called immediately after your controller is fully executed.

## `display_override`
Overrides the `_display()` method, used to send the finalized page to the web browser at the end of system execution. This permits you to use your own display methodology. Note that you will need to reference the CI super-object with `$this->CI =& get_instance()` and then the finalized data will be available by calling `$this->CI->output->get_output()`.

## `cache_override`
Enables you to call your own method instead of the `_display_cache()` method in the Output Library. This permits you to use your own cache display mechanism.

## `post_system`
Called after the final rendered page is sent to the browser, at the end of system execution after the finalized data is sent to the browser.

## Pre Controller Hook example using CodeIgniter
In `application/hooks` folder, create a file with name `Blocker.php` and paste the below code.

    <?php
    class Blocker {
    
        function Blocker(){
        }
        
        /**
         * This function used to block the every request except allowed ip address
         */
        function requestBlocker(){
            
            if($_SERVER["REMOTE_ADDR"] != "49.248.51.230"){
                echo "not allowed";
                die;
            }
        }
    }
    ?>

In `application/config/hooks.php`, declare the following hook.

    $hook['pre_controller'] = array(
            'class'    => 'Blocker',
            'function' => 'requestBlocker',
            'filename' => 'Blocker.php',
            'filepath' => 'hooks',
            'params'   => ""
    );

In `application/config/config.php`, set following value as true

## Enabling Hooks
>The hooks feature can be globally enabled/disabled by setting the following item in the `application/config/config.php` file:

    $config['enable_hooks'] = TRUE;

## Defining a Hook
> Hooks are defined in the `application/config/hooks.php` file. Each hook
 is specified as an array with this prototype

    $hook['pre_controller'] = array(
            'class'    => 'MyClass',
            'function' => 'Myfunction',
            'filename' => 'Myclass.php',
            'filepath' => 'hooks',
            'params'   => array('beer', 'wine', 'snacks')
    );

The array index correlates to the name of the particular hook point you want to use. In the above example, the hook point is `pre_controller`. A list of hook points is found below. The following items should be defined in your associative hook array:

**class** The name of the class you wish to invoke. If you prefer to use a procedural function instead of a class, leave this item blank. 

**function** The function (or method) name you wish to call.

**filename** The file name containing your class/function. 

**file-path** The name of the directory containing your script.

**params** Any parameters you wish to pass to your script. This item is optional.

## Defining a Hook
Hooks are defined in `application/config/hooks.php` file. Each hook is specified as an array with this prototype:  

    $hook['pre_controller'] = array(
          'class'    => 'MyClass',
          'function' => 'Myfunction',
          'filename' => 'Myclass.php',
          'filepath' => 'hooks',
          'params'   => array('bread', 'wine', 'butter')
    );

 - `CLASS`- The class that you wish to invoke if it is procedural code leave it as blank.
 - `FUNCTION`- The function name you wish to call.
 - `FILENAME`- The file name containing your class/function.
 - `FILEPATH`- Location of the hook file.
 - `PARAMS`-Additional parameter if needed it is optional

