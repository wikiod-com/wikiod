---
title: "Performance"
slug: "performance"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## Profiling with Xdebug
An extension to PHP called Xdebug is available to assist in [profiling PHP applications][1], as well as runtime debugging.  When running the profiler, the output is written to a file in a binary format called "cachegrind".  Applications are available on each platform to analyze these files.

To enable profiling, install the extension and adjust php.ini settings.  In our example we will run the profile optionally based on a request parameter.  This allows us to keep settings static and turn on the profiler only as needed.

    // Set to 1 to turn it on for every request
    xdebug.profiler_enable = 0
    // Let's use a GET/POST parameter to turn on the profiler
    xdebug.profiler_enable_trigger = 1
    // The GET/POST value we will pass; empty for any value
    xdebug.profiler_enable_trigger_value = ""
    // Output cachegrind files to /tmp so our system cleans them up later
    xdebug.profiler_output_dir = "/tmp"
    xdebug.profiler_output_name = "cachegrind.out.%p"

Next use a web client to make a request to your application's URL you wish to profile, e.g.

    http://example.com/article/1?XDEBUG_PROFILE=1

As the page processes it will write to a file with a name similar to 

    /tmp/cachegrind.out.12345

Note that it will write one file for each PHP request / process that is executed.  So, for example, if you wish to analyze a form post, one profile will be written for the GET request to display the HTML form.  The XDEBUG_PROFILE parameter will need to be passed into the subsequent POST request to analyze the second request which processes the form.  Therefore when profiling it is sometimes easier to run curl to POST a form directly.

Once written the profile cache can be read by an application such as KCachegrind.

[![KCachegrind][2]][2]

This will display information including:

- Functions executed
- Call time, both itself and inclusive of subsequent function calls
- Number of times each function is called
- Call graphs
- Links to source code

Obviously performance tuning is very specific to each application's use cases.  In general it's good to look for:

- Repeated calls to the same function you wouldn't expect to see. For functions that process and query data these could be prime opportunities for your application to cache.
- Slow-running functions.  Where is the application spending most of its time? the best payoff in performance tuning is focusing on those parts of the application which consume the most time.

*Note*: Xdebug, and in particular its profiling features, are very resource intensive and slow down PHP execution.  It is recommended to not run these in a production server environment.


  [1]: https://xdebug.org/docs/profiler
  [2]: http://i.stack.imgur.com/ENtOu.gif

## Memory Usage
PHP's runtime memory limit is set through the INI directive `memory_limit`.  This setting prevents any single execution of PHP from using up too much memory, exhausting it for other scripts and system software.  The memory limit defaults to 128M and can be changed in the `php.ini` file or at runtime.  It can be set to have no limit, but this is generally considered bad practice.

The exact memory usage used during runtime can be determined by calling `memory_get_usage()`.  It returns the number of bytes of memory allocated to the currently running script.  As of PHP 5.2, it has one optional boolean parameter to get the total allocated system memory, as opposed to the memory that's actively being used by PHP.

     <?php
     echo memory_get_usage() . "\n";
     // Outputs 350688 (or similar, depending on system and PHP version)

     // Let's use up some RAM
     $array = array_fill(0, 1000, 'abc');

     echo memory_get_usage() . "\n";
     // Outputs 387704

     // Remove the array from memory
     unset($array);
    
     echo memory_get_usage() . "\n";
     // Outputs 350784

Now `memory_get_usage` gives you memory usage at the moment it is run.  Between calls to this function you may allocate and deallocate other things in memory.  To get the maximum amount of memory used up to a certain point, call `memory_get_peak_usage()`.

    <?php
    echo memory_get_peak_usage() . "\n";
    // 385688
    $array = array_fill(0, 1000, 'abc');
    echo memory_get_peak_usage() . "\n";
    // 422736
    unset($array);
    echo memory_get_peak_usage() . "\n";
    // 422776

Notice the value will only go up or stay constant.

## Profiling with XHProf


