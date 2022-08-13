---
title: "Loading Source Files"
slug: "loading-source-files"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Require files to be loaded only once
The [Kernel#require][1] method will load files only once (several calls to `require` will result in the code in that file being evaluated only once). It will search your ruby `$LOAD_PATH` to find the required file if the parameter is not an absolute path. Extensions like `.rb`, `.so`, `.o` or `.dll` are optional. Relative paths will be resolved to the current working directory of the process.

    require 'awesome_print'

The [Kernel#require_relative][2] allows you to load files relative to the file in which `require_relative` is called.

    # will search in directory myproj relative to current source file.
    #
    require_relative 'myproj/version'  


  [1]: http://www.rubydoc.info/stdlib/core/Kernel%3Arequire
  [2]: http://www.rubydoc.info/stdlib/core/Kernel%3Arequire_relative

## Automatically loading source files
The method [`Kernel#autoload`][1] registers filename to be loaded (using `Kernel::require`) the first time that module (which may be a String or a symbol) is accessed.

    autoload :MyModule, '/usr/local/lib/modules/my_module.rb' 

The method [Kernel#autoload?][2] returns filename to be loaded if name is registered as `autoload`.

    autoload? :MyModule  #=> '/usr/local/lib/modules/my_module.rb'


  [1]: http://www.rubydoc.info/stdlib/core/Kernel%3Aautoload
  [2]: http://www.rubydoc.info/stdlib/core/Kernel%3Aautoload%253F

## Loading optional files
When files are not available, the `require` family will throw a [`LoadError`][1]. This is an example which illustrates loading optional modules only if they exist.

    module TidBits
    
    @@unavailableModules = []
    
    [
          { name: 'CoreExtend', file: 'core_extend/lib/core_extend'  } \
        , { name: 'Fs'        , file: 'fs/lib/fs'                    } \
        , { name: 'Options'   , file: 'options/lib/options'          } \
        , { name: 'Susu'      , file: 'susu/lib/susu'                } \
    
    ].each do |lib|
    
        begin
    
            require_relative lib[ :file ]
    
        rescue LoadError
    
            @@unavailableModules.push lib
    
        end
    
    end

    end # module TidBits


  [1]: http://www.rubydoc.info/stdlib/core/LoadError

## Loading files repeatedly
The [Kernel#load][1] method will evaluate the code in the given file. The search path will be constructed as with `require`. It will re-evaluate that code on every subsequent call unlike `require`. There is no `load_relative`.

    load `somefile`


  [1]: http://www.rubydoc.info/stdlib/core/Kernel%3Aload

## Loading several files
You can use any ruby technique to dynamically create a list of files to load. Illustration of globbing for files starting with `test`, loaded in alphabetical order.

    Dir[ "#{ __dir__ }**/test*.rb" ) ].sort.each do |source|

        require_relative source

    end

