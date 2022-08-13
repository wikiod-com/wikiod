---
title: "Auto-completion and suggestion"
slug: "auto-completion-and-suggestion"
draft: false
images: []
weight: 9928
type: docs
toc: true
---

## Basic settings of Auto-completion and suggestions
Notepad++ provides 2 types of features for auto-completion and suggestions:

 - Auto-completion that reads the open file and provide suggestion of words and/or functions within the file
 - Suggestion with the arguments of functions (specific to the language)

To enable it, you need to change a setting.

 1. Go to `Settings` > `Preferences...` > `Auto-completion`
 2. Check `Enable Auto-completion on each input`
 3. Select the type of completion you desire: 
    * words only
    * name of functions only
    * both of them
 4. Optionally, check `Function parameter hint on input` to display parameters while typing a function name (c.f. picture below)

[![Calltip][1]][1]


To use it, start to type a word or a function and after the number of characters you have specified, the suggestions will appear:

[![Suggestions][2]][2]
 
Some documentation about auto-completion is available here: http://docs.notepad-plus-plus.org/index.php/Auto_Completion


  [1]: http://i.stack.imgur.com/rkwMP.png
  [2]: http://i.stack.imgur.com/FUHWa.png

## Customization of language suggestion (function parameter hint)
Parameters hints can be customized by the user as indicated in this link: http://docs.notepad-plus-plus.org/index.php/Auto_Completion#How_to_create_keyword_auto-completion_definition_files

> **How to create keyword auto-completion definition files**
>
> Since version 5.0 Notepad++ has support for so called Calltips, and has introduced a new way of storing autocomplete data. Now everything is stored in the XML format, which allows for easy extension of functionality. By doing so, autocomplete and calltip data are combined in a single file. Older .api plain text files are no longer used by Notepad++, and can be safely deleted if present.
> 
> You can choose what srt of auto-completion you wish to have, from `Settings -> Preferences -> Auto Completion -> Enable Auto-completion` on each input: words from the current document, functions from the current language, or both.
> 
> The AutoComplete files are located in the "plugins\APIs" folder, to be found in the Notepad++ Install Folder, most often `C:\Program Files\Notepad++`.
> 
> The syntax of AutoComplete files is simple, but does have a few rules, most importantly correct syntax and proper sorting. If the syntax is incorrect, the XML file will fail to load and AutoComplete will be disabled. A more formal description can be found at [Editing Auto-Completion files][1].
> 
> Improper sorting (see below) can cause the AutoComplete function to behave erratic, causing it to fail on certain words.
> 
> The basic character set used to recognise keywords is made of letters (i.e. `a-z`, `A-Z`, `0-9` digits and the `underscore`). [...]

<!-- language: xml -->

    <?xml version="1.0" encoding="Windows-1252" ?>
    <NotepadPlus>
       <AutoComplete language="C++">
           <Environment ignoreCase="no" startFunc="(" stopFunc=")" paramSeparator="," terminal=";" additionalWordChar = "."/>
           <KeyWord name="abs" func="yes">
               <Overload retVal="int" descr="Returns absolute value of given integer">
                   <Param name="int number" />
               </Overload>
           </KeyWord>
       </AutoComplete>
    </NotepadPlus>

> A small example of how the XML file is built is given above. NotepadPlus, AutoComplete and Environment are singleton elements, there should be only one of each, and all of them should be present for correctness, although it is allowed to remove the `<Environment>` element. Doing so will default all values to the ones given in the above example.
> 
> For keywords that are not functions, the Keyword tag is autoclosing and only has the `name` attribute. To indicate a keyword can be displayed in a calltip, add the attribute `func` with the value `yes`. In this case, the Keyword tag is a node and contains other tags.
> 
> Then, for each overload of the function, an Overload element should be added ,which specifies the behavior and the parameters of the function. A function must have at least one Overload or it will not be displayed as a calltip. The `retVal` attribute must be present and specifies the type of the return value, but the `descr` attribute is optional and describes the functions behavior, like a comment. 
> 
> You can add newlines in the description if you wish to do so. For each parameter the function takes, a `Param` element can be added. The `name` attribute must be present and specifies the type of the parameters and/or any name of the parameter.
> 
> In the `AutoComplete` element you can add the `language` attribute but it is not used by Notepad++, you can add it for completeness if you wish and can take any string you want.


  [1]: http://docs.notepad-plus-plus.org/index.php/Editing_Configuration_Files#API_files

