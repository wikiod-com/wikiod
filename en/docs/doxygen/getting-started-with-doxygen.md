---
title: "Getting started with doxygen"
slug: "getting-started-with-doxygen"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting doxygen set up or installed.

## Commenting your code
There are several ways to mark a comment block as a detailed description, so that this comment block is parsed by Doxygen and added as a description of the following code item to the documentation. The first and most common one are C style comments with an extra asterisk in the comment start sequence, e.g.:

    /**
     * … text …
     */
    int dummy_var;

The next alternative is to use the Qt style and add an exclamation mark (!) after the opening sequence of a C-style comment block: 

    /*!
     * … text …
     */
    void foo(void);

A third alternative is to use a block of at least two C++ comment lines, where each line starts with an additional slash or an exclamation mark:

    /// 
    /// ... text ... 
    ///

or

    //! 
    //! ... text ... 
    //!

Some people like to make their comment blocks more visible in the documentation. For this purpose you can use the following:

     /********************************************//** 
      * ... text 
      ***********************************************/

Note the 2 slashes to end the normal comment block and start a special comment block.

    /////////////////////////////////////////////////
    /// ... text ...
    /////////////////////////////////////////////////

To structure and fomat the generated documentation, Doxygen provides a large number (> 170) of special commands. All commands in the documentation start with a backslash (\) or an at-sign (@).

For example

    /**
     * \brief    A brief description in one short sentence.
     */

is equivalent to

    /**
     * @brief    A brief description in one short sentence.
     */

For the brief description there are also several possibilities: 

One could use the `\brief` command with one of the above comment blocks. This command ends at the end of a paragraph, so the detailed description follows after an empty line.

    /** \brief Brief description. 
     * Brief description continued. 
     * 
     * Detailed description starts here. 
     */ 

If `JAVADOC_AUTOBRIEF` is set to `YES` in the configuration file, then using JavaDoc style comment blocks will automatically start a brief description which ends at the first dot followed by a space or new line.

    /// Brief description which ends at this dot. Details follow 
    /// here. 

And finally here an example for a full documentation of a function with doxygen:

    /**
     * \brief   The function bar. 
     *
     * \details This function does something which is doing nothing. So this text
     *          is totally senseless and you really do not need to read this,
     *          because this text is basically saying nothing.
     *
     * \note    This text shall only show you, how such a \"note\" section
     *          is looking. There is nothing which really needs your notice,
     *          so you do not really need to read this section.
     *
     * \param[in]     a    Description of parameter a.
     * \param[out]    b    Description of the parameter b.
     * \param[in,out] c    Description of the parameter c.
     *
     * \return        The error return code of the function.
     *
     * \retval        ERR_SUCCESS    The function is successfully executed
     * \retval        ERR_FAILURE    An error occurred
     */

    errcode_t bar(int a, int b, int c)
    {    
        /** More detailed description inside the code */
    }

source and more info on the [Doxygen homepage][1]


  [1]: http://www.stack.nl/~dimitri/doxygen/manual/index.html

