---
title: "TypoScript"
slug: "typoscript"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Syntax
 - #one line comment
 - /* multiple line comments */
 - parameter = value
 - parameter.property = value2
 - parameter.property > # delete property
 - parameter.property2 < parameter.property # copy (deep) properties
 - parameter.property := addToList(35) # Add value to comma separated list
 - parameter.property\\.with\\.dots = value # Escape dots to allow properties with dots

## Basic TypoScript : Hello World
    page = PAGE
    page.10 = TEXT
    page.10.value = HELLO WORLD


Usually this typoScript snippets are added to Web >> Template >> Info/Modify >> setup

This snippet opens a new `PAGE` object. Inside the `PAGE` object, the 10th entry is set to be a `TEXT` object. The value of thus `TEXT` object is set to `HELLO WORLD`. It is convention to only use the increments of `10` to define `content objects` in order to allow more entries to be added at a later time.

## Image and Image Resource
> Get image from file

    lib.myImage = IMAGE
    lib.myImage.file = fileadmin/My-Image.png
    lib.myImage.file.width = 100
    lib.myImage.file.height = 100

> Get image from page properties

    lib.pageImage = IMAGE
    lib.pageImage {
        file.import.data = levelmedia: -1, "slide"
        file.import = uploads/media/
        file.import.listNum = 0
        file.import.override.field = media
    
        border = 0
        altText = xy
        titleText = xy
    }

> get images from page resources (FAL)

    lib.pageResources = FILES 
    lib.pageResources { 
      references { 
        table = pages 
        uid.data = uid
        fieldName = media
      } 
      renderObj = IMAGE 
      renderObj {
        file { 
          import.data = file:current:uid 
          treatIdAsReference = 1 
          width = 150c 
          height = 150c 
        } 
        altText.data = file:current:alternative
        titleText.data = file:current:title
      } 
      maxItems = 3
    }

> Get image resource

    lib.myImage = IMAGE
    lib.myImage.file = fileadmin/My-Image.png
    lib.myImage.file.width = 100
    lib.myImage.file.height = 100
    lib.myImage.stdWrap.wrap (
     <div style="background-image:url(|); width:100px; height:100px;">Headline</div>
    )

> Get images from content elements

    lib.ceImages = FILES
    lib.ceImages {
        stdWrap.wrap = <div class="pic">|</div>
        references {
            table = tt_content
            // current CE (needs context)
            #uid.data = uid
            // for CSC
            fieldName = image
            // for FSC
            fieldname = assets
        }
        renderObj = IMAGE
        renderObj {
            file {
                import.data = file:current:uid
                treatIdAsReference = 1
                width = 150c
                height = 150c
            }
            altText.data = file:current:alternative
            titleText.data = file:current:title
            stdWrap.typolink.parameter.data = file:current:link
        }
        maxItems = 5
    }

## Get date in object
> Get current date and time

    lib.date = TEXT
    lib.date {
      data = date:U
      strftime = %d.%m.%Y %H:%M:%S
      wrap = Today is |
    }

> Get last login time and date from fe_users

    lib.date = TEXT
    lib.date {
      data = TSFE:fe_user|user|lastlogin
      strftime = %d.%m.%Y %H:%M:%S
      wrap = Last login is at |
    }

> Consider url to be http://test.com/page1/?tstamp=1469683852

    lib.date = TEXT
    lib.date {
      data = GP : tstamp
      strftime = %d.%m.%Y %H:%M:%S
      wrap = Current tstamp is |
    }

## Create link 
> A link to any text by using typolink object

    lib.link = TEXT
    lib.link {
        value = Here is link text
        
        typolink {
            
            #You can give page uid or any external url here
            parameter = http://www.example.com/
    
            #Target of link
            extTarget = _blank
    
            #Additional parameters bound to link
            ATagParams = class="linkclass" title="Here is a link"
        }
    }

> Following will create link with javascript popup

    lib.link = TEXT
    lib.link {
         value = Open a popup window.
    
         stdWrap.typolink {
              # The first parameter is the page ID of the target page,
              # second parameter is the size of the popup window.
              parameter = 10 500x400
    
              # The title attribute of the link.
              title = Click here to open a popup window.
    
              # The parameters of the popup window.
              JSwindow_params = menubar=0, scrollbars=0, toolbar=0, resizable=1
    
         }
    }

## Get current page title
    lib.pagetitle = TEXT
    lib.pagetitle.data = page : title

## Get Current URL
    lib.currentURL= TEXT
    lib.currentURL.data = getIndpEnv:TYPO3_REQUEST_URL

