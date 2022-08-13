---
title: "Basic typoscript for TYPO3 installation"
slug: "basic-typoscript-for-typo3-installation"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Here is the lisf of some basic typoscript for TYPO3 installation.

## Require Config Typoscript.
    config {
        simulateStaticDocuments = 0
        index_enable = 1
        index_externals = 1
        linkVars = L
        sys_language_mode = content_fallback
        sys_language_overlay = hideNonTranslated
        doctype = xhtml_trans
        xhtml_cleaning = all
        doctype = html5
        xmlprologue = none
        renderCharset = utf-8
        no_cache = 0
        locale_all = de-DE
        sys_language_uid = 0
        htmlTag_langKey = de-DE
        language = de-DE
        metaCharset = utf-8
    }

    page.meta.description = {page:description}
    page.meta.description.insertData = 1
    page.meta.keywords = {page:keywords}
    page.meta.keywords.insertData = 1
    page.meta.viewport = width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0;
    page.meta.format-detection=telephone=no
    page.meta.charset = UTF-8
    page.meta.X-UA-Compatible = IE=edge
    page = PAGE
    page.shortcutIcon = images/favicon.ico

## get Page content :
    # CONTENT: Main content
    lib.content.main = COA
    lib.content.main {
        stdWrap.innerWrap = <!--TYPO3SEARCH_begin-->|<!--TYPO3SEARCH_end-->
        10 < styles.content.get
    }
    lib.content.0 < lib.content.main
    
    # CONTENT: Sidebar
    lib.content.left = COA
    lib.content.left {
        stdWrap.innerWrap = <!--TYPO3SEARCH_begin-->|<!--TYPO3SEARCH_end-->
        10 < styles.content.getLeft
    }
    lib.content.1 < lib.content.left
    
    # CONTENT: Sidebar
    lib.content.right = COA
    lib.content.right {
        stdWrap.innerWrap = <!--TYPO3SEARCH_begin-->|<!--TYPO3SEARCH_end-->
        10 < styles.content.getRight
    }
    lib.content.2 < lib.content.right
    
    # CONTENT: Top Content
    lib.content.top = COA
    lib.content.top {
        stdWrap.innerWrap = <!--TYPO3SEARCH_begin-->|<!--TYPO3SEARCH_end-->
        10 < styles.content.getBorder
    }
    lib.content.3 < lib.content.top

## Add css and js file:
    page.includeCSS {
        bootstrap = fileadmin/css/bootstrap.min.css
        fonts = fileadmin/css/font-awesome.min.css
        owl = fileadmin/css/owl.carousel.css
        style = fileadmin/css/docs.css
    }

    page.includeJSFooter{
        bootstrapmin = fileadmin/js/bootstrap.min.js
        lightbox = fileadmin/js/lightbox-plus-jquery.min.js
        owl = fileadmin/js/owl.carousel.min.js
        custom = fileadmin/js/custom.js
    }
    
    page.includeJS {
      jqueryMin = EXT:website_template/Resources/Public/js/jquery-1.11.2.min.js
    }

## Remove Controller and Action name In URL
    plugin.tx_news {
        settings {
            link {
                 skipControllerAndAction = 1
            }
        }
    }
    
    [globalVar = GP:tx_news_pi1|news > 0]
        config.defaultGetVars {
            tx_news_pi1 {
                    controller=News
                    action=detail
            }
        }
    [global]

## include typoscript:
    <INCLUDE_TYPOSCRIPT: source="FILE:fileadmin/html/mainmenu_typoscript.txt">
    <INCLUDE_TYPOSCRIPT: source="DIR:fileadmin/templates/" extensions="ts">

