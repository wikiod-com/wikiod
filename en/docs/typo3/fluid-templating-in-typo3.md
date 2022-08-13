---
title: "Fluid templating in TYPO3"
slug: "fluid-templating-in-typo3"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Typoscript setup
If you want to use your own templates (be it HTML, XML, JSON or any other format) for a TYPO3 extension using fluid templating, you can add additional template paths.

An extensions template/partial/layout paths are usually set in TypoScript setup under the properties 


    plugin.tx_<extensionkey>.view.templateRootPaths
    plugin.tx_<extensionkey>.view.partialRootPaths
    plugin.tx_<extensionkey>.view.layoutRootPaths

*(\<extensionkey> = The extension key in lowercase without underscores. 
Example: power_blog  => powerblog)* 

It is also possible to set template paths only for a specific plugin of an extension, not for the whole extension. This can be done by setting the paths 

    plugin.tx_<extensionkey>_<pluginname>.view.templateRootPaths
    plugin.tx_<extensionkey>_<pluginname>.view.partialRootPaths
    plugin.tx_<extensionkey>_<pluginname>.view.layoutRootPaths

*(\<pluginname > = The plugin name in lower case. Example: Pi1  => pi1 , AjaxCall => ajaxcall)* 

Each of these properties must be an array with numerical keys. The keys are the priority of the path: When searching for a specific template, partial or layout, fluid first looks in the path with the highest key, then in the path with the next lower key and so on, and uses the first matching template it finds.

This priorization has the benefit that one can override only some of the templates or partials of the extensions default template, and reuse the others.

Here is an example for the extension `news`. In version 4.3.0, the static TypoScript template defines these template paths:

    plugin.tx_news {
        view {
            templateRootPaths {
                0 = EXT:news/Resources/Private/Templates/
                1 = {$plugin.tx_news.view.templateRootPath}
            }
            partialRootPaths {
                0 = EXT:news/Resources/Private/Partials/
                1 = {$plugin.tx_news.view.partialRootPath}
            }
            layoutRootPaths {
                0 = EXT:news/Resources/Private/Layouts/
                1 = {$plugin.tx_news.view.layoutRootPath}
            }
        }
    }

In order to use another template, one could add this to the TypoScript setup, in addition to the static template from `news`:

    plugin.tx_news {
        view {
            templateRootPaths {
                100 = path/to/my/own/templates/
            }
            partialRootPaths {
                100 = path/to/my/own/partials/
            }
            layoutRootPaths {
                100 = path/to/my/own/layouts/
            }
        }
    }

Then templates would first be searched in `path/to/my/own/templates/`, and if not found there, then in the path defined in the TypoScript constant `{$plugin.tx_news.view.templateRootPath}`, and after that in the folder where the extensions default templates reside, `EXT:news/Resources/Private/Templates/`, which usually resolves to `typo3conf/ext/news/Resources/Private/Templates/`.

If you want to do the same, but only for the plugin `Pi1` of the extension (its only plugin as of version 4.3.0), the last example would look like this:

    plugin.tx_news_pi1 {
        view {
            templateRootPaths {
                100 = path/to/my/own/templates/
            }
            partialRootPaths {
                100 = path/to/my/own/partials/
            }
            layoutRootPaths {
                100 = path/to/my/own/layouts/
            }
        }
    }

## Example Fluid Template File
> Templates/Text.html

    <html xmlns:f="http://typo3.org/ns/TYPO3/CMS/Fluid/ViewHelpers" data-namespace-typo3-fluid="true">
    <f:layout name="Default" />
    <f:section name="Main">
    
        <f:format.html>{data.bodytext}</f:format.html>
    
    </f:section>
    </html>

> Layouts/Default.html

    <html xmlns:f="http://typo3.org/ns/TYPO3/CMS/Fluid/ViewHelpers" data-namespace-typo3-fluid="true">
    <f:spaceless>
        <f:render section="Main" optional="true" />
    </f:spaceless>
    </html>

