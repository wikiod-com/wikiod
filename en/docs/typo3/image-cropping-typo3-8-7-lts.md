---
title: "Image cropping TYPO3 8.7 LTS"
slug: "image-cropping-typo3-87-lts"
draft: false
images: []
weight: 9905
type: docs
toc: true
---

## Usage and configuration of image cropping
PageTS Settings:

    ## Default Image cropping ##
    TCEFORM.sys_file_reference.crop.config.cropVariants {
        default {
            title = Desktop
            selectedRatio = NaN
            allowedAspectRatios {
                NaN {
                    title = Free
                    value = 0.0
                }
                21:9 {
                    title = 21:9
                    value = 2.3333333
                }
                16:9 {
                    title = 16:9
                    value = 1.7777777
                }
                4:3 {
                    title = 4:3
                    value = 1.3333333
                }
                3:2 {
                    title = 3:2
                    value = 1.5
                }
                1:1 {
                    title = 1:1
                    value = 1
                }
            }
        }
        tablet {
            title = Tablet
            selectedRatio = NaN
            allowedAspectRatios {
                NaN {
                    title = Free
                    value = 0.0
                }
                21:9 {
                    title = 21:9
                    value = 2.3333333
                }
                16:9 {
                    title = 16:9
                    value = 1.7777777
                }
                4:3 {
                    title = 4:3
                    value = 1.3333333
                }
                3:2 {
                    title = 3:2
                    value = 1.5
                }
                1:1 {
                    title = 1:1
                    value = 1
                }
            }
        }
        mobile {
            title = Mobile
            selectedRatio = NaN
            allowedAspectRatios {
                NaN {
                    title = Free
                    value = 0.0
                }
                21:9 {
                    title = 21:9
                    value = 2.3333333
                }
                16:9 {
                    title = 16:9
                    value = 1.7777777
                }
                4:3 {
                    title = 4:3
                    value = 1.3333333
                }
                3:2 {
                    title = 3:2
                    value = 1.5
                }
                1:1 {
                    title = 1:1
                    value = 1
                }
            }
        }
    }
    ## Default Image cropping  - END ##

Fluid Template Example:

    <f:for each="{images}" as="image">
        <picture>
            <source srcset="{f:uri.image(image: image, maxWidth: settings.maxImgWidth, cropVariant: 'default')}" media="(min-width: 1200px)">
            <source srcset="{f:uri.image(image: image, maxWidth: '992', cropVariant: 'default')}, {f:uri.image(image: image, maxWidth: '1984', cropVariant: 'default')} 2x" media="(min-width: 992px)">
            <source srcset="{f:uri.image(image: image, maxWidth: '768', cropVariant: 'tablet')}, {f:uri.image(image: image, maxWidth: '1536', cropVariant: 'tablet')} 2x" media="(min-width: 768px)">
            <source srcset="{f:uri.image(image: image, maxWidth: '768', cropVariant: 'mobile')}, {f:uri.image(image: image, maxWidth: '1536', cropVariant: 'mobile')} 2x" media="(max-width: 767px)">
            <!---Fallback--->
            <img class="img-responsive" src="{f:uri.image(image: image, maxWidth: settings.maxImgWidth, cropVariant: 'default')}" alt="{image.alternative}" longdesc="{image.description}" title="{image.title}">
        </picture>
    </f:for>

