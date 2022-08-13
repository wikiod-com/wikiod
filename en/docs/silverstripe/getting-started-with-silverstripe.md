---
title: "Getting started with silverstripe"
slug: "getting-started-with-silverstripe"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
SilverStripe can be installed via composer or through the extraction of downloaded zip file.

To install through composer we run the following command

    composer create-project silverstripe/installer /path/to/project 3.4.0

A download zip file can be found on the [download page][1] of the SilverStripe website. Once downloaded, this file needs to be extracted into the root directory of the desired project.

Upon visiting the website for the first time an installation wizard will be presented to configure and set up the SilverStripe install.

  [1]: http://www.silverstripe.org/download

## Customising the CMS / White Labeling
The SilverStripe CMS can be customised to change the CMS logo, link and application name.

This can be achieved with the following `config.yml` settings

    LeftAndMain:
      application_name: 'My Application'
      application_link: 'http://www.example.com/'
      extra_requirements_css:
        - mysite/css/cms.css

**mysite/css/cms.css**

    .ss-loading-screen {
        background: #fff;
    }
    .ss-loading-screen .loading-logo {
        background: transparent url('../images/my-logo-loading.png') no-repeat 50% 50%;
    }
    .cms-logo a {
        background: transparent url('../images/my-logo-small.png') no-repeat left center;
    }



