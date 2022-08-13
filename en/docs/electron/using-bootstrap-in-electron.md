---
title: "Using bootstrap in electron"
slug: "using-bootstrap-in-electron"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

One of the best front-end frameworks in the web world in twitter bootstrap. As electron is relies on web browser, we can easily use bootstrap with electron in order to use the power of bootstrap in our electron framework.
 The latest version of bootstrap as of today is 3.3.7 and bootstrap 4 is still in alpha phase.

## Linking Electron with Bootstrap
In order to use bootstrap, there are 2 cases.
1. The electron app is connected to internet
2. The electron app is not connected to internet

For electron apps that are connected to internet, we can just make use of CDN links for bootstrap and include that in our html files.

The problem comes when we have to take it to offline version where the app is not connected to the net.
In that case, 

 1. Download bootstrap from [Bootstrap][1]
 2. Unzip the folder into the electron app 
 3. In the bootstrap directory, there are css and javascript files.
 4. For better understanding, move the bootstrap css files into the CSS folder ( All the styling files will be in this folder) and bootstrap js files to JS folder ( All the Javascript files will be in this folder)
 5. In your html files , link the html files using the following code
   

    <link rel="stylesheet" href="path_to_the_offline_bootstrap_css_file">
    <script scr="path_to_the_offline_bootstrap_js_file"></script>

In this way you can start using twitter bootstrap in electron framework.

  [1]: http://getbootstrap.com/

