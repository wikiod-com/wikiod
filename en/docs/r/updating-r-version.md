---
title: "Updating R version"
slug: "updating-r-version"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Installing or Updating your Software will give access to new features and bug fixes. Updating your R installation can be done in a couple of ways. One Simple way is go to [R website](https://cran.r-project.org/) and download the latest version for your system.

## Installing from R Website
To get the latest release go to https://cran.r-project.org/ and download the file for your operating system. Open the downloaded file and follow the on-screen installation steps. All the settings can be left on default unless you want to change a certain behaviour.

## Updating from within R using installr Package
You can also update R from within R by using a handy package called **installr**. 

Open R Console (NOT RStudio, this doesn't work from RStudio) and run the following code to install the package and initiate update.

    install.packages("installr")
    library("installr")
    updateR()


 [![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/UMl3T.png

## Deciding on the old packages
Once the installation is finished click the Finish button.

Now it asks if you want to copy your packages fro the older version of R to Newer version of R. Once you choose yes all the package are copied to the newer version of R.

[![enter image description here][1]][1]

After that you can choose if you still want to keep the old packages or delete.

[![enter image description here][2]][2]

 
You can even move your Rprofile.site from older version to keep all your customised settings.

[![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/ytDqh.png
  [2]: https://i.stack.imgur.com/zK6L9.png
  [3]: https://i.stack.imgur.com/ffK8W.png

## Updating Packages
You can update your installed packages once the updating of R is done.

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/rQbIt.png

Once its done Restart R and enjoy exploring.

## Check R Version
You can check R Version using the console

    version

