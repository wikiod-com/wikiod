---
title: "Installing packages"
slug: "installing-packages"
draft: false
images: []
weight: 9755
type: docs
toc: true
---

## Syntax
- install.packages(pkgs, lib, repos, method, destdir, dependencies, ...)


## Parameters
| Parameter|Details|
|:--------:|:-----|
|pkgs|character vector of the names of packages. If `repos = NULL`, a character vector of file paths.|
|lib|character vector giving the library directories where to install the packages.|
|repos|character vector, the base URL(s) of the repositories to use, can be `NULL` to install from local files
|method|download method|
|destdir|directory where downloaded packages are stored|
|dependencies|logical indicating whether to also install uninstalled packages which these packages depend on/link to/import/suggest (and so on recursively). Not used if `repos = NULL`.|
|...| Arguments to be passed to ‘download.file’ or to the functions for binary installs on OS X and Windows.|

# Related Docs
- https://www.wikiod.com/r/inspecting-packages

## Download and install packages from repositories
    
Packages are collections of R functions, data, and compiled code in a [well-defined format](http://r-pkgs.had.co.nz/description.html). Public (and private) repositories are used to host collections of R packages. The largest collection of R packages is available from CRAN.

# Using CRAN
A package can be installed from [CRAN](https://cran.r-project.org/) using following code:

    install.packages("dplyr")

Where `"dplyr"` is referred to as a character vector. 

More than one packages can be installed in one go by using the combine function `c()` and passing a series of character vector of package names:

    install.packages(c("dplyr", "tidyr", "ggplot2"))

In some cases, `install.packages` may prompt for a CRAN mirror or fail, depending on the value of `getOption("repos")`. To prevent this, specify a [CRAN mirror](https://cran.r-project.org/mirrors.html) as `repos` argument:

    install.packages("dplyr", repos = "https://cloud.r-project.org/") 

Using the `repos` argument it is also possible to install from other repositories.
For complete information about all the available options, run `?install.packages`.

Most packages require functions, which were implemented in other packages (e.g. the package `data.table`). In order to install a package (or multiple packages) with all the packages, which are used by this given package, the argument `dependencies` should be set to `TRUE`):

    install.packages("data.table", dependencies = TRUE)

# Using Bioconductor
[Bioconductor](https://www.bioconductor.org) hosts a substantial collection of packages related to Bioinformatics. They provide their own package management centred around the `biocLite` function:
```
    ## Try http:// if https:// URLs are not supported
    source("https://bioconductor.org/biocLite.R")
    biocLite()
```
By default this installs a subset of packages that provide the most commonly used functionality. Specific packages can be installed by passing a vector of package names. For example, to install `RImmPort` from Bioconductor:
```
    source("https://bioconductor.org/biocLite.R")
    biocLite("RImmPort")
```

## Install packages from GitHub
To install packages directly from GitHub use the `devtools` package:

    library(devtools)
    install_github("authorName/repositoryName")

To install `ggplot2` from github:

    devtools::install_github("tidyverse/ggplot2")

The above command will install the version of `ggplot2` that corresponds to the *master* branch. To install from a different branch of a repository use the `ref` argument to provide the name of the branch. For example, the following command will install the `dev_general` branch of the `googleway` package.
    
    devtools::install_github("SymbolixAU/googleway", ref = "dev_general")

Another option is to use the `ghit` package. It provides a lightweight alternative for installing packages from github:

    install.packages("ghit")
    ghit::install_github("google/CausalImpact")



To install a package that is in a **private** repository on Github, generate a __personal access token__ at <http://www.github.com/settings/tokens/> (See ?install_github for documentation on the same). Follow these steps:

1.     install.packages(c("curl", "httr"))

2.     config = httr::config(ssl_verifypeer = FALSE)

3.      install.packages("RCurl")
        options(RCurlOptions = c(getOption("RCurlOptions"),ssl.verifypeer = FALSE, ssl.verifyhost = FALSE ) )

4.     getOption("RCurlOptions")
    
    You should see the following:
          
       ssl.verifypeer ssl.verifyhost 
           
       FALSE          FALSE 

4.     library(httr)
       set_config(config(ssl_verifypeer = 0L)) 

   This prevents the common error: "Peer certificate cannot be authenticated with given CA certificates"

5. Finally, use the following command to install your package seamlessly  
    
       install_github("username/package_name",auth_token="abc")

Alternatively, set an environment variable `GITHUB_PAT`, using
```
Sys.setenv(GITHUB_PAT = "access_token")
devtools::install_github("organisation/package_name")
```
The PAT generated in Github is only visible once, i.e., when created initially, so its prudent to save that token in `.Rprofile`. This is also helpful if the organisation has many private repositories. 

## Install package from local source
To install package from local source file:

    install.packages(path_to_source, repos = NULL, type="source")

    install.packages("~/Downloads/dplyr-master.zip", repos=NULL, type="source")

Here, `path_to_source` is absolute path of local source file.

Another command that opens a window to choose downloaded zip or tar.gz source files is:

    install.packages(file.choose(), repos=NULL)
---

**Another possible way is using the *GUI based RStudio*:** 

**Step 1:** Go to **Tools**.

**Step 2:** Go to **Install Packages**.

**Step 3:** In the *Install From* set it as **Package Archive File (.zip; .tar.gz)**

**Step 4:** Then *Browse* find your package file (say crayon_1.3.1.zip) and *after some time (after it shows the **Package path and file name** in the *Package Archive* tab)*

---
Another way to install R package from local source is using `install_local()` function from devtools package.

    library(devtools)
    install_local("~/Downloads/dplyr-master.zip")




    


    



## Using a CLI package manager -- basic pacman usage
`pacman` is a simple package manager for R. 

`pacman` allows a user to compactly load all desired packages, installing any which are missing (and their dependencies), with a single command, `p_load`. `pacman` does not require the user to type quotation marks around a package name. Basic usage is as follows:

    p_load(data.table, dplyr, ggplot2)

The only package requiring a `library`, `require`, or `install.packages` statement with this approach is `pacman` itself:
 
    library(pacman)
    p_load(data.table, dplyr, ggplot2)

or, equally valid:

    pacman::p_load(data.table, dplyr, ggplot2)

In addition to saving time by requiring less code to manage packages, `pacman` also facilitates the construction of reproducible code by installing any needed packages if and only if they are not already installed.

Since you may not be sure if `pacman` is installed in the library of a user who will use your code (or by yourself in future uses of your own code) a best practice is to include a conditional statement to install `pacman` if it is not already loaded:

    if(!(require(pacman)) install.packages("pacman")
    pacman::p_load(data.table, dplyr, ggplot2)

## Install local development version of a package
While working on the development of an R package it is often necessary to install the latest version of the package. This can be achieved by first building a source distribution of the package (on the command line)

    R CMD build my_package

and then [installing it in R][1]. Any running R sessions with previous version of the package loaded will need to reload it.

    unloadNamespace("my_package")
    library(my_package)

A more convenient approach uses the `devtools` package to simplify the process. In an R session with the working directory set to the package directory

    devtools::install()

will build, install and reload the package.

  [1]: https://www.wikiod.com/r/installing-packages#Install package from local source

