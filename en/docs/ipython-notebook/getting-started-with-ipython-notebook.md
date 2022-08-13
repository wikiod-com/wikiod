---
title: "Getting started with ipython-notebook"
slug: "getting-started-with-ipython-notebook"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting ipython-notebook set up or installed:

***Please Note:*** iPython Notebook is now *no longer supported* as all of the functionality has been moved into to the **[Jupyter][1]** project.

Prerequisite
---
To install Jupyter or iPython Notebook you must have python installed at at least version 2.7.3+ or 3.3+ - python can be installed from the [main python site][2], from your Linux/OS-X distribution or as one of the bundled packages such as [Anaconda][3] *recommended*.

Installing Jupyter Notebook
---
To install the Jupyter Notebook:

 - In Anaconda: Ensure you are running a recent version of Anaconda for Python 3 and you already have Jupyter and it's Notebook installed just run with `jupyter notebook`
 - Using pip *Linux and OS-X users may need to prefix all the following commands with `sudo`*:
  - Update `pip` with: `python -m pip install --upgrade pip`
  - Install Jupyter with: `pip install --upgrade jupyter`
  - Run with: `jupyter notebook` to start the server and you should see the "home" interface in your browser:

[![Jupyter Notebook][4]][4]

For instructions on how to enable additional kernals see the Jupyter site.

*Note:* There is currently, 2016, a new, *next generation*, user interface for Jupyter under active development called [Jupyter Lab][5] which is worth watching:

[![Lab Interface][6]][6]

Legacy Systems
---
On legacy systems iPython it ***may*** be possible to install iPython notebook to a python system with `pip` installed use the command:

    pip install ipython[notebook]

Every time that you run ipython notebook you will receive a warning:

    [TerminalIPythonApp] WARNING | Subcommand `ipython notebook` is deprecated and will be removed in future versions.
    [TerminalIPythonApp] WARNING | You likely want to use `jupyter notebook` in the future

and find that you **are** running Jupyter.

***Note on Importing Libraries***

On some systems, *notably OS-X or when running from a Virtual Environment*, the notebook ***may*** not recognize some of the libraries installed on the underlying system, *this is due to the system python and that being used to run the notebook being different installations*. Adding the libraries can be done independently in the notebooks environment by the following command from within the iPython environment:

    !pip install [desired library]  

Or with the following code:

    import pip
    pip.main(['install', 'libary-name'])  # '-U' can be added after install to update existing packages


  [1]: https://jupyter.org/
  [2]: https://www.python.org/
  [3]: https://www.continuum.io/downloads
  [4]: https://i.stack.imgur.com/fkzZb.png
  [5]: https://github.com/jupyterlab/jupyterlab
  [6]: https://i.stack.imgur.com/YV2WT.png

