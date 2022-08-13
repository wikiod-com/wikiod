---
title: "Getting started with virtualenv"
slug: "getting-started-with-virtualenv"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Virtual Environment tool ([virtualenv][1]) is used to isolate different projects and their dependencies by creating individual python environments for each of them. It's like installing a package locally (and not globally), similar to  [npm package installation option][2].
Following is an example to install and test virtualenv for creating two projects (Project1-A Django application and Project2- A Flask application):
 1. Initially check if virtualenv is already installed `$ virtualenv --version`
 2. Run `$ pip install virtualenv` (for Mac and Linux) or `$ sudo apt-get install python-virtualenv` for Ubuntu, `easy_install` [for Windows][3] to install the python environment.
 3. `$ mkdir Project1` and `$ cd Project1`
 4. Run `$ virtualenv venvp1` and this would create a venvp1 folder inside Project1 directory.
 5. To activate the environment run `source venvp1/bin/activate` (if Linux) and `venvp1\Scripts\activate` (if Windows) and prompt will change to `(venvp1)Your-Computer:your_project UserName$)`
 6. Run `pip install Django` to install Django for project1 and `deactivate` (if needed) to return to the global environment.
 7. Repeat steps 3-6 for Flask application with different directory, virtualenv names and `pip install Flask` to install Flask.

Once above steps are executed (without any errors) one could (possibly and) simultaneously work between both environments without any conflicts.

Notes:

 1. [virtualenvwrapper][4] is another handy tool which is extended version of virtualenv, though the installation procedure for both is nearly same.
 2. Executing `virtualenv` command with `--no-site-packages` excludes the globally installed packages.
 3. To freeze current state of environment run `$ pip freeze > installedpkgp1.txt`. This text file contains list of installed packages (including their versions) in the current environment. If there comes a need to deploy same environment at different folder (or machine) simply executing the command `$ pip install -r installedpkgp1.txt` would create same environment.
 4. Useful commands:
  - `lsvirtualenv` - list of all environments  
  - `cdvirtualenv` - goto currently activated virtual environment   
  - `cdsitepackages` - like previous, but goes directly to `site-packages` directory   
  - `lssitepackages` - shows content of `site-packages` directory

  [1]: https://pypi.python.org/pypi/virtualenv
  [2]: https://docs.npmjs.com/getting-started/installing-npm-packages-locally
  [3]: http://flask.pocoo.org/docs/0.12/installation/#windows-easy-install
  [4]: https://virtualenvwrapper.readthedocs.io/en/latest/

