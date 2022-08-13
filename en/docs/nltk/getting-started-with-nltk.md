---
title: "Getting started with nltk"
slug: "getting-started-with-nltk"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## NLTK's download function
You can install NLTK over `pip` (`pip install nltk`).After it is installed, many components will not be present, and you will not be able to use some of NLTK's features.


From your Python shell, run the function `ntlk.download()` to select which additional  packages you want to install using UI. Alternatively, you can use `python -m nltk.downloader [package_name]`.


----------

 -  To download all packages available.


    nltk.download('all')

----------

 -  To download specific package.


    nltk.download('package-name')


----------


 - To download all packages of specific folder.


    import nltk

    dwlr = nltk.downloader.Downloader()

    # chunkers, corpora, grammars, help, misc, 
    # models, sentiment, stemmers, taggers, tokenizers
    for pkg in dwlr.packages():
        if pkg.subdir== 'taggers':
            dwlr.download(pkg.id)


----------


 - To download all packages except Corpora Folder.


    import nltk

    dwlr = nltk.downloader.Downloader()

    for pkg in dwlr.corpora():
        dwlr._status_cache[pkg.id] = 'installed'

    dwlr.download('all')


## Installation or Setup
NLTK requires `Python` versions **2.7** or **3.4+**.

These instructions consider `python` version - **3.5**

----------


 - **Mac/Unix :**

     1. Install NLTK: run `sudo pip install -U nltk`
     2. Install Numpy (optional): run `sudo pip install -U numpy`
     3. Test installation: run `python` then type `import nltk`

     *NOTE : For older versions of Python it might be necessary to install setuptools (see http://pypi.python.org/pypi/setuptools) and to install pip (sudo easy_install pip).*


----------


 - **Windows :**

      These instructions assume that you do not already have Python installed on your machine.

      *32-bit binary installation*

      1. Install Python 3.5: http://www.python.org/downloads/ (avoid the 64-bit versions)
      2. Install Numpy (optional): http://sourceforge.net/projects/numpy/files/NumPy/ (the version that specifies pythnon3.5)
      3. Install NLTK: http://pypi.python.org/pypi/nltk
      4. Test installation: `Start>Python35`, then type `import nltk`


----------


 - **Installing Third-Party Software :**

      Please see: https://github.com/nltk/nltk/wiki/Installing-Third-Party-Software


----------


  **Reference :** http://www.nltk.org/install.html





## NLTK installation with Conda.

To install NLTK with Continuum's `anaconda` / `conda`.

If you are using Anaconda, most probably nltk would be already downloaded in the root (though you may still need to download various packages manually). 

Using `conda`:

    conda install nltk 


To upgrade `nltk` using `conda`:

    conda update nltk

With `anaconda`:

If you are using multiple python envriroments in anaconda, first activate the enviroment where you want to install nltk. You can check the active enviroment using the command

    conda info --envs

The enviroment with the * sign before the directory path is the active one.
 To change the active enviroment use 

    activate <python_version>
    for eg. activate python3.5

Now check the list of packages installed in this enviroment using commnad 

    conda list

If you dont find 'nltk' in the list, use 


    conda install -c anaconda nltk=3.2.1


For further information, you may consult https://anaconda.org/anaconda/nltk.

----

To install mini-conda a.k.a. `conda`: http://conda.pydata.org/docs/install/quick.html

To install `anaconda`: https://docs.continuum.io/anaconda/install

## With NLTK
You can use NLTK (especially, the [`nltk.tokenize`](http://www.nltk.org/api/nltk.tokenize.html) package) to perform sentence boundary detection:

    import nltk
    text = "This is a test. Let's try this sentence boundary detector."
    text_output = nltk.tokenize.sent_tokenize(text)
    print('text_output: {0}'.format(text_output))

Output:

    text_output: ['This is a test.', "Let's try this sentence boundary detector."]



## Basic Terms


