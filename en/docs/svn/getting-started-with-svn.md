---
title: "Getting started with svn"
slug: "getting-started-with-svn"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Committing your local changes to the repository
To publish the changes you made in your working copy, run the [`svn commit`][1] command.

> **IMPORTANT:** Review your changes before committing them! Use [`svn status`][2] and [`svn diff`][3] to review the changes. Also, make sure you are in the correct path before performing a commit. If you updated many files across various directories, you should be at the appropriate level to include all of them beneath your location.

Here is an example of the commit command:

    svn commit -m "My Descriptive Log Message"

Alternatively, `svn ci` is the shorthand for `svn commit`

Note the `-m (--message)` option. Good commit messages help others understand why a commit was made. Also, on the server side it's possible to [enforce non-empty messages] [4], and even enforce that each commit message mentions an existing ticket in your bug tracking system.

  [1]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.commit.html
  [2]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.status.html
  [3]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.diff.html
  [4]: https://tortoisesvn.net/docs/nightly/TortoiseSVN_en/tsvn-howto-minlogmsgsize.html

## Reviewing the logs
Running `svn log` will show you all the commit messages, you probably want to review only certain revisions.

 - View the `n` most recent revisions:

    `svn log -n`


 - View a specific revision:

    `svn log -c rXXX`


 - View the paths affected:

    `svn log -v -c rXXX`

## Installation and initial setup
Install the `svn` client to start collaborating on the project that is using Subversion as its version control system.

To install Subversion, you can build it yourself from a source code release or download a binary package pre-built for your operating system. The list of sites where you can obtain a compiled Subversion client (`svn`) for various operating systems is available at the [official binary packages page][1]. If you feel like compiling the software for yourself, grab the source at the [Source Code page][2].

*With Subversion, you are not limited to using only the standard `svn` command-line client. There are some notable graphical Subversion clients for various operating systems and most of the IDEs nowadays provide robust integration with SVN right out of the box or via plugins. For the list of graphical clients, check the Wikipedia page: https://en.wikipedia.org/wiki/Comparison_of_Subversion_clients*.

Right after you install the client you should be able to run it by issuing the command `svn`. You should see the following:

    $ svn
    Type 'svn help' for usage.

Everything is mostly ready. Now you should create a local workspace called a *working copy* which is going to be connected to the remote central *repository*. In other words, you are going to *checkout a working copy*. You are going to operate with the versioned data with the help of the working copy and can publish your changes (called *committing* in SVN) so that others working on the same project can see them and benefit from your changes. To later fetch the future changes made by others from the repository, you would *update your working copy*. These basic operations are covered in other examples.

  [1]: http://subversion.apache.org/packages.html
  [2]: http://subversion.apache.org/source-code.html
  [3]: https://tortoisesvn.net/

## Creating and applying patches
A patch is a file that show the differences between two revisions or between your local repository and the last revision your repository is pointing.

To share or save a patch of your local uncommitted changes either for peer review or to apply later, do:

    svn diff > new-feature.patch

To get a patch from the differences between two revisions:

    svn diff -r NEWER_REVISION:OLDER_REVISION > feature.patch

To apply a patch, run:

    svn patch new-feature.patch

> In order to apply the patch successfully, you must run the command from the same path where the patch was created.

## Checking out a working copy
To begin making modifications to the project's data, you have to obtain a local copy of the versioned project. Use the command line `svn` client or your favorite SVN client (TortoiseSVN, for example). Your local copy of the project is called a *working copy* in Subversion and you get it by issuing the command [`svn checkout <URL>`][1] where `<URL>` is a repository URL. e.g.

    $ svn checkout https://svn.example.com/svn/MyRepo/MyProject/trunk

Alternatively, you can use `svn co <URL>` as a shorthand in order to checkout a local copy.

As a result, you will get a working copy of the `/trunk` of a project called MyProject that resides in MyRepo repository. The working copy will be located in a directory called `trunk` on your computer relative to the directory you issued the command in. 

If you wish to have a different name for your working copy you can add that as a parameter to the end of the command. e.g.

    $ svn checkout https://svn.example.com/svn/MyRepo/MyProject/trunk MyProjectSource

This will create a working copy called `MyProjectSource`.

Note that instead of checking out the trunk, you could check out some branch, private shelve or a tag (assuming they already exist in the repository); you can have unlimited number of local working copies on your machine.

You could get the working copy of the whole repository MyRepo, too. But you should refrain from doing so. Generally speaking, you do **not** need to have a working copy of the whole repository for your work because your working copy can be instantly switched to another development branch / tag / whatever. Moreover, Subversion repository can contain a number of (un)related projects and it's better to have a dedicated working copy for each of them, not a single working copy for all of the projects.


  [1]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.checkout.html

## Exporting the versioned data (plain download)
If you want to get the versioned project's data, but you don't need any of the version control capabilities offered by Subversion, you could run [`svn export <URL>`][1] command. Here is an example:
        
    $ svn export https://svn.example.com/svn/MyRepo/MyProject/trunk

As a result, you will get the project's data export, but unlike with a working copy, you won't be able to run `svn` commands on it. The export is just a plain download of the data.

If some time later you'd want to convert the downloaded data to a fully-functional working copy, run `svn checkout <URL>` to the directory where you ran the export to.


  [1]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.export.html

## Checking out a working copy at a specific revision
To get version 5394 use:

    svn co --revision r5394 https://svn.example.com/svn/MyRepo/MyProject/trunk

Or the shorter version:

    svn co -r 5394 https://svn.example.com/svn/MyRepo/MyProject/trunk

Or by using pegged revisions:

    svn co https://svn.example.com/svn/MyRepo/MyProject/trunk@5394

If already checked out, you can use the `update` command to move a to a particular revision, by doing:

    svn up -rXXX

## Updating a working copy
You are not the only person working on the project, right? This means that your colleagues are also making modifications to the project's data. To stay up to date and to fetch the modifications committed by others, you should run [`svn update`][1] command in your working copy. As a result, your working copy will sync with the repository and download the changes made by your colleagues.

Shorthand for `svn update` is `svn up`.

It is a rule to run [`svn update`][1] before committing your changes.


  [1]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.update.html

## Making changes in your local working copy
The *working copy (WC)* is your *local and private workspace* that you use to interact with the central Subversion repository. You use the working copy to modify the contents of your project and fetch changes committed by others.

The working copy contains your project's data and looks and acts like a regular directory on your local file system, but with one major difference -- the working copy tracks the status and changes of files
and directories within. You can think of the working copy as of a regular directory with a version-control flavor added by a hidden `.svn` metadata directory at its root.

Most of the time, you are going to perform modifications to the project's data by modifying the contents of the working copy. As soon as you are satisfied with the modifications and you've reviewed them thoroughly, you are ready to publish them to the central repository.

You can perform any actions with your project's data within the working copy, but operations that involve copying, moving, renaming and deleting must be performed using the corresponding `svn` commands:

* **Modifying existing files**. Modify the files as you usually do using your favorite text processor, graphics editor, audio editing software, IDE, etc. As soon as you save the changes to disk, Subversion will recognize them automatically.
* **Adding new files**. Put new files to the working copy and Subversion will recognize them as *unversioned*. It will not automatically start tracking the new files unless you run [`svn add`][1] command:

      svn add foo.cs

* **Moving files and directories**. Move files and directories using [`svn move`][2] command:

      svn move foo.cs bar.cs

* **Renaming files and directories**. Rename files and directories using [`svn rename`][2] command:

      svn rename foo.cs bar.cs

     **NOTE:** `svn rename` command is an alias of `svn move` command.

* **Copying files and directories**. Copy files and directories using [`svn copy`][3] command:

      svn copy foo.cs bar.cs

* **Deleting files and directories**. Delete files and directories using [`svn delete`][4] command:
      
      svn delete foo.cs

* **Checking the status of files and directories in the working copy**. Review your changes using [`svn status`][5] (or [`svn st`][5] for short) command:

      svn status

    > **IMPORTANT:** Always review your changes before committing them. This will help you to avoid committing unnecessary or irrelevant changes.

* **Reverting changes**. Revert your changes using [`svn revert`][6] command:
            
      svn revert foo.c

* **Reverting all changes**: From the repository's root:
            
      svn revert -R .
   > **IMPORTANT:** Reverted uncommitted changes will be lost forever. You won't be able to recover the reverted changes. Use `svn revert` with caution! If you want to keep the changes but need to revert, save them in a patch. See example of how to create and apply a patch.


  [1]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.add.html
  [2]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.move.html
  [3]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.copy.html
  [4]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.delete.html
  [5]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.status.html
  [6]: http://svnbook.red-bean.com/en/1.8/svn.ref.svn.c.revert.html

## Revert or rollback of a file
To restore a file to the latest updated svn version, i.e. undo the local changes, you can use `revert`:

    svn revert file

To restore a file to an older version (revision XXX) use `update`:

    svn update -r XXX file

**Warning**: in both cases you will lose any local changes in the file because it will be overwritten.

---

To only view the older version of a file use `cat`:

    svn cat -r XXX file

And to view the differences with your local version of the file:

    svn diff -r XXX file

## Using a password-protected repository
A Subversion repository can be configured so that certain contents or commands are only accessible to certain users.  In order to access this restricted content, you will need to specify a username and password.

Your username and password can be specified directly as part of the command:

    $ svn checkout https://svn.example.com/MyRepo/trunk --username JoeUser --password topsecret

Unfortunately, this causes your password to appear in plaintext on the console.  To avoid this possible security problem, specify a username but not a password.  Doing this will cause a password prompt to appear, allowing you to enter your password without exposing it:

    $ svn checkout https://svn.example.com/MyRepo/trunk --username JoeUser
    Password for 'JoeUser':

Providing no authentication information at all causes Subversion to prompt you for both the username and password:

    $ svn checkout https://svn.example.com/MyRepo/trunk
    Username:  JoeUser
    Password for 'JoeUser':

While the first method is less secure, it's frequently seen in automated scripts since it is difficult for many types of script to provide information to an interactive prompt.

__Note__: commands that only operate on your working copy (such as `revert` or `status`) will never require a password, only commands that require communicating with the repository server.

