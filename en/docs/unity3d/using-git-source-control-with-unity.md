---
title: "Using Git source control with Unity"
slug: "using-git-source-control-with-unity"
draft: false
images: []
weight: 9718
type: docs
toc: true
---

## Setting up a Git repository for Unity
When initializing a Git repository for Unity development, there are a couple of things that need to be done.

Unity Ignore Folders
======================
Not everything should be versioned in the repository. You can add the template below to your `.gitignore` file in the root of your repository. Or alternatively, you can check the open source [Unity .gitignore on GitHub][1] and alternatively generate one using [gitignore.io for unity.][2]

    # Unity Generated
    [Tt]emp/
    [Ll]ibrary/
    [Oo]bj/

    # Unity3D Generated File On Crash Reports
    sysinfo.txt

    # Visual Studio / MonoDevelop Generated
    ExportedObj/
    obj/
    *.csproj
    *.unityproj
    *.sln
    *.suo
    *.tmp
    *.user
    *.userprefs
    *.pidb
    *.booproj
    *.svd

    # OS Generated
    desktop.ini
    .DS_Store
    .DS_Store?
    .Spotlight-V100
    .Trashes
    ehthumbs.db
    Thumbs.db

To learn more about how to setup a .gitignore file, [check out here][3].

Unity Project Settings
========================

By default Unity projects aren't setup to support versioning correctly.

1. (Skip this step in v4.5 and up) Enable `External` option in `Unity → Preferences → Packages → Repository`. 
2. Switch to `Visible Meta Files` in `Edit → Project Settings → Editor → Version Control Mode`.
3. Switch to `Force Text` in `Edit → Project Settings → Editor → Asset Serialization Mode`.
4. Save the scene and project from `File` menu.


Additional Configuration
========================

One of the few major annoyances one has with using Git with Unity projects is that Git doesn't care about directories and will happily leave empty directories around after removing files from them. Unity will make `*.meta` files for these directories and can cause a bit of a battle between team members when Git commits keep adding and removing these meta files.

[Add this Git post-merge hook][4] to the `/.git/hooks/` folder for repositories with Unity projects in them. After any Git pull/merge, it will look at what files have been removed, check if the directory it existed in is empty, and if so delete it.


  [1]: https://github.com/github/gitignore/blob/master/Unity.gitignore
  [2]: https://www.gitignore.io/api/unity
  [3]: https://www.wikiod.com/git/ignoring-files-and-folders
  [4]: https://github.com/strich/git-dir-cleaner-for-unity3d

## Scenes and Prefabs merging
A common problem when working with Unity is when 2 or more developers are modifying a Unity scene or prefab (*.unity files). Git does not know how to merge them correctly out of the box.
Thankfully the Unity team deployed a tool called [SmartMerge][1] which makes simple merge automatic. 
The first thing to do is to add the following lines to your `.git` or `.gitconfig` file:
(Windows: `%USERPROFILE%\.gitconfig`, Linux/Mac OS X: `~/.gitconfig`)

    [merge]
    tool = unityyamlmerge

    [mergetool "unityyamlmerge"]
    trustExitCode = false
    cmd = '<path to UnityYAMLMerge>' merge -p "$BASE" "$REMOTE" "$LOCAL" "$MERGED"

On **Windows** the path to UnityYAMLMerge is :

    C:\Program Files\Unity\Editor\Data\Tools\UnityYAMLMerge.exe

or

    C:\Program Files (x86)\Unity\Editor\Data\Tools\UnityYAMLMerge.exe
 
and on **MacOSX** : 

    /Applications/Unity/Unity.app/Contents/Tools/UnityYAMLMerge

Once this is done, the mergetool will be available when conflicts arise during merge/rebase. Don't forget to run `git mergetool` manually to trigger UnityYAMLMerge.


  [1]: https://docs.unity3d.com/Manual/SmartMerge.html

## Using Git Large File Storage (LFS) with Unity
Foreword
========

Git can work with video game development out of the box. However the main caveat is that versioning large (>5 MB) media files can be a problem over the long term as your commit history bloats - Git simply wasn't originally built for versioning binary files.

The great news is that since mid-2015 GitHub has released a plugin for Git called [Git LFS][1] that directly deals with this problem. You can now easily and efficiently version large binary files!

Finally, this documentation is focused on the specific requirements and information necessary to ensure your Git life works well with video game development. This guide will not cover how to use Git itself.

Installing Git & Git-LFS
========================

You have a number of options available to you as a developer and the first choice is whether to install the core Git command-line or let one of the popular Git GUI applications deal with it for you.

Option 1: Use a Git GUI Application
-----------------------------------

This is really a personal preference here as there are quite a few options in terms of Git GUI or whether to use a GUI at all. You have a number of applications to choose from, here are 3 of the more popular ones:

 - [Sourcetree (Free)][2]
 - [Github Desktop (Free)][3]
 - [SmartGit (Commerical)][4]

Once you've installed your application of choice, please google and follow instructions on how to ensure it is setup for Git-LFS. We'll be skipping this step in this guide as it is application specific.

Option 2: Install Git & Git-LFS
-------------------------------

This is pretty simple - [Install Git][5]. Then. [Install Git LFS][1].

  [1]: https://git-lfs.github.com/
  [2]: https://www.sourcetreeapp.com/
  [3]: https://desktop.github.com/
  [4]: http://www.syntevo.com/smartgit/
  [5]: https://git-scm.com/downloads


Configuring Git Large File Storage on your project
=================================

If you're using the Git LFS plugin to give better support for binary files, then you'll need to set some file types to be managed by Git LFS. Add the below to your `.gitattributes` file in the root of your repository to support common binary files used in Unity projects:

    # Image formats:
    *.tga filter=lfs diff=lfs merge=lfs -text
    *.png filter=lfs diff=lfs merge=lfs -text
    *.tif filter=lfs diff=lfs merge=lfs -text
    *.jpg filter=lfs diff=lfs merge=lfs -text
    *.gif filter=lfs diff=lfs merge=lfs -text
    *.psd filter=lfs diff=lfs merge=lfs -text
    
    # Audio formats:
    *.mp3 filter=lfs diff=lfs merge=lfs -text
    *.wav filter=lfs diff=lfs merge=lfs -text
    *.aiff filter=lfs diff=lfs merge=lfs -text
    
    # 3D model formats:
    *.fbx filter=lfs diff=lfs merge=lfs -text
    *.obj filter=lfs diff=lfs merge=lfs -text
    
    # Unity formats:
    *.sbsar filter=lfs diff=lfs merge=lfs -text
    *.unity filter=lfs diff=lfs merge=lfs -text
    
    # Other binary formats
    *.dll filter=lfs diff=lfs merge=lfs -text


