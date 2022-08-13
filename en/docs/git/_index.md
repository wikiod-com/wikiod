---
title : Git Tutorial
slug : git-tutorial
weight : 8339
draft : false
images : []
type : docs
---

Git is a free, distributed version control system which allows programmers to keep track of code changes, via "snapshots" (commits), in its current state. Utilizing commits allows programmers to test, debug, and create new features collaboratively. All commits are kept in what is known as a "Git Repository" that can be hosted on your computer, private servers, or open source websites, such at Github.

Git also allows for users to create new "branches" of the code, which allows different versions of the code to live alongside each other. This enables scenarios where one branch contains the most recent stable version, a different branch contains a set of new features being developed, and a yet another branch contains a different set of features. Git makes the process of creating these branches, and then subsequently merging them back together, nearly painless.

Git has 3 different "areas" for your code:
- **Working directory**: The area that you will be doing all of your work in (creating, editing, deleting, and organizing files)
- **Staging area**: The area where you will list the changes that you have made to the working directory 
- **Repository**: Where Git permanently stores the changes you have made as different versions of the project

Git was originally created for managing the Linux kernel source. By making them easier, it encourages small commits, forking of projects and merging between forks, and having lots of short-lived branches.

The biggest change for people who are used to CVS or Subversion is that every checkout contains not only the source tree, but also the whole history of the project. Common operations like diffing of revisions, checking out older revisions, committing (to your local history), creating a branch, checking out a different branch, merging branches or patch files can all be done locally without having to communicate with a central server. Thus the biggest source of latency and unreliability is removed. Communicating with the "upstream" repository is only needed to get the latest changes, and to publish your local changes to other developers. This turns what was previously a technical constraint (whoever has the repository owns the project) into an organisational choice (your "upstream" is whomever you choose to sync with).

