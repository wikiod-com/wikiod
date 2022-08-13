---
title: "Removing sensitive data or large files"
slug: "removing-sensitive-data-or-large-files"
draft: false
images: []
weight: 9902
type: docs
toc: true
---

If you commit sensitive data, such as a password or SSH key into a Git repository, you can remove it from the history. To entirely remove unwanted files from a repository's history you can use either the git filter-branch command or the BFG Repo-Cleaner.

 1. Tell your collaborators to rebase, not merge, any branches they
    created off of your old (tainted) repository history. One merge
    commit could reintroduce some or all of the tainted history that you
    just went to the trouble of purging.
 2. After some time has passed and you're confident that git filter-branch had no unintended side effects, you can force all objects in your local repository to be dereferenced and garbage collected with the following commands (using Git 1.8.5 or newer):

    git for-each-ref --format='delete %(refname)' refs/original | git update-ref --stdin

    git reflog expire --expire=now --all

    git gc --prune=now

## Using filter-branch
    git filter-branch --force --index-filter \
    'git rm --cached --ignore-unmatch PATH-TO-YOUR-FILE-WITH-SENSITIVE-DATA' \
    --prune-empty --tag-name-filter cat -- --all


Add your file with sensitive data to .gitignore to ensure that you don't accidentally commit it again.

    echo "YOUR-FILE-WITH-SENSITIVE-DATA" >> .gitignore
    git add .gitignore
    git commit -m "Add YOUR-FILE-WITH-SENSITIVE-DATA to .gitignore"
    
Push your local repo to GitHub

    git push origin --force --all


In order to remove the sensitive file from your tagged releases, you'll also need to force-push against your Git tags:

    git push origin --force --tags


## Using the BFG Repo Cleaner
BFG Repo cleaner is an alternative to git filter-branch. It can be used to remove sensitive data or large files that were committed wrongly like binaries compiled from the source. It is written in Scala.

Project website: [BFG Repo Cleaner][1]

## Requirements ##

The Java Runtime Environment (Java 7 or above - BFG v1.12.3 was the last version to support Java 6). The Scala library and all other dependencies are folded into the downloadable jar.

## Remove files with sensitive data ##

    bfg --delete-files YOUR-FILE-WITH-SENSITIVE-DATA


  [1]: https://rtyley.github.io/bfg-repo-cleaner/

