---
title: "Rewriting history with filter-branch"
slug: "rewriting-history-with-filter-branch"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Changing the author of commits
You can use an environment filter to change the author of commits. Just modify and export `$GIT_AUTHOR_NAME` in the script to change who authored the commit.

Create a file `filter.sh` with contents like so:

    if [ "$GIT_AUTHOR_NAME" = "Author to Change From" ]
    then
        export GIT_AUTHOR_NAME="Author to Change To"
        export GIT_AUTHOR_EMAIL="email.to.change.to@example.com"
    fi

Then run `filter-branch` from the command line:

    chmod +x ./filter.sh
    git filter-branch --env-filter ./filter.sh

## Setting git committer equal to commit author
This command, given a commit range `commit1..commit2`, rewrites history so that git commit author becomes also git committer:

    git filter-branch -f --commit-filter \
       'export GIT_COMMITTER_NAME=\"$GIT_AUTHOR_NAME\";
        export GIT_COMMITTER_EMAIL=\"$GIT_AUTHOR_EMAIL\";
        export GIT_COMMITTER_DATE=\"$GIT_AUTHOR_DATE\";
        git commit-tree $@' \
        -- commit1..commit2

