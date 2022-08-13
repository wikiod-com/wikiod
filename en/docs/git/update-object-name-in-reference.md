---
title: "Update Object Name in Reference"
slug: "update-object-name-in-reference"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Update Object Name in Reference
## **Use** ##
Update the object name which is stored in reference

## **SYNOPSIS** ##

    git update-ref [-m <reason>] (-d <ref> [<oldvalue>] | [--no-deref] [--create-reflog] <ref> <newvalue> [<oldvalue>] | --stdin [-z])

## **General Syntax** ##

 1. Dereferencing the symbolic refs, update the current branch head to the new object.

        git update-ref HEAD <newvalue>


 2. Stores the `newvalue` in `ref`, after verify that the current value of the `ref` matches `oldvalue`.

        git update-ref refs/head/master <newvalue> <oldvalue>
    
    above syntax updates the master branch head to `newvalue` only if its current value is `oldvalue`.

Use `-d` flag to deletes the named `<ref>` after verifying it still contains `<oldvalue>`.

Use `--create-reflog`, update-ref will create a reflog for each ref even if one would not ordinarily be created.

Use `-z` flag to specify in NUL-terminated format, which has values like update, create, delete, verify.

**Update**

Set `<ref>` to `<newvalue>` after verifying `<oldvalue>`, if given. Specify a zero `<newvalue>` to ensure the ref does not exist after the update and/or a zero `<oldvalue>` to make sure the ref does not exist before the update.

**Create**

Create `<ref>` with `<newvalue>` after verifying it does not exist. The given `<newvalue>` may not be zero.

**Delete**

Delete `<ref>` after verifying it exists with `<oldvalue>`, if given. If given, `<oldvalue>` may not be zero.

**Verify**

Verify `<ref>` against `<oldvalue>` but do not change it. If `<oldvalue>` zero or missing, the ref must not exist.



