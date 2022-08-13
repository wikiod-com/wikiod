---
title: "Folding"
slug: "folding"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

*Folding* causes multiple lines of text to be collapsed and displayed as a single line. It is useful for hiding portions of a document considered unimportant for the current task. Folding is purely a visual change to the document: the folded lines are still present, unchanged.

A fold is persistent. Once created, a fold can be opened and closed without needing to re-create it. When closed, folds can be moved over or yanked and put as if they were a single line, even though the underlying operation will operate on all of the text underneath the fold

## Configuring the Fold Method
`:set foldmethod={method}` sets the fold method for the current window. This determines how folds are manipulated within that window. Valid options for "method" are:

 - `manual` (folds are manually created and destroyed by the user)
 - `indent` (folds are created for lines of equal indentation)
 - `marker` (substring markers are used to indicate the beginning and end of a fold)
 - `syntax` (syntax highlighting items define folds)
 - `expr` (a Vimscript expression is evaluated per line to define its fold level)
 - `diff` (text change isn't changed in a diff view is folded)

The default is `manual`.

## Opening, Closing and Toggling Folds
- `zo` opens a fold underneath the cursor.
- `zO` opens all folds underneath the cursor, recursively.
- `zc` closes a fold underneath the cursor.
- `zC` closes all folds underneath the cursor, recursively.
- `za` toggles a fold under the cursor (a closed fold is opened, an opened fold is closed).
- `zM` closes all folds in the buffer.
- `zR` opens all folds in the buffer.
- `zm` closes a level of fold in the buffer.
- `zr` opens a level of fold in the buffer.

## Creating a Fold Manually
- `zf{motion}` creates a fold that covers the text that "motion" would cover. 
- `{count}zF` creates a fold that covers "count" lines.
- `{range}fo[ld]` creates a fold for the lines in the provided range.

All three commands are valid only when `foldmethod` is set to `manual` or `marker`. In the case of the former fold method, the newly-created folds are closed immediately.

## Showing the Line Containing the Cursor
`zv` will ensure the line containing the cursor is not folded. The minimum number of folds required to expose the cursor line will be opened.

## Folding C blocks
This is our buffer:

```
void write_buffer(size_t size, char ** buffer)
{
    char * buf = *buffer;
    size_t iter;
    for(iter = 0; iter < size; iter++)
    {
        putc(*(buf + iter));
    }
}
```

The cursor is at [1][1] ([line][col]).
Move the cursor to the curl bracket of the for loop:  
`/for<Enter>j`  cursor is [6][2].

Now enter `zf%` (create folding, move to matching bracket).
You have successfully create the first folding.

Now enter `:2<Enter>_`, the cursor is now at [2][1] and `zf%`: the complete function body is folded.

You are able to open all foldings you just created using `zO` and re-close them using `zC`.

