---
title: "Basic Keybindings"
slug: "basic-keybindings"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Quit Emacs
You can quit Emacs with the following keybinding:

<kbd>C-x C-c</kbd>

Where `C` is the `control` key.<br/>

<h1> Suspend Emacs </h1>

You can suspend Emacs using the following keybinding :

<kbd> C-z </kbd>

It gets you back to your shell. If you want to resume your emacs session, enter `fg` in your terminal.


## Abort current command
Often you will get into a state where you have a partially typed command sequence in progress, but you want to abort it. You can abort it with either of the following keybindings:

<kbd>C-g</kbd>

<kbd>Esc</kbd><kbd>Esc</kbd><kbd>Esc</kbd>



## Buffers
 - Example of a buffer list
    ```
    CRM Buffer                Size  Mode              Filename[/Process]
    . * .emacs                3294  Emacs-Lisp        ~/.emacs
     %  *Help*                 101  Help
        search.c             86055  C                 ~/cvs/emacs/src/search.c
     %  src                  20959  Dired by name     ~/cvs/emacs/src/
      * *mail*                  42  Mail
     %  HELLO                 1607  Fundamental       ~/cvs/emacs/etc/HELLO
     %  NEWS                481184  Outline           ~/cvs/emacs/etc/NEWS
        *scratch*              191  Lisp Interaction
      * *
    Messages*            1554  Messages
    ```
    The first field of a line indicates:
    - ‘.’ the buffer is current. 
    - ‘%’ a read-only buffer. 
    - ‘*’ the buffer is modified. 

- Select buffer. You can select out of any open buffer with the following keybinding:

  <kbd>C-x b</kbd>

  <sub>You will be prompted for the buffer name you wish to switch to.</sub>

 - List buffers:

    <kbd>C-x C-b</kbd>
 
 - Save-some-buffer, giving the choice which buffer to save or not:
        
    <kbd>C-x s</kbd>

 - Kill one buffer:

    <kbd>C-x k</kdb>

  
 - Operations on marked buffers:                                                                    
                                                                           
      <kbd>S</kbd>  Save the marked buffers                                                                  
     
      <kbd>A</kbd>  View the marked buffers in this frame.                                                   
  
      <kbd>H</kbd>  View the marked buffers in another frame.                                                
  
      <kbd>V</kbd> Revert the marked buffers.                                                               
  
      <kbd>T</kbd> Toggle read-only state of marked buffers.                                                
      
      <kbd>D</kbd> Kill the marked buffers.                                                                 
      
      <kbd>M-s a C-s</kbd> Do incremental search in the marked buffers.                                     
      
      <kbd>M-s a C-M-s</kbd> Isearch for regexp in the marked buffers.                                      
      
      <kbd>U</kbd> Replace by regexp in each of the marked buffers.                                                                                                                  
      
      <kbd>Q</kbd> Query replace in each of the marked buffers.                                             
      
      <kbd>I</kbd> As above, with a regular expression.                                                     
      
      <kbd>P</kbd> Print the marked buffers.                                                                

      <kbd>O</kbd> List lines in all marked buffers which match a given regexp (like the function `occur').                                                                                    

      <kbd>X</kbd> Pipe the contents of the marked buffers to a shell command.                                                   

      <kbd>N</kbd> Replace the contents of the marked buffers with the output of a shell command.  

      <kbd>!</kbd> Run a shell command with the buffer's file as an argument.                                                             
                                                         
      <kbd>E</kbd> Evaluate a form in each of the marked buffers. This is a very flexible command.  For example, if you want to make all of the marked buffers read only, try using (read-only-mode 1) as the input form.                                  
              
      <kbd>W</kbd> - As above, but view each buffer while the form is evaluated.                                           
                                                                                        
      <kbd>k</kbd> - Remove the marked lines from the *Ibuffer* buffer, but don't kill the associated buffer.                                      

      <kbd>x</kbd> - Kill all buffers marked for deletion.

 - Save-some-buffer, giving the choice which buffer to save or not:

    <kbd>C-x s</kbd>

 - Switch to the next buffer:

   <kbd>C-x RIGHT</kbd> 

 - Switch to previous buffer:
    
   <kbd>C-x LEFT</kbd>

## File handling
 - Re-Save open file under the same filename (Save):
    
    <kbd>C-x C-s</kbd>
 
 - Write as `filename` (Save As):
    
    <kbd>C-x C-w</kbd> `filename`

   <sub>The new file name will be prompted in the minibuffer.   </sub>
 
 - Create new file **or** load existing file (New / Load):
    
    <kbd>C-x C-f</kbd> `filename`

    <sub>With the mnemonic here for f meaning file. You will be prompted for a file path in the minibuffer.</sub>

 - Visit alternate file

    <kbd>C-x C-f</kbd>
   
    <sub>If the file does not exist yet, you will be prompted the path of the file to create in the minibuffer. </sub>



## Cursor (point) movement
In addition to cursor movements using the arrow keys, Home, End, Page up, and Page down, emacs defines a number of keystrokes that can move the cursor over smaller or larger pieces of text:

**By character:** 
 - Backward character: <kbd>C-b</kbd>
 - Forward character: <kbd>C-f</kbd>

**By word**
 - Backward word: <kbd>M-b</kbd> (*i.e.* <kbd>Alt b</kbd>, or <kbd>Meta b</kbd>)
 - Forward word: <kbd>M-f</kbd>

**By line:**
 - Beginning of current line: <kbd>C-a</kbd>
 - Beginning of current line first(non-space)character:<kbd>M-m</kbd>
 - End of current line: <kbd>C-e</kbd>
 - Previous line: <kbd>C-p</kbd>
 - Next line: <kbd>C-n</kbd>

**Entire buffer:**
 - Beginning of buffer: <kbd>M-<</kbd>
 - End of buffer: <kbd>M-></kbd>

**By 'block', depending on context (mode):**

Typical key bindings:

 - Backward sentence/statement: <kbd>M-a</kbd>
 - Forward sentence/statement: <kbd>M-e</kbd>
 - Beginning of function: <kbd>M-C-a</kbd>
 - End of function: <kbd>M-C-e</kbd>
 
**Prefix arguments**

In order to move several 'steps' at once, the movement commands may be given a prefix argument by pressing <kbd>ESC</kbd> or <kbd>C-u</kbd> and a number before the listed keystrokes. For <kbd>C-u</kbd>, the number is optional and defaults to 4.  
*E.g.*  <kbd>ESC 3 C-n</kbd> moves 3 lines down, while <kbd>C-u M-f</kbd> moves 4 words forward.

## Key bindings notation
Emacs' documentation uses a consistent notation for all key bindings, which is explained here:

## Key chords

A "key chord" is obtained by pressing two or more keys simultaneously. Key chords are denoted by separating all keys by dashes (`-`). They usually involve modifier keys, which are put up front:
- <kbd>C-</kbd>: control;
- <kbd>S-</kbd>: shift;
- <kbd>M-</kbd>: alt (the "M" stands for "Meta" for historical reasons).

Other keys are simply denoted by their name, like:
- <kbd>a</kbd>: the `a` key;
- <kbd>left</kbd>: the left arrow key;
- <kbd>SPC</kbd>: the space key;
- <kbd>RET</kbd>: the return key.

Examples of key chords thus include:
- <kbd>C-a</kbd>: pressing <kbd>control</kbd> and <kbd>a</kbd> simultaneously;
- <kbd>S-right</kbd>: pressing <kbd>shift</kbd> and <kbd>right</kbd> simultaneously;
- <kbd>C-M-a</kbd>: pressing <kbd>control</kbd>, <kbd>alt</kbd> and <kbd>a</kbd> simultaneously.

## Key sequences

"Key sequences" are sequences of keys (or key chords), which must be typed one after the other. They are denoted by separating all key (or chord) notations by a space.

Examples include:
- <kbd>C-x b</kbd>: pressing <kbd>control</kbd> and <kbd>x</kbd> simultaneously, then releasing them and pressing <kbd>b</kbd>;
- <kbd>C-x C-f</kbd>: pressing <kbd>control</kbd> and <kbd>x</kbd> simultaneously, then releasing <kbd>x</kbd> and pressing <kbd>f</kbd> (since both chords involve the <kbd>control</kbd> modifier, it is not necessary to release it).

## Using ESC instead of Alt

Key chords using the Alt modifier can also be entered as a key sequence starting with <kbd>ESC</kbd>. This can be useful when using Emacs over a remote connection that does not transmit Alt key chords, or when these key combinations are captured _e.g_ by a window manager.  

Example:
  
<kbd>M-x</kbd> can be entered as <kbd>ESC</kbd> <kbd>x</kbd>.

## Describing key bindings in Emacs lisp files

The same notation that is described here can be used when defining key bindings in Emacs lisp files.  
  
Example:  

(global-set-key (kbd "C-x C-b") 'buffer-menu)  
binds the key sequence <kbd>C-x C-b</kbd> to the command `buffer-menu`

## Multiples windows or frames
"Window" in Emacs refers to what might otherwise be called a "pane" or "screen division". Some window manipulation commands include:  
 
 - Split current window horizontally: <kbd>C-x 2</kbd>
 - Split current window vertically: <kbd>C-x 3</kbd>
 - Select next window: <kbd>C-x o</kbd>
 - Close current window: <kbd>C-x 0</kbd>
 - Close all other windows, except the current one: <kbd>C-x 1</kbd>

A "frame" in Emacs is what might otherwise be called a "window".  Frames are manipulated using these commands:

 - Create new frame: <kbd>C-x 5 2</kbd>
 - Delete current frame: <kbd>C-x 5 0</kbd>
 - Delete other frames: <kbd>C-x 5 1</kbd>

Switching windows can be acheived using
 - <kbd>S-left</kbd>, <kbd>S-right</kbd>, <kbd>S-up</kbd>, <kbd>S-down</kbd> (that is, <kbd>Shift</kbd> in conjunction with an arrow key) to switch to the neighboring window in a direction, or 
 - <kbd>C-x o</kbd> to switch to the next window.


## Region - Cut, Copy, Paste
 - Set mark in cursor location:

     <kbd> C-space </kbd> or <kbd>C-@</kbd>

 - Kill region (Cut):

     <kbd> C-w </kbd>

 - Copy region to kill ring:

     <kbd> M-w </kbd>   or   <kbd> Esc-w </kbd>

 - Yank (Paste) most recently killed:

    <kbd> C-y </kbd>

 - Yank (Paste) next last killed:

    <kbd> M-y </kbd> or <kbd> Esc-y </kbd> 

Kill
====
`kill`is the command used by Emacs for the deletion of text. The `kill` command is analogous to the `cut` command in Windows.  Various commands exist that 'kills' one word (<kbd>M-d</kbd>), the rest of the line (<kbd>C-k</kbd>), or larger text blocks. The deleted text is added to the `kill-ring`, from which it can later be `yanked`.

Select and cut (kill)
------------------------

Killing and yanking
Similar to the `select-and-cut` feature in Windows, here we have <kbd>C-spc</kbd>. The key binding <kbd>C-spc</kbd> will start the selection, the user can move the mark with the help of arrow keys or other command to make a selection. Once selection is complete - push the <kbd>C-w</kbd> to kill the selected text


Some basic commands which can be used as quick reference for `kill` command (taken from Emacs tutorial)
<pre>
    M-DEL       Kill the word immediately before the cursor
    M-d         Kill the next word after the cursor

    C-k         Kill from the cursor position to end of line
    M-k         Kill to the end of the current sentence
</pre>

Yank  
====   
`yank`describes the insertion of previously deleted text, *e.g.* using <kbd>C-y</kbd> which yanks the most recently killed text.  `Yank` command is analogous to the `paste` command in Windows.  

Yank text killed previously
---------------------------  
We know that the `kill` command adds the text killed to a `kill-ring`. To retrieve the deleted text from the `kill-ring` use <kbd>M-y</kbd> command repeatedly until the desired text is yanked.

*(Note: For the <kbd>M-y</kbd> key to work the previous command should be a `YANK` otherwise it wouldn't work)*

## Undo
To undo something you just did:

<kbd>C-_</kbd> or <kbd>C-x u</kbd> or <kbd>C-/</kbd>

## Search and Replace
In Emacs, basic search tool (`I-Search`) allows you to search after or before the location of your cursor.

 - To search for *sometext* after the location of your cursor (`search-forward`) hit <kbd> C-s *`sometext`*</kbd>. If you want to go to the next occurence of *`sometext`*, just press <kbd> C-s </kbd> again (and so on for the next occurences). When cursor lands in the right location, press <kbd>Enter</kbd> to exit the search prompt.

- To search before the location of your cursor (`search-backward`), use <kbd>C-r</kbd> the same way you used <C-s> before.

- To switch from `search-backward` to `search-forward`, press 2 times <kbd>C-s</kbd>. And press 2 times <kbd>C-r</kbd> to `search backward` when you're in `search-forward` prompt.

   
 - Search and replace:

    <kbd> M-% </kbd> (or <kbd> Esc-% </kbd>) `oldtext` <kbd> Enter </kbd> `newtext` <kbd> Enter </kbd>

    - Confirm: <kbd> y </kbd>
    - Skip: <kbd> n </kbd>
    - Quit: <kbd> q </kbd>
    - Replace all: <kbd> ! </kbd>


## Case
* Capitalize word: <kbd>M-c</kbd>

* Convert word to upper case: <kbd>M-u</kbd>

* Convert word to lower case: <kbd>M-l</kbd>

