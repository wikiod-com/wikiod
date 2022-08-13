---
title: "Compilation of Errors and Warnings"
slug: "compilation-of-errors-and-warnings"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Notice: Undefined index
**Appearance :** 

Trying to access an array by a key that does not exist in the array

**Possible Solution :**

Check the availability before accessing it. Use:
1. [`isset()`][1]
2. [`array_key_exists()`][2]


  [1]: http://php.net/manual/en/function.isset.php
  [2]: http://php.net/manual/en/function.array-key-exists.php

## Warning: Cannot modify header information - headers already sent
**Appearance :**

Happens when your script tries to send a HTTP header to the client but there already was output before, which resulted in headers to be already sent to the client.

**Possible Causes :**
1. *Print, echo:* Output from print and echo statements will terminate the opportunity to send HTTP headers. The application flow must be restructured to avoid that.

2. *Raw HTML areas:* Unparsed HTML sections in a .php file are direct output as well. Script conditions that will trigger a `header()` call must be noted before any raw <html> blocks.

       <!DOCTYPE html>
       <?php
            // Too late for headers already.

3. *Whitespace before `<?php` for "script.php line 1" warnings:* If the warning refers to output in line 1, then it's mostly leading whitespace, text or HTML before the opening `<?php` token.

       <?php
       # There's a SINGLE space/newline before <? - Which already seals it.

Reference from SO [answer][1] by [Mario][2]


  [1]: http://stackoverflow.com/a/8028987/5447994
  [2]: http://stackoverflow.com/users/345031/mario

## Parse error: syntax error, unexpected T_PAAMAYIM_NEKUDOTAYIM
**Appearance:**

"Paamayim Nekudotayim" means "double colon" in Hebrew; thus this error refers to the inappropriate use of the double colon operator (`::`). The error is typically caused by an attempt to call a static method that is, in fact, not static. 


**Possible Solution:**

    $classname::doMethod();

If the above code causes this error, you most likely need to simply change the way you call the method:

    $classname->doMethod();

The latter example assumes that `$classname` is an instance of a class, and the `doMethod()` is not a static method of that class.  

