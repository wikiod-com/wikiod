---
title: "How Can I integrate Markdown editor with Advance Custom Field's repeater Add-on."
slug: "how-can-i-integrate-markdown-editor-with-advance-custom-fields-repeater-add-on"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Add MarkDown Editor
I found the solution. Please consider below mention steps.

Install [wp Markdown Editor][1] plugin. 


Then 
Install "[acf-wp-wysiwyg][2]" for repeater field. Now you have to update in some files. Open this file and go to line number "180" or go to "create_field" function add

    echo '<script> var simplemde = new SimpleMDE({element: document.getElementById("'.$id.'")});jQuery(".quicktags-toolbar").css("display","none");</script>';


Now under "acf-repeater" plugin open "input.js" file, line number "142"

replace 

    new_field_html = this.$el.find('> table > tbody > tr.row-clone').html().replace(/(=["]*[\w-\[\]]*?)(acfcloneindex)/g, '$1' + new_id),

With

    new_field_html = this.$el.find('> table > tbody > tr.row-clone').html().replace(/(["]*[\w-\[\]]*?)(acfcloneindex)/g, '$1' + new_id),


  [1]: https://wordpress.org/plugins/wp-markdown-editor/
  [2]: https://github.com/elliotcondon/acf-wordpress-wysiwyg-field

