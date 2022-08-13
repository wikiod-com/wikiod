---
title: "Tree Activation Bookmarklet"
slug: "tree-activation-bookmarklet"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Activate all pages in the JCR starting with the currently opened page
Create a new browser bookmark, for example, in Chrome click the star icon at the right in the address bar, make sure the ***Folder*** is *Bookmarks Bar*, and then click the **Edit...** button:

 [![Create bookmark in chrom][1]][1]


In the edit box that opens paste the following code as the URL:

    javascript:(function(){var root=(window.location.pathname+window.location.hash).replace(/.html.*$/,'').replace('cf#/','').replace('/crx/de/index.jsp#','').replace('siteadmin#/','').replace('/editor.html','');if(!document.forms.ta){document.body.insertAdjacentHTML('afterbegin','<form name="ta" target="_blank" action="/etc/replication/treeactivation.html" method="POST"><input type="hidden" name="_charset_" value="UTF-8"><input type="hidden" id="path" name="path" value="'+root+'"></form>');}document.forms.ta.submit();})()

[![Editing bookmark source][2]][2]

# Usage
Make sure the bookmarks bar is visible: Settings > Bookmarks > Show Bookmarks Bar (or <kbd>Cmd</kbd>+<kbd>Shift</kbd>+<kbd>B</kbd> / <kbd>Ctr</kbd>+<kbd>Shift</kbd>+<kbd>B</kbd>.

 1. Open an AEM page.
 2. Click the bookmarklet. The tree activation progress with be displayed in a new window.

You can also call tree activation directly from the siteadmin while the required parent page is selected in the navigation tree, and the displayed URL is e.g. http://localhost:4502/siteadmin#/content/geometrixx-outdoors/en

PS: If you want to fix/update the bookmarklet code then paste it into the form at http://subsimple.com/bookmarklets/jsbuilder.htm and click **Format**.


  [1]: http://i.stack.imgur.com/k2HE7.png
  [2]: http://i.stack.imgur.com/CCIXf.png

