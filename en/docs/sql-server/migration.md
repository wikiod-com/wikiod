---
title: "Migration"
slug: "migration"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## How to generate migration scripts
1. Click <kbd>Right Mouse</kbd> on Database you want to migrate then -> `Tasks` -> `Generate Scripts... `

[![enter image description here][1]][1]

2. Wizard will open click `Next` then chose objects you want to migrate and click `Next` again, then click `Advanced` scroll a bit down and in `Types of data to script` choose `Schema and data` (unless you want only structures)

[![enter image description here][2]][2]

3. Click couple more times `Next` and `Finish` and you should have your database scripted in `.sql` file.

4. run `.sql` file on your new server, and you should be done.

  [1]: http://i.stack.imgur.com/Ip4P0.png
  [2]: http://i.stack.imgur.com/SOjkp.png

