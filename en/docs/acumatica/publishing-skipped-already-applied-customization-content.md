---
title: "Publishing skipped already applied customization content"
slug: "publishing-skipped-already-applied-customization-content"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

When publishing a customization project, you might see some item being skipped for the reason of being already applied. Ex: 

EntityEndpoint EntityEndpoint#6.00.001§DefaultPlus(skipped, already applied)

This can happen for any items contained saved in the database. Ex:
Generic inquiries, reports, site map nodes, DB scripts, system locales, import/export scenarios, shared filters, access rights, wikis, web service endpoints and analytical reports.

## Publish with cleanup from the customization screen
 1) You must obviously select the project that you want to publish.
 2) You must click on the small arrow right next to the "Publish" button.
 3) You must click on the "Publish to Multiple Companies" option.

[![enter image description here][1]][1]

 4) On the smart panel that will appear you must select the companies that you want to publish the project(s). Only one company is also a possibility.
5) Check the check box indicating "Publish with Cleanup", this will make sure to reapply all item present in the customization project replacing the already present one with their newer version.

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/28g4V.jpg
  [2]: https://i.stack.imgur.com/UTwmQ.jpg

## Publish with clean up from inside a customization project
1) Open the customization project that you want to publish with this method.
2) Open the publish menu at the top and select the "Publish with Cleanup" option.

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/O0j3Z.jpg

*Please take note that all customization project that are selected on the customization screen will be republish even if you are inside only a single project.

