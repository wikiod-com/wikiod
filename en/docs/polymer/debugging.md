---
title: "Debugging"
slug: "debugging"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Disabling HTML Import caching
HTML Import caching will sometimes mean that changes made to HTML files that get imported do not get reflected upon browser refresh. Take the following import as an example:
```
<link rel="import" href="./my-element.html">
```
If a change is done to `my-element.html` after previously loading the page, then the changed file may not be downloaded and used in the current document when it is refreshed (as it was previously imported and cached). This can be great for a production, but might hinder development.

To disable this in Google Chrome:
- Open up Google Chrome's [DevTools][1]
- Select the [Main Menu][2] > Settings
- Go to the Network section
- Select "Disable cache (while DevTools is open)"

This will avoid caching HTML Imports, but only when DevTools is open.

  [1]: https://developer.chrome.com/devtools#access%20DevTools
  [2]: https://developers.google.com/web/tools/chrome-devtools/settings?hl=en#main-menu%20settings

