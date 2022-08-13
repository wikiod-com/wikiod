---
title: "Assets"
slug: "assets"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Sass/SCSS
By default, all your `.scss` partials go in the `<source>/_sass` folder.

**/_sass/base.scss**

<!-- language: lang-sass -->
```
body {
  margin: 0;
}
```

Your main `.css` or `.scss` files go in the `<source>/css` folder. Note: the two first two lines of triple dashes are necessary in order for Jekyll to transpile your `.scss` file to `.css`.

**/css/main.scss**
<!-- language: lang-sass -->
```
---
---

@import "base";

div {
  color: #000;
}
```

A transpiled `.css` file will then appear in `_site/css/` when you build your site:

**/_site/css/main.css**
<!-- language: lang-sass -->
```
body {
  margin: 0 }
div {
  color: #000 }
```

The css file can be referenced by your `.html` files like so:

**/_layouts/home.html**
<!-- language: lang-html -->
```
<!DOCTYPE html>
<html>
<head>

<link rel="stylesheet" href="/css/main.css">
</head>

<body>
</body>
</html>
```

