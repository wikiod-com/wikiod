---
title: "Partials and Import"
slug: "partials-and-import"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Importing
Using `@import` allows you to split up your files into multiple smaller files. This makes sense, as you are able to keep better structure for your stylesheets and avoid very large files.

### Example

Let's say you have a few files.

```
- application.scss
- header.scss
- content
  |-- article.scss
  '-- list.scss
- footer.scss
```

Your main stylesheet `application.scss` can import all files as well as define its own styles.

```
// application.scss
// Import files:
@import 'header.scss';
@import 'content/article.scss';
@import 'content/list.scss';
@import 'footer.scss';

// other styles in application.scss
body {
  margin: 0;
}
```

Note that you can also omit the `.scss` extension so you could write `@import 'header';` instead of `@import 'header.scss'`.

This leads to `application.scss` having all imported `.scss` included in the compiled file you serve to the client (browser). In this case your compiled file would be `application.css` which you include in your html.

```
<html>
  <head>
    <link rel="stylesheet" type="text/css" href="/application.css?v=18c9ed25ea60">
  </head>
  <body>
    ...
  </body>
</html>
```

Although you are working with multiple files you only serve one file, eliminating the need for multiple requests (one for each file) and speeding up the load time for your visitors.

# Main benefits

- Better structure for development using folder and multiple files
- Serving only one file to the client (browser)

## Partials
You can create partial files that contain smaller snippets of your stylesheets. This allows you to modularize your CSS and allows for better structure of your stylesheets. A partial is a Sass file named with a leading underscore, i.e: `_partial.scss`. The underscore lets Sass know that the specific file is a partial and it will not be generated into a CSS file.

Sass partials are meant to be used with the `@import` directive. When using `@import`, you can omit the leading underscore.

### Example

Supposing you have a file structure with partials like this

```
- main.scss
- _variables.scss
- content
  |-- _buttons.scss
  '-- _otherelements.scss
```

You can include those partials in your `main.scss` file as follows (leading underscores and file extensions are omitted in this example):
```
// main.scss - Imports:
@import 'variables';
@import 'content/buttons';
@import 'content/otherelements';
```

