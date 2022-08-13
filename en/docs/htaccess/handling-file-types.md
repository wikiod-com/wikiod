---
title: "Handling File Types"
slug: "handling-file-types"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Enable PHP to be parsed in HTML
<!-- language-all: lang-bash -->

If you want to include PHP code in your HTML file and you don't want to rename the file type from `.html` or `.htm` to `.php`, the below allows your HTML file to parse your PHP code correctly.

    AddHandler application/x-httpd-php .html .htm

