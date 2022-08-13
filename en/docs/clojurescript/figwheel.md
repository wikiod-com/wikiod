---
title: "Figwheel"
slug: "figwheel"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Figwheel automatically rebuilds your clojurescript code when source files change and reloads code in browser. Reload works without refreshing page and you can preserve some of app's state between reloads by using `defonce`.

It is alternative to REPL-based development (although it includes REPL too). Instead of re-evaling changed functions in REPL, it reloads all code, and you can use `println` to see result of expression evaluation in browser's js console.

## Creating a New Project
Create a new project with the Leiningen `figwheel` template:

<!-- language: lang-sh -->
    lein new figwheel hello-world

Run Figwheel:

<!-- language: lang-sh -->
    cd hello-world
    lein figwheel

After a moment it will start a development webserver and open the page in your browser.

It also opens a Clojurescript REPL connected to the browser. Try entering `(js/alert "Test")`: you should see an alert pop up in the browser.

Try editing `hello_world/core.cljs`. It will automatically refresh in your browser when you save it. You can use reload functionality similarly to the REPL by adding `print`s and seeing results in the Javascript console.

