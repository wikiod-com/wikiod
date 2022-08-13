---
title: "Introduction to Server-Side Rendering"
slug: "introduction-to-server-side-rendering"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Rendering components
There are two options to render components on server: `renderToString` and `renderToStaticMarkup`.

# renderToString

This will render React components to HTML on server. This function will also add `data-react-` properties to HTML elements so React on client won't have to render elements again.

<!-- language: lang-js -->

    import { renderToString } from "react-dom/server";
    renderToString(<App />);


# renderToStaticMarkup

This will render React components to HTML, but without `data-react-` properties, it is not recommended to use components that will be rendered on client, because components will rerender.

<!-- language: lang-js -->

    import { renderToStaticMarkup } from "react-dom/server";
    renderToStaticMarkup(<App />);

