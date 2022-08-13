---
title: "Developing for Screen Reader Users"
slug: "developing-for-screen-reader-users"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

[NVDA][1] is a free screen reader for Windows, which you can use for testing.


  [1]: http://www.nvaccess.org/



## Hiding non-interactive content from visible display, still read by screen readers
If you were to hide a link by setting `display: none` in the CSS then screen readers wouldn’t find it.

Instead, we position it absolutely, with clipping.

**CSS**

<code>.offscreen
{
position: absolute;
clip: rect(1px 1px 1px 1px); /* for Internet Explorer */
clip: rect(1px, 1px, 1px, 1px);
padding: 0;
border: 0;
height: 1px;
width: 1px;
overflow: hidden;
}</code>

**HTML**

<code>&lt;div class="offscreen"&gt;This text is hidden.&lt;/div&gt;</code>

**Credit**:

Steve Faulkner (Paciello Group): [HTML5 Accessibility Chops: hidden and aria-hidden](https://www.paciellogroup.com/blog/2012/05/html5-accessibility-chops-hidden-and-aria-hidden/), 1 May 2012.

Notes by Ted Drake, on use of the off screen technique described:

> Using negative position can create long scroll bars when localizing a
> site for a rtl language. Also, it uses CSS properties that are
> commonly used and easy to accidentally over-ride.
> 
> The Yahoo Accessibility Lab recommends using clip for content that
> should be hidden from the visual user, yet available to screen reader
> users. Thierry Koblentz has a great article on this technique, as well
> as the underlying philosophy behind using the correct CSS techniques
> for hiding content. [Clip your hidden content for better
> accessibility][1]


  [1]: http://yaccessibilityblog.com/library/css-clip-hidden-content.html

