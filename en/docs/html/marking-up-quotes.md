---
title: "Marking-up Quotes"
slug: "marking-up-quotes"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

 `cite` and `blockquote` elements should not be used for the purpose of representing a conversation, transcripts of conversations, dialogues in scripts, records of instant messages and other situations in which different players take turns in the speech.

## Inline with <q>
The **`q` element** can be used for a quote that is part of a sentence:

    <p>She wrote <q>The answer is 42.</q> and everyone agreed.</p>

### Quotation marks ###

<!-- if version [lte 4.01] -->
Quotation marks should not be added. User agents should (in HTML 4.01) resp. must (in HTML 4.0) render them automatically.
<!-- end version if -->

<!-- if version [eq 5] -->
Quotation marks must not be added. User agents will render them automatically.
<!-- end version if -->

### Source URL (`cite` attribute) ###

The **`cite` attribute** can be used to reference the URL of the quoted source:

    <p>She wrote <q cite="http://example.com/blog/hello-world">The answer is 42.</q> and everyone agreed.</p>

Note that browsers typically don’t show this URL, so if the source is relevant, you should add a hyperlink (`a` element) in addition.

## Block with <blockquote>
The **`blockquote` element** can be used for a (block-level) quote:

    <blockquote>
      <p>The answer is 42.</p>
    </blockquote>

### Source URL (`cite` attribute) ###

The **`cite` attribute** can be used to reference the URL of the quoted source:

    <blockquote cite="http://example.com/blog/hello-world">
      <p>The answer is 42.</p>
    </blockquote>

Note that browsers typically don’t show this URL, so if the source is relevant, you should add a hyperlink (`a` element) in addition (see the section *Citation/Attribution* about where to place this link).

### Citation/Attribution ###

<!-- if version [lte 4.01] -->
The citation/attribution should not be part of the `blockquote` element:

    <blockquote cite="http://example.com/blog/hello-world">
      <p>The answer is 42.</p>
    </blockquote>
    <p>Source: <cite><a href="http://example.com/blog/hello-world" rel="external">Hello World</a></cite></p>

You can add a `div` element to group the quote and the citation, but it exists no way to associate them semantically.

The **`cite` element** can be used for the reference of the quoted source (but not for the author name).

<!-- end version if -->

<!-- if version [eq 5] -->
The citation/attribution (e.g., the hyperlink giving the source URL) can be inside the `blockquote`, but in that case it must be within a `cite` element (for in-text attributions) or a `footer` element:

    <blockquote cite="http://example.com/blog/hello-world">
      <p>The answer is 42.</p>
      <footer>
        <p>Source: <cite><a href="http://example.com/blog/hello-world" rel="external">Hello World</a></cite></p>
      </footer>
    </blockquote>

The **`cite` element** can be used for the reference of the quoted source, or for the name of the quote’s author.

<!-- end version if -->



