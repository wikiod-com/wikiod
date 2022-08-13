---
title: "Getting started with xss"
slug: "getting-started-with-xss"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Search Results Example
Let's assume we have a search results page that displays a user's search query back to them. The code below is an example of how this could be done in PHP:

    Results for "<?php echo $_GET['query'] ?>"

For this to work, you would access the page with a URL like:

    https://yoursite.test/search?query=stackoverflow

In the response, we get:

    Results for "stackoverflow"

Now we will attempt to inject our payload into the response:

    https://yoursite.test/search?query=<script>alert(1)</script>

And our new response:

    Results for "<script>alert(1)</script>"

We have successfully injected our XSS payload.

