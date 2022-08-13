---
title: "Getting started with schema.org"
slug: "getting-started-with-schemaorg"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic implementation
Person
------

[Microdata][1]

<!-- language: lang-html -->
    
    <section itemscope itemtype="http://schema.org/Person">
      Hello, my name is
      <span itemprop="name">John Doe</span>,
      I am a
      <span itemprop="jobTitle">Graduate research assistant</span>
      at the
      <span itemprop="affiliation">University of Dreams</span>
      My friends call me
      <span itemprop="additionalName">Johnny</span>
      You can visit my homepage at
      <a href="http://www.example.com" itemprop="url">www.example.com</a>
    </section>

[JSON-LD][2]

<!-- language: lang-json -->

    <script type="application/ld+json">
    {
      "@context": "http://schema.org",
      "@type": "Person",
      "name": "John Doe",
      "jobTitle": "Graduate research assistant",
      "affiliation": "University of Dreams",
      "additionalName": "Johnny",
      "url": "http://www.example.com"
    }
    </script>

  [1]: http://stackoverflow.com/tags/microdata/info
  [2]: http://stackoverflow.com/tags/json-ld/info

