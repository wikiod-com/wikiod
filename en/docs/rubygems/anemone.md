---
title: "Anemone"
slug: "anemone"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
 - Use Anemone::Core.new(url, options) to initialize the crawler
 - Use on_every_page block to run code on every page visited
 - Use .run method to start the crawl. No code beforehand will actually start any GET calls.

## Parameters
| Parameter | Details |
| ------ | ------ |
| url | URL (including protocol to be crawled) |
| options | optional hash, see all options [here](http://www.rubydoc.info/github/chriskite/anemone/Anemone/Core)|

- The crawler will by only visit links that are on the same domain as the starting URL. This is important to know when dealing with content subdomains such as `media.domain.com` since they will be ignored when crawling `domain.com`
- The crawler is HTTP / HTTPS aware and will by default stay on the initial protocol and not visit other links on the same domain
- The `page` object in the `on_every_page` block above has a `.doc` method which returns the Nokogiri document for the HTML body of the page. This means you can use Nokogiri selectors inside the `on_every_page` block such as `page.doc.css('div#id')`
- Other information to start can be found [here](http://anemone.rubyforge.org/information-and-examples.html)

## Basic Site Crawl
    pages = []
    crawler = Anemone::Core.new(url, options)
    crawler.on_every_page do |page|
      results << page.url
    end
    crawler.run

