---
title : Jsoup Tutorial
slug : jsoup-tutorial
weight : 9959
draft : false
images : []
type : docs
---

Jsoup is a HTML parsing and data extraction library for Java, focused on flexibility and ease of use. It can be used to extract sepecific data from HTML pages, which is commonly known as "web scraping", as well as modify the content of HTML pages, and "clean" untrusted HTML with a whitelist of allowed tags and attributes.

## JavaScript support

**Jsoup does not support JavaScript**, and, because of this, any dynamically generated content or content which is added to the page after page load cannot be extracted from the page. If you need to extract content which is added to the page with JavaScript, there _are_ a few alternative options:

* Use a library which does support JavaScript, such as Selenium, which uses an an actual web browser to load pages, or HtmlUnit.

* Reverse engineer how the page loads it's data. Typically, web pages which load data dynamically do so via AJAX, and thus, you can look at the network tab of your browser's developer tools to see where the data is being loaded from, and then use those URLs in your own code. See [how to scrape AJAX pages](http://stackoverflow.com/questions/8550114/can-scrapy-be-used-to-scrape-dynamic-content-from-websites-that-are-using-ajax) for more details.

## Official website & documentation

You can find various Jsoup related resources at **[jsoup.org](http://jsoup.org)**, including [the Javadoc](https://jsoup.org/apidocs/), usage examples in [the Jsoup cookbook](https://jsoup.org/cookbook/) and [JAR downloads](https://jsoup.org/download).
See **[the GitHub repository](https://github.com/jhy/jsoup)** for the source code, issues, and pull requests.

## Download

Jsoup is available on Maven as `org.jsoup.jsoup:jsoup`, If you're using Gradle (eg. with Android Studio), you can add it to your project by adding the following to your `build.gradle` dependencies section:

    compile 'org.jsoup:jsoup:1.8.3'

---

If you're using Ant (Eclipse), add the following to your POMs dependencies section:

    <dependency>
      <!-- jsoup HTML parser library @ http://jsoup.org/ -->
      <groupId>org.jsoup</groupId>
      <artifactId>jsoup</artifactId>
      <version>1.8.3</version>
    </dependency>

---

Jsoup is also available as **[downloadable JAR](http://jsoup.org/download)** for other environments.

