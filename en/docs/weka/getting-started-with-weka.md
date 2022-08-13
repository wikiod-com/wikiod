---
title: "Getting started with weka"
slug: "getting-started-with-weka"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Weka is a collection of machine learning algorithms for data mining tasks. The algorithms can either be applied directly to a dataset or called from your own Java code. Weka contains tools for data pre-processing, classification, regression, clustering, association rules, and visualization. It is also well-suited for developing new machine learning schemes.

**Downloading and installing Weka**
===================================
There are two versions of Weka: Weka 3.8 is the latest stable version, and Weka 3.9 is the development version. For the bleeding edge, it is also possible to download nightly snapshots.

Stable versions receive only bug fixes, while the development version receives new features. Weka 3.8 and 3.9 feature a package management system that makes it easy for the Weka community to add new functionality to Weka. The package management system requires an internet connection in order to download and install packages.

You can download the application for Windows/MacOS/Linux [here][1].

Integrate WEKA library in your code:

**pox.xml:**

    <dependency>
        <groupId>nz.ac.waikato.cms.weka</groupId>
        <artifactId>weka-dev</artifactId>
        <version>3.9.1</version>
    </dependency>

**gradle:**

    compile group: 'nz.ac.waikato.cms.weka', name: 'weka-dev', version: '3.9.1'


  [1]: http://www.cs.waikato.ac.nz/ml/weka/downloading.html


