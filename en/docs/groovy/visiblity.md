---
title: "Visiblity"
slug: "visiblity"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Private fields and methods are not private in groovy
    class MyClass { 
        private String privateField 
    } 
    
    def prvtClss = new MyClass(privateField: 'qwerty') 
    println prvtClss.privateField

will print us 'qwerty'

This issue is known since version 1.1 and there is a bug report on that: http://jira.codehaus.org/browse/GROOVY-1875. It is not resolved even with groovy 2 release.


