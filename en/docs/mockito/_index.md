---
title : mockito Tutorial
slug : mockito-tutorial
weight : 9903
draft : false
images : []
type : docs
---

Mockito is a java Mocking framework that aims at providing the ability to write clean an readable unit tests by using it's simple API. It differs from other mocking frameworks by leaving the expect-run-verify pattern that most other frameworks use.

Instead it only knows one way to mock (non-final) classes and interfaces and allows to verify and stub based on flexible argument matchers.

The current Version 1.10.19 is best obtained using maven

    <dependency>
        <groupId>org.mockito</groupId>
        <artifactId>mockito-core</artifactId>
        <version>1.10.19</version>
    </dependency>

or gradle

    repositories { jcenter() }
    dependencies { testCompile "org.mockito:mockito-core:1.+" }

Version 2 is still in beta.

