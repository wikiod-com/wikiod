---
title: "Domain Specific Languages"
slug: "domain-specific-languages"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Language capabilities
The [Jenkins Pipeline DSL][1] is used as an example for such a language:

    node {
      git 'https://github.com/joe_user/simple-maven-project-with-tests.git'
      def mvnHome = tool 'M3'
      sh "${mvnHome}/bin/mvn -B -Dmaven.test.failure.ignore verify"
      archiveArtifacts artifacts: '**/target/*.jar', fingerprint: true
      junit '**/target/surefire-reports/TEST-*.xml'
     }

The purpose of this DSL is the define and execute Jenkins build jobs (or better pipelines) in a more natural language.

Writing a domain specific language in Groovy benefits by Groovy's core features like:

 - [Optionality][2] (e.g. omit parentheses)
 - [Operator overloading][3]
 - Meta programming (e.g. resolving missing properties or methods)
 - [Closures and delegation strategies][4]
 - Compiler customization
 - Scripting support and [integration capabilities][5]


  [1]: https://jenkins.io/doc/pipeline/
  [2]: http://www.groovy-lang.org/semantics.html#_optionality
  [3]: http://www.groovy-lang.org/dsls.html#_operator_overloading
  [4]: http://www.groovy-lang.org/dsls.html#section-delegatesto
  [5]: http://www.groovy-lang.org/integrating.html

