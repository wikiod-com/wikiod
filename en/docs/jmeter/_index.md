---
title : Apache JMeter Tutorial
slug : apache-jmeter-tutorial
weight : 9945
draft : false
images : []
type : docs
---

JMeter is a Load-Testing Tool used for [Performance Testing][1]. A Performance Tester can record actions in a web browser or manually build a *script* which can then be run with hundreds or thousands of users.

JMeter can be used to create incredibly dynamic users and scenarios using its various elements. For instance, the [`CSV Data Set Config`][2] can be used to specify a set of users to log into a web application. The [`Regular Expression Extractor`][3] or the [CSS/JQuery Extractor][4] can be used to save session ids to be used in future requests. The [`JSR223 PreProcessor`][5] coupled to [Groovy][6] language can be used to create dynamic unique data for each user to be sent as part of a `POST` body.


  [1]: https://en.wikipedia.org/wiki/Software_performance_testing
  [2]: http://jmeter.apache.org/usermanual/component_reference.html#CSV_Data_Set_Config
  [3]: http://jmeter.apache.org/usermanual/component_reference.html#Regular_Expression_Extractor
  [4]: http://jmeter.apache.org/usermanual/component_reference.html#CSS/JQuery_Extractor
  [5]: http://jmeter.apache.org/usermanual/component_reference.html#JSR223_PreProcessor
  [6]: http://www.groovy-lang.org/

