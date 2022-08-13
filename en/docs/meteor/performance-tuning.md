---
title: "Performance Tuning"
slug: "performance-tuning"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

It should be noted that Meteor is simply Javascript and Node.js. Yes, it's a very specific implementation of those two technologies, and has it's own unique ecosystem, and leverages isomorphic APIs and a JSON datastore to achieve some truly amazing results. But, at the end of the day, Meteor is a web technology, and it's written in Javascript. So all of your typical javascript performance techniques still apply. Start there.

[25 Javascript Performance Techniques](http://desalasworks.com/article/javascript-performance-techniques/)  
[Performance Optimizations for High Speed Javascript](http://www.webreference.com/programming/javascript/jkm3/index.html)  
[Optimizing JavaScript Code](https://developers.google.com/speed/articles/optimizing-javascript)  
[Performance Tips for JavaScript in V8](http://www.html5rocks.com/en/tutorials/speed/v8/)  
[10 Javascript Performance Boosting Tip](http://jonraasch.com/blog/10-javascript-performance-boosting-tips-from-nicholas-zakas)  
[Write Efficient JavaScript](http://oreilly.com/server-administration/excerpts/even-faster-websites/writing-efficient-javascript.html)  
[Improving the Performance of your Meteor JS projects](http://projectricochet.com/blog/meteor-js-performance#.U-lvxo1dWnD)


## Designing and Deploying Production Ready Software
Remember, all the best practices of typical web architecture still apply. For an excellent overview on the subject, please refer to Michael Nygard's excellent book [Release It! Design and Deploy Production-Ready Software](https://www.amazon.com/Release-Production-Ready-Software-Pragmatic-Programmers/dp/0978739213?ie=UTF8&*Version*=1&*entries*=0). Writing your app in Meteor doesn't absolve you of auditing third party libraries, writing circuit breakers, wrapping calls in timeouts, monitoring your resource pools, and all the rest. If you want your application to perform well, you need to make sure you're using stability patterns, and avoiding anti-patterns.

**Stability Patterns**  
- Timeouts
- Circuit Breakers
- Bulkheads
- Handshaking
- Stability Anti-Patterns

**Integration Points**  
- Third Party Libraries
- Scaling Effects
- Unbalanced Capabilities
- Capacity Anti-Patterns

**Resource Pool Contention**
- AJAX Overkill
- Overstaying Sessions
- Excessive White Space
- Data Eutropification

If these concepts are unfamiliar and don't seem like second-nature to you, it means that you haven't brought bigger production systems online. Buy a copy of the book. It will be time and money well spent.



