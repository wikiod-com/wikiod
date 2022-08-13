---
title: "Connectiong Components"
slug: "connectiong-components"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## If / OnComponent / OnSubjob
There are 2/3 options to connect components together in Talend. You should always try to use OnSubjob connectors. This saves a lot of headaches. You'll see from the examples why.

**What happens when you mix the connection types / What is the execution order?**

 1. If
 2. OnComponent
 3. OnSubjob

Keep in mind that the If connections gets evaluated runtime, which means if you use globalMap then be really careful about the order. 

For example:

`(Boolean)globalMap.get("failure") == true` -> calls a subjob that resets this failure flag.
`(Boolean)globalMap.get("failure") == false` -> calls a subjob that lets the main job continue, because the failure path resetted the flag.

**What is the a difference?**

If and Oncomponent connections act as a function call. Which makes the Garbage collector to keep all the local data stored in memory. This could cause "memory leaks".

OnSubjob connections on the other hand let the subjob complete and return, thus the GC will free up some of the memory.

Other than the memory there's also a few things you need to keep in mind. If you have a data flow that reads from / writes to file, you should always go with OnSubjobOk as the data file will be closed once the job completes. If you use onComponent it can happen that the file is not saved hence you start working with a 0 byte file, and after the job completes you'll see a file with content. It's logical but really hard to figure out.

