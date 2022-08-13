---
title: "Process and Thread affinity setting"
slug: "process-and-thread-affinity-setting"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
| affinity | integer that describes the set of processors on which the process is allowed to run. For example, on a 8 processor system if you want your process to be executed only on processors 3 and 4 than you choose affinity like this : 00001100 which equals 12 |

The processor affinity of a thread is the set of processors it has a relationship to. In other words, those it can be scheduled to run on.

Processor affinity represents each processor as a bit. Bit 0 represents processor one, bit 1 represents processor two, and so on.


## Get process affinity mask


## Set process affinity mask


