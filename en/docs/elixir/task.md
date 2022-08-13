---
title: "Task"
slug: "task"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Syntax
- Task.async(fun)
- Task.await(task)

## Parameters
| Parameter | Details |
| ------ | ------ |
| fun    | The function that should be executed in a separate process.   |
| task   | The task returned by `Task.async`.   |

## Doing work in the background
    task = Task.async(fn -> expensive_computation end)
    do_something_else
    result = Task.await(task)

## Parallel processing
    crawled_site = ["http://www.google.com", "http://www.stackoverflow.com"]
    |> Enum.map(fn site -> Task.async(fn -> crawl(site) end) end)
    |> Enum.map(&Task.await/1)

