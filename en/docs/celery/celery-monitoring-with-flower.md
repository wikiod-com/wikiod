---
title: "Celery monitoring with Flower"
slug: "celery-monitoring-with-flower"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Up and running with Flower
Flower is a web based tool to monitor Celery.

To install Flower, we can use `pip` as follows:

    pip install flower

To run Flower for `proj`:

    celery -A proj flower

Now, flower is accessible in:

    http://localhost:5555

