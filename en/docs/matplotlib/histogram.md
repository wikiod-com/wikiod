---
title: "Histogram"
slug: "histogram"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Simple histogram
```python
import matplotlib.pyplot as plt
import numpy as np

# generate 1000 data points with normal distribution
data = np.random.randn(1000)

plt.hist(data)

plt.show()
```

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/UwsUq.png

