---
title: "Smoothing a signal"
slug: "smoothing-a-signal"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Using a Savitzky–Golay filter
Given a noisy signal:


```
import numpy as np
import matplotlib.pyplot as plt
np.random.seed(1)

x = np.linspace(0,2*np.pi,100)
y = np.sin(x) + np.random.random(100) * 0.2

plt.plot(x,y)
plt.show()
```
[![enter image description here][1]][1]

one can smooth it using a [Savitzky–Golay filter](https://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter) using the [`scipy.signal.savgol_filter()`](http://scipy.github.io/devdocs/generated/scipy.signal.savgol_filter.html#scipy.signal.savgol_filter) method:

[![enter image description here][2]][2]

```
import scipy.signal
import numpy as np
import matplotlib.pyplot as plt
np.random.seed(1)

x = np.linspace(0,2*np.pi,100)
y = np.sin(x) + np.random.random(100) * 0.2
yhat = scipy.signal.savgol_filter(y, 51, 3) # window size 51, polynomial order 3

plt.plot(x,y)
plt.plot(x,yhat, color='red')
plt.show()
```


  [1]: http://i.stack.imgur.com/DPSSa.png
  [2]: http://i.stack.imgur.com/ZOHNf.png

