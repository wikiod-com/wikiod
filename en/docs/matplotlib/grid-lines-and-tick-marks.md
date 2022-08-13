---
title: "Grid Lines and Tick Marks"
slug: "grid-lines-and-tick-marks"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Plot With Gridlines
# Plot With Grid Lines

[![Picture of Plot with Gridlines][1]][1]

```
import matplotlib.pyplot as plt

# The Data
x = [1, 2, 3, 4]
y = [234, 124,368, 343]

# Create the figure and axes objects
fig, ax = plt.subplots(1, figsize=(8, 6))
fig.suptitle('Example Of Plot With Grid Lines')

# Plot the data
ax.plot(x,y)

# Show the grid lines as dark grey lines
plt.grid(b=True, which='major', color='#666666', linestyle='-')

plt.show()
```

# Plot With Major and Minor Grid Lines

[![enter image description here][2]][2]

```
import matplotlib.pyplot as plt

# The Data
x = [1, 2, 3, 4]
y = [234, 124,368, 343]

# Create the figure and axes objects
fig, ax = plt.subplots(1, figsize=(8, 6))
fig.suptitle('Example Of Plot With Major and Minor Grid Lines')

# Plot the data
ax.plot(x,y)

# Show the major grid lines with dark grey lines
plt.grid(b=True, which='major', color='#666666', linestyle='-')

# Show the minor grid lines with very faint and almost transparent grey lines
plt.minorticks_on()
plt.grid(b=True, which='minor', color='#999999', linestyle='-', alpha=0.2)

plt.show()
```


  [1]: http://i.stack.imgur.com/eaMZu.png
  [2]: http://i.stack.imgur.com/xXiYf.png

