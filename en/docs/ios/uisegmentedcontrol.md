---
title: "UISegmentedControl"
slug: "uisegmentedcontrol"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

A UISegmentedControl object is a horizontal control made of multiple segments, each segment functioning as a discrete button. A segmented control affords a compact means to group together a number of controls.

## Creating UISegmentedControl via code
1. Create new instance of UISegmentedControl filled with 3 items (segments):
```
let mySegmentedControl = UISegmentedControl (items: ["One", "Two", "Three"])
```
2. Setup frame;
```
mySegmentedControl.frame = CGRect(x: 0.0, y: 0.0, width: 300, height: 50)
```
3. Make default selection (not that segments are indexed by 0):
```
mySegmentedControl.selectedSegmentIndex = 0
```
4. Configure target:
```
mySegmentedControl.addTarget(self, action: #selector(segmentedValueChanged(_:)), for: .valueChanged)
```
5 Handle value changed:
```
func segmentedValueChanged(_ sender:UISegmentedControl!) {
    print("Selected Segment Index is : \(sender.selectedSegmentIndex)")
}
```
6. Add UISegmentedControl to views hierarchy
```
yourView.addSubview(mySegmentedControl)


