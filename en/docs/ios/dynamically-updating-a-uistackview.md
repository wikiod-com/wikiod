---
title: "Dynamically updating a UIStackView"
slug: "dynamically-updating-a-uistackview"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Connect the UISwitch to an action we can animate switching between a horizontal or vertical layout of the image views
    @IBAction func axisChange(sender: UISwitch) {
        UIView.animateWithDuration(1.0) {
            self.updateConstraintsForAxis()
        }
    }

The updateConstraintForAxis function just sets the axis of the stack view containing the two image views:

    private func updateConstraintsForAxis() {
        if (axisSwitch.on) {
            stackView.axis = .Horizontal
        } else {
            stackView.axis = .Vertical
        }
    }

The animated gif below gives you an idea of how this appears:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/n0ZX0.gif

