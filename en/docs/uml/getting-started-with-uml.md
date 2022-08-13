---
title: "Getting started with uml"
slug: "getting-started-with-uml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Talking UML
UML is a language. That means it has words and a syntax how to group the words of the language in order to make sentences, paragraphs and finally articles and essays. Like with human languages you can construct anything from blurb to artistic works. And unlike in human languages you use graphical elements like rectangles, ellipses, edges and the like rather than pure alphabet and sentence marks. Learning UML is like learning a foreign language. You first will gain a small set of passive vocabulary which you can increase in practice by stepwise improving your active vocabulary. Talking fluent UML needs quite some practice, but even with some basics you can achieve a lot and will be understood well in the UML community.

## Basic Vocabulary
UML is not about diagramming. It is about choosing the right words to express some (in most cases technical) context. Diagrams are a means to present the chosen text to humans since a visual perception is generally a good way to convey information. So you will be using graphical elements not in order to draw a picture but to convey a message. The whole language specification of UML stretches over several hundred pages of text where even experts can have difficulties understanding the details. So let's get down to some very basic language elements and ignore all the tidbits which will just confuse you in the beginning.

**Class**: is represented by a rectangle which looks like this:
[![enter image description here][1]][1]

You see three compartments where the lower two can be omitted if you want to hide details in certain contexts. The top compartment shows the class' name in bold face. Above the name an optional stereotype can appear which is meant specialize what kind of class. This stereotype can also be used to control the shape of the class or to display an icon that shows in the top right. 

The mid compartment lists attributes defined in the class. The `+` and `-` to the left denote the scope (public or private; there are a few more). It is followed by the name, a colon and the attribute's type. A multiplicity may follow enclosed in square brackets.

The lowest compartment lists all operations of the class. Again with scope and name followed with the parameter types enclosed in brackets and an optional return value.

**Edge**: is a (bend) line of manifold shapes: [![enter image description here][2]][2]

From top to bottom you can see a dependency, an association, a realization and a generalization. There are (many) more than the here shown, but with these you will have a good start.

A dependency is used as weakest relation. It just shows that an element depends on the one it points to. The example here shows a stereotype which can be used to specialize the type of dependency, but in most cases it is used without. Stereotypes can be applied to any kind of UML element and further specializes its meaning.

An association is a stronger relation between two elements. When used with classes the above example uses a diamond to the left to express that an instance of the class which is next to the diamond composes an instance at the other end of the association. This means that the left class is responsible for freeing up the memory of the object it owns.

A realization is used when the left class realizes an interface (which is a class with an applied `«interface»` stereotype) to the right. And the generalization at the bottom expresses that the left class inherits from the right one.

Note the little differences in the drawings where dotted lines, open and closed triangles at the end are used. They have great importance and you should not be sloppy in using them.

  [1]: http://i.stack.imgur.com/sQLZv.png
  [2]: http://i.stack.imgur.com/lD6N3.png

