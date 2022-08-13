---
title: "Fluent Assertions"
slug: "fluent-assertions"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

NUnit's `Assert.That()` form supports the use of constraints as its second parameter. All constraints provided out of the box by NUnit are available through the static classes `Is`, `Has` and `Does`. Constraints can be combined into fluent expressions using the built in methods `And`, `Or` and `With`. Expressions can be conveniently expanded up using the many methods in `ConstraintExpression`, such as `AtMost` and `Contains`.

## Advanced Constraint Usage
Large fluent assertions do become harder to read, but when combined with classes that have good implementations of `ToString()`,  they can generate very useful error messages.

<!-- language: lang-cs -->
    [Test]
    public void AdvancedContraintsGiveUsefulErrorMessages() {
        Assert.That(actualCollection, Has
            .Count.EqualTo(4)
            .And.Exactly(1).Property("Age").GreaterThan(60)
            .And.Some.Property("Address").Null
            .And.No.Property("Age").LessThanOrEqualTo(17));
    }

On failure, this assertion generates messages like this:

    Expected: property Count equal to 4 and exactly one item property Age greater
    than 60 and some item property Address null and not property Age less than or
    equal to 17
    But was:  < <Steve Taylor (23) lives in Newcastle
    >, <Michelle Yung (65) lives in San Francisco
    >, <Ranjit Saraman (49) lives in Milano
    >, <LaChelle Oppenheimer (16) lives in 
    > >

## Basic fluent assertion
    Assert.That(actual, Is.EqualTo(expected));

## Collections
    var a = new List<int> { 1, 2 };
    var b = new List<int> { 2, 1 };

    Assert.That (a, Is.EqualTo(b)); // fails
    Assert.That (a, Is.EquivalentTo(b)); // succeeds

