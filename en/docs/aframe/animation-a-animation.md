---
title: "Animation  <a-animation>"
slug: "animation--a-animation"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Animations and transitions in A-Frame are defined using the `<a-animation>` element as a child. The system is roughly based after the Web Animations specification. A-Frame uses tween.js internally.

## Attributes

Here is an overview of animation attributes. We'll go into more detail below.

| Attribute | Description                                                                                                            | Default Value  |
|-----------|------------------------------------------------------------------------------------------------------------------------|----------------|
| **attribute** | Attribute to animate. To specify a component attribute, use `componentName.property` syntax (e.g., `light.intensity`). | rotation       |
| **begin**     | Event name to wait on before beginning animation.                                                                      | ''             |
| **delay**     | Delay (in milliseconds) or event name to wait on before beginning animation.                                           | 0              |
| **direction** | Direction of the animation (between `from` and `to`). One of `alternate`, `alternateReverse`, `normal`, `reverse`.     | normal         |
| **dur**       | Duration in (milliseconds) of the animation.                                                                           | 1000           |
| **easing**    | Easing function of the animation. There are very many to choose from.                                                  | ease           |
| **end**       | Event name to wait on before stopping animation.                                                                       | ''             |
| **fill**      | Determines effect of animation when not actively in play. One of `backwards`, `both`, `forwards`, `none`.              | forwards       |
| **from**      | Starting value.                                                                                                        | Current value. |
| **repeat**    | Repeat count or `indefinite`.                                                                                          | 0              |
| **to**        | Ending value. Must be specified.                                                                                       | None           |

> **Begin**

The `begin` attribute defines when the animation should start playing.

This can either be a *number*, representing milliseconds to wait, or an *event
name* to wait for. For example, we can define an animation that waits 2 seconds
before scaling an entity.

<!-- language: lang-html -->

    <a-entity>
      <a-animation attribute="scale" begin="2000" to="2 2 2"></a-animation>
    </a-entity>

Or we can define an animation that waits for the parent element to trigger an
event named `fade` before fading an entity.

<!-- language: lang-html -->

    <a-entity id="fading-cube" geometry="primitive: box" material="opacity: 1">
      <a-animation attribute="material.opacity" begin="fade" to="0"></a-animation>
    </a-entity>

<!-- language: lang-js -->

    // Trigger an event to begin fading.
    document.querySelector('#fading-cube').emit('fade');

---

> **Direction**

The `direction` attribute defines which way to animate between the starting
value and the final value.

When we define an alternating direction, the animation will go back and forth
between the `from` and `to` values like a yo-yo. Alternating directions only
take affect when we repeat the animation.

| Value | Description |
| --- | --- |
| **alternate** | On even-numbered cycles, animate from `from` to `to`. On odd-numbered cycles, animation from `to` to `from` |
| **alternate-reverse** | On odd-numbered cycles, animate from `from` to `to`. On even-numbered cycles, animation from `to` to `from` |
| **normal** | Animate from `from` to `to`. |
| **reverse** | Animate from `to` to `from`. |

---

> **Easing**

The `easing` attribute defines the easing function of the animation, which
defaults to `ease`. There are too many easing functions to list, but we can
implicitly explain them.

One possible value is `linear`. And the basic easing functions are `ease`,
`ease-in`, `ease-out`, and `ease-in-out`.

Then there are more groups of easing functions. The above basic easing
functions prefix each group of easing functions. The groups of easing functions
are `cubic`, `quad`, `quart`, `quint`, `sine`, `expo`, `circ`, `elastic`,
`back`, and `bounce`.

For example, the `cubic` group of easing functions would consist of
`ease-cubic`, `ease-in-cubic`, `ease-out-cubic`, `ease-in-out-cubic`.

> **Fill**

The `fill` attribute defines the effect of animation when not actively in play.
Think of `fill` as what values the animation sets on the entity *before* and/or
*after* each animation cycle. Below are the possible values for `fill` and
their effects.

| Value     | Description |
| --- | --- |
| **backwards** | Before the animation starts, set the starting value to the `from` value. |
| **both**      | Combine the effects of both backwards fill and forwards fill. |
| **forwards**  | After the animation finishes, the final value will stay at the `to` value. The default fill. |
| **none**      | Before the animation starts, set the starting value to the initial value. After the animation finishes, reset the value to the initial value. |

---

> **Repeat**

The `repeat` attribute defines how often the animation repeats. We call each
repeat of the animation a cycle. Repeat can either be a number that counts down
on each animation cycle until it reaches `0` at which point the animation will
end, or we can set `repeat` to `indefinite` and the animation will loop
continuously until the animation is manually removed or stopped.

---

# EVENTS

The `<a-animation>` element emits a couple of events.

| Event Name     | Description |
| --- | --- |
| **animationend**   | Emitted when the animation finishes. In case of repeats, emitted when the repeat count reaches `0`. Not emitted for indefinite repeats. |
| **animationstart** | Emitted immediately when the animation begins playing. |




## Animating Different Types of Properties
A-Frame's animation system can animate different types of properties.

### vec3 Properties

A-Frame has standard `vec3` components (i.e., `position`, `rotation`,
and `scale`). These components consist of three factors: X, Y, and Z. We can
pass three space-delimited numbers to the `from` and `to` attributes just as we
would define them on an entity. In this case, the animation system will assume
we are animating a `vec3` value.

For example, if we want to animate an entity going from one spot to another, we
can animate the `position` component.

<!-- language: lang-html -->

    <a-entity>
      <a-animation attribute="position" from="1 1 1" to="2 4 -8"></a-animation>
    </a-entity>

### Boolean Properties

A-Frame has standard components that accept a single boolean value.  Boolean
values can be "animated" as well by flipping the boolean at the end of each
animation cycle.

For example, we can define an animation that toggles off the visibility of an
entity after 5 seconds.

<!-- language: lang-html -->

    <a-entity>
      <a-animation attribute="visible" dur="5000" to="false" repeat="indefinite"></a-animation>
    </a-entity>

### Numeric Properties

We can animate numeric attributes as well. For example, we can animate the
intensity of the light primitive.

<!-- language: lang-html -->

    <a-light intensity="1">
      <a-animation attribute="intensity" to="3"></a-animation>
    </a-light>

### Color Properties

We can animate any component property that has a color type. For example, we
can animate a box from white to red.

<!-- language: lang-html -->

    <a-entity id="blushing-cube" geometry="primitive: box">
      <a-animation attribute="material.color" from="white" to="red" dur="1000"></a-animation>
    </a-entity>

### Component Properties

We can animate a certain property of a multi-property component. To do so, we
select the component property using the dot syntax:
`componentName.propertyName`.

For example, to animate a cone's top radius, we can select the `radiusTop`
value with `geometry.radiusTop`.

<!-- language: lang-html -->

    <a-entity geometry="primitive: cone; radiusTop: 1">
      <a-animation attribute="geometry.radiusTop" to="0.5"></a-animation>
    </a-entity>



## Example Animations
As an introductory example, to define a 5-meter orbit on an entity about the Y-axis that takes 10 seconds, we can offset the position and animate the rotation. This animation starts with the initial rotation about the Y-axis of 0 degrees, and goes around 360 degrees. It???s defined with a duration of 10000 milliseconds, maintains the final value on each cycle of the animation, and loops infinitely.

<!-- language: lang-html -->

    <a-entity position="5 0 0" rotation="0 0 0">
      <a-animation attribute="rotation"
                   to="0 360 0"
                   dur="10000"
                   fill="forwards"
                   repeat="indefinite"></a-animation>
    </a-entity>

