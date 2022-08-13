---
title: "WPF Behaviors"
slug: "wpf-behaviors"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

WPF behaviors allow a developer to alter the way WPF controls acts in response to system and user events.

Behaviors inherit from the `Behavior` class of the `System.Windows.Interactity` namespace.  This namespace is a part of the overarching Expression Blend SDK, but a lighter version, suitable for behavior libraries, is available as a [nuget package][1].


  [1]: https://www.nuget.org/packages/System.Windows.Interactivity.WPF/

## Simple Behavior to Intercept Mouse Wheel Events
**Implementing the Behavior**

This behavior will cause mouse wheel events from an inner `ScrollViewer` to bubble up to the parent `ScrollViewer` when the inner one is at either its upper or lower limit.  Without this behavior, the events will never make it out of the inner `ScrollViewer`.

    public class BubbleMouseWheelEvents : Behavior<UIElement>
    {
        protected override void OnAttached()
        {
            base.OnAttached();
            this.AssociatedObject.PreviewMouseWheel += PreviewMouseWheel;
        }

        protected override void OnDetaching()
        {
            this.AssociatedObject.PreviewMouseWheel -= PreviewMouseWheel;
            base.OnDetaching();
        }

        private void PreviewMouseWheel(object sender, MouseWheelEventArgs e)
        {
            var scrollViewer = AssociatedObject.GetChildOf<ScrollViewer>(includeSelf: true);
            var scrollPos = scrollViewer.ContentVerticalOffset;
            if ((scrollPos == scrollViewer.ScrollableHeight && e.Delta < 0) || (scrollPos == 0 && e.Delta > 0))
            {
                UIElement rerouteTo = AssociatedObject;
                if (ReferenceEquals(scrollViewer, AssociatedObject))
                {
                    rerouteTo = (UIElement) VisualTreeHelper.GetParent(AssociatedObject);
                }

                e.Handled = true;
                var e2 = new MouseWheelEventArgs(e.MouseDevice, e.Timestamp, e.Delta);
                e2.RoutedEvent = UIElement.MouseWheelEvent;
                rerouteTo.RaiseEvent(e2);
            }
        }
    }

Behaviors subclass the `Behavior<T>` base-class, with `T` being the type of control that it is able to attach to, in this case `UIElement`.  When the `Behavior` is instantiated from XAML, the `OnAttached` method is called.  This method allows the behavior to hook in to events from the control it is attached to (via `AssociatedControl`).  A similar method, `OnDetached` is called when the behavior need to be unhooked from the associated element.  Care should be taken to remove any event handlers, or otherwise clean up objects to avoid memory leaks.

This behavior hooks in to the `PreviewMouseWheel` event, which gives it a change to intercept the event before the `ScrollViewer` has a chance to see it.  It checks the position to see if it needs to forward the event up the visual tree to any `ScrollViewer` higher hierarchy.  If so, it sets `e.Handled` to `true` to prevent the default action of the event.  It then raises a new `MouseWheelEvent` routed to `AssociatedObject`.  Otherwise, the event is routed as normal.


----------

**Attaching the Behavior to an Element in XAML**

First, the `interactivity` xml-namespace must be brought in to scope before it can be used in XAML.  Add the following line to the namespaces of your XAML.

> xmlns:interactivity="http://schemas.microsoft.com/expression/2010/interactivity"


The behavior can be attached like so:

    <ScrollViewer>
        <!--...Content...-->
        <ScrollViewer>
            <interactivity:Interaction.Behaviors>
                <behaviors:BubbleMouseWheelEvents />
            </interactivity:Interaction.Behaviors>
            <!--...Content...-->
        </ScrollViewer>
        <!--...Content...-->.
    </ScrollViewer>

This creates a `Behaviors` collection as an Attached Property on the inner `ScrollViewer` that contains a `BubbleMouseWheelEvents` behavior.

This particular behavior could also be attached to any existing control that contains an embedded `ScrollViewer`, such as a `GridView`, and it would still function correctly.



