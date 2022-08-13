---
title: "Slider Binding Update only on Drag Ended"
slug: "slider-binding-update-only-on-drag-ended"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Parameters
| Parameter | Detail |
| ------ | ------ |
| Value (float)  | The property bound to this Dependency Property will be updated whenever the user will cease dragging the slider  |

<ul>
<li>
Make sure to reference the <i>System.Windows.Interactivity</i> assembly, so that the XAML Parser will recognize the <i>xmlns:i</i> declaration.
</li>
<li>
Note that the <i>xmlns:b</i> statement matches the namespace where the behavior implementation resides
</li>
<li>
Example assumes working knowledge of binding expressions and XAML.
</li>
</ul>

## Behavior Implementation
    using System.Windows;
    using System.Windows.Controls;
    using System.Windows.Controls.Primitives;
    using System.Windows.Interactivity;

    namespace MyBehaviorAssembly
    {

    public class SliderDragEndValueBehavior : Behavior<Slider>
    {

        public static readonly DependencyProperty ValueProperty = DependencyProperty.Register(
            "Value", typeof (float), typeof (SliderDragEndValueBehavior), new PropertyMetadata(default(float)));

        public float Value
        {
            get { return (float) GetValue(ValueProperty); }
            set { SetValue(ValueProperty, value); }
        }

        protected override void OnAttached()
        {
            RoutedEventHandler handler = AssociatedObject_DragCompleted;
            AssociatedObject.AddHandler(Thumb.DragCompletedEvent, handler);
        }

        private void AssociatedObject_DragCompleted(object sender, RoutedEventArgs e)
        {
            Value = (float) AssociatedObject.Value;
        }

        protected override void OnDetaching()
        {
            RoutedEventHandler handler = AssociatedObject_DragCompleted;
            AssociatedObject.RemoveHandler(Thumb.DragCompletedEvent, handler);
        }
    }
    }

## XAML Usage
    <UserControl x:Class="Example.View"
             xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
             xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
             xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
             xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
             xmlns:i="http://schemas.microsoft.com/expression/2010/interactivity"
                    
             xmlns:b="MyBehaviorAssembly;assembly=MyBehaviorAssembly"
                          
             mc:Ignorable="d" 
             d:DesignHeight="200" d:DesignWidth="200"
                   
             >
               <Slider>
                <i:Interaction.Behaviors>
                    <b:SliderDragEndValueBehavior
                          
                            Value="{Binding Value, Mode=OneWayToSource, UpdateSourceTrigger=PropertyChanged}"
                            
                            />
                </i:Interaction.Behaviors>
              </Slider>

    </UserControl>


