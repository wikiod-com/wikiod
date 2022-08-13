---
title: "Control Templates"
slug: "control-templates"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Control Templates
The default user interfaces for WPF controls are typically constructed from other controls and shapes. For example, a Button is composed of both ButtonChrome and ContentPresenter controls. The ButtonChrome provides the standard button appearance, while the ContentPresenter displays the button's content, as specified by the Content property.
Sometimes the default appearance of a control may be incongruent with the overall appearance of an application. In this case, you can use a ControlTemplate to change the appearance of the control's user interface without changing its content and behavior.

## XAML
       <Window 
          xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
          xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
          x:Class="SDKSample.ControlTemplateButtonWindow"
          Title="Button with Control Template" Height="158" Width="290">
        
          <!-- Button using an ellipse -->
          <Button Content="Click Me!" Click="button_Click">
            <Button.Template>
              <ControlTemplate TargetType="{x:Type Button}">
                <Grid Margin="5">
                  <Ellipse Stroke="DarkBlue" StrokeThickness="2">
                    <Ellipse.Fill>
                      <RadialGradientBrush Center="0.3,0.2" RadiusX="0.5" RadiusY="0.5">
                        <GradientStop Color="Azure" Offset="0.1" />
                        <GradientStop Color="CornflowerBlue" Offset="1.1" />
                      </RadialGradientBrush>
                    </Ellipse.Fill>
                  </Ellipse>
                  <ContentPresenter Name="content" HorizontalAlignment="Center" 
                    VerticalAlignment="Center"/>
                </Grid>
              </ControlTemplate>
            </Button.Template>
        
          </Button>
        
        </Window>

## C# Code
     using System.Windows;
        
        namespace SDKSample
        {
            public partial class ControlTemplateButtonWindow : Window
            {
                public ControlTemplateButtonWindow()
                {
                    InitializeComponent();
                }
        
                void button_Click(object sender, RoutedEventArgs e)
                {
                    MessageBox.Show("Hello, Windows Presentation Foundation!");
                }
            }
        }

