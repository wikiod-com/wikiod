---
title: "System.Windows.Controls.WebBrowser"
slug: "systemwindowscontrolswebbrowser"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This allows you to put a Web browser into your WPF application.

A key point to note, which is not obvious from the documentation, and you could go for years without knowing is that it defaults to behaving like InternetExplorer7, rather than your most up-to-date InternetExplorer installation (see https://weblog.west-wind.com/posts/2011/may/21/web-browser-control-specifying-the-ie-version ).

This cannot be fixed by setting a property on the control; you must either modify the pages being displayed by adding an HTML Meta Tag, or by applying a registry setting(!). (Details of both approaches are on the link above.)

For example, this bizarre design behaviour might lead you to get a message saying "Script Error"/"An error has occurred in the script on this page". Googling this error might make you think that the solution is to try to suppress the error, rather that understanding the actual problem, and applying the correct solution.

## Example of a WebBrowser within a BusyIndicator
Be aware that the WebBrowser control is not sympathetic to your XAML definition, and renders itself over the top of other things. For example, if you put it inside a BusyIndicator that has been marked as being busy, it will still render itself over the top of that control. The solution is to bind the visibility of the WebBrowser to the value that the BusyIndicator is using, and use a converter to invert the Boolean and convert it to a Visibility. For example:

        <telerik:RadBusyIndicator IsBusy="{Binding IsBusy}">
            <WebBrowser Visibility="{Binding IsBusy, Converter={StaticResource InvertBooleanToVisibilityConverter}}"/>
        </telerik:RadBusyIndicator>

