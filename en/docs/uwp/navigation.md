---
title: "Navigation"
slug: "navigation"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

A soon as application have several pages/screens, a way of navigating among them is needed.
With UWP applications, the navigation is handled by the [Frame][1] control. It displays [Page][2] instances, support the navigation to new pages and keep an history both for backward and forward navigation


  [1]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.controls.frame.aspx
  [2]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.controls.page.aspx

## Create frame
A Frame is created like any other controls:

    <Frame x:Name="contentRoot"
           Navigated="OnNavigated"
           Navigating="OnNavigating" />

The navigated/navigating events can then be intercepted to cancel the navigation or show/hide the back button.

    private void OnNavigating(object sender, NavigatingCancelEventArgs e)
    {
        if(contentRoot.SourcePageType  == e.SourcePageType && m_currentPageParameter == e.Parameter)
       {
           // we are navigating again to the current page, we cancel the navigation
           e.Cancel    = true;
       }
    }

    private void OnNavigated(object sender, NavigationEventArgs e)
    {
        // user has navigated to a newest page, we check if we can go back and show the back button if needed.
        // we can also alter the backstack navigation history if needed
        SystemNavigationManager.GetForCurrentView().AppViewBackButtonVisibility = (contentRoot.CanGoBack ? AppViewBackButtonVisibility.Visible : AppViewBackButtonVisibility.Collapsed);  
    }

## Navigate to a newest page
To navigate to a newest page, we can use the [Navigate()][1] method from the frame.

    contentRoot.Navigate(typeof(MyPage), parameter);

where `contentRoot` is the [Frame][2] instance and `MyPage` a control inheriting from [Page][3]

In `MyPage`, the [OnNavigatedTo()][4] method will be called once the navigation will complete (ie when the user will enter the page) allowing us to triggering or finalizing the loading of the page data. The [OnNavigatedFrom()][5] method will be called when leaving the page allowing us to release what has to be released.

    public class MyPage : Page
    {
        protected override void OnNavigatedTo(NavigationEventArgs e)
        {
            // the page is now the current page of the application. We can finalized the loading of the data to display
        }

        protected override void OnNavigatedFrom(NavigationEventArgs e)
        {
            // our page will be removed from the screen, we release what has to be released
        }
    }

  [1]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.controls.frame.navigate.aspx
  [2]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.controls.frame.aspx
  [3]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.controls.page.aspx
  [4]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.controls.page.onnavigatedto.aspx
  [5]: https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.controls.page.onnavigatedfrom.aspx

## Confirming Navigation request using OnNavigatingFrom


    private bool navigateFlag = false;
    
    protected async override void OnNavigatingFrom(NavigatingCancelEventArgs e)
    {
        base.OnNavigatingFrom(e);

        if (!navigateFlag)
            {
                e.Cancel = true;

                var dialog = new MessageDialog("Navigate away?", Confir,);
                dialog.Commands.Add(new UICommand("Yes", null, 0));
                dialog.Commands.Add(new UICommand("No", null, 1);

                dialog.CancelCommandIndex = 1;
                dialog.DefaultCommandIndex = 0;

                var result = await dialog.ShowAsync();

                if (Convert.ToInt16(result.Id) != 1)
                {
                    navigateFlag= true;
                    this.Frame.Navigate(e.SourcePageType);
                }
               
            }

        }

