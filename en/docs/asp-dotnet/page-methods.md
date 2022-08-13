---
title: "Page Methods"
slug: "page-methods"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Parameters
| Parameter| Detail |
| ------ | ------ |
| limit  | The parameter of the C# method. You supply the argument via the Page Method.   |
| onSuccess | The JavaScript function that is executed when the Page Method call is successful. |
| onError| The JavaScript function that is executed when there is an error in the Page Method call. |

# More than one parameter #

In the example the C# function just request one parameter, if you need to pass more than one you can do it, just put them in order in your JS call and you are good to go. Ej.

    //C#
    public static int SumValues(int num1, int num2, int num3, ..., int numN)

    //JS
    PageMethods.SumValues(num1, num2, num3, ..., numN, onSuccess, onError);

# Return value #

In the `onSuccess` function the result is going to be the C# function's return value.
In the `onError` function the result is going to be the error.

## How to call it
Just add the `using` at the beginning and the `[WebMethod]` decorator to the `static` method to be called in the aspx page:

    using System.Web.Services;

    public partial class MyPage : System.Web.UI.Page
    {
        [WebMethod]
        public static int GetRandomNumberLessThan(int limit)
        {
            var r = new Random();
            return r.Next(limit);
        }
    }

In your .aspx file add a asp:ScriptManager enabling Page Methods:

    <asp:ScriptManager ID="ScriptManager1" runat="server" EnablePageMethods="true">
    </asp:ScriptManager>

Then you can call it from JS like this:

    var limit= 42 // your parameter value
    PageMethods.GetRandomNumberLessThan(limit, onSuccess, onError);
    function onSuccess(result) {
        var randomNumber = result;
        // use randomNumber...
    }
    function onError(result) {
        alert('Error: ' + result);
    }


