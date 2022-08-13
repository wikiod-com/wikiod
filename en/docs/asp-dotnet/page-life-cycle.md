---
title: "Page Life Cycle"
slug: "page-life-cycle"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Life Cycle Events
Following are the page life cycle events:

**PreInit** - PreInit is the first event in page life cycle. It checks the IsPostBack property and determines whether the page is a postback. It sets the themes and master pages, creates dynamic controls, and gets and sets profile property values. This event can be handled by overriding the OnPreInit method or creating a Page_PreInit handler.

**Init** - Init event initializes the control property and the control tree is built. This event can be handled by overriding the OnInit method or creating a Page_Init handler.

**InitComplete** - InitComplete event allows tracking of view state. All the controls turn on view-state tracking.

**LoadViewState** - LoadViewState event allows loading view state information into the controls.

**LoadPostData** - During this phase, the contents of all the input fields are defined with the <form> tag are processed.

**PreLoad** - PreLoad occurs before the post back data is loaded in the controls. This event can be handled by overriding the OnPreLoad method or creating a Page_PreLoad handler.

**Load** - The Load event is raised for the page first and then recursively for all child controls. The controls in the control tree are created. This event can be handled by overriding the OnLoad method or creating a Page_Load handler.

**LoadComplete** - The loading process is completed, control event handlers are run, and page validation takes place. This event can be handled by overriding the OnLoadComplete method or creating a Page_LoadComplete handler

**PreRender** - The PreRender event occurs just before the output is rendered. By handling this event, pages and controls can perform any updates before the output is rendered.

**PreRenderComplete** - As the PreRender event is recursively fired for all child controls, this event ensures the completion of the pre-rendering phase.

**SaveStateComplete** - State of control on the page is saved. Personalization, control state and view state information is saved. The HTML markup is generated. This stage can be handled by overriding the Render method or creating a Page_Render handler.

**UnLoad** - The UnLoad phase is the last phase of the page life cycle. It raises the UnLoad event for all controls recursively and lastly for the page itself. Final cleanup is done and all resources and references, such as database connections, are freed. This event can be handled by overriding the OnUnLoad method or creating a Page_UnLoad handler.

## Code Example
    using System;
    
    namespace myProject
    {
        public partial class WebForm1 : System.Web.UI.Page
        {
            public string PageSteps = string.Empty;
    
            //Raised after the start stage is complete and before the initialization stage begins.
            protected void Page_PreInit(object sender, EventArgs e)
            {
                PageSteps += "1 - Page_PreInit<br>";
    
                //Access to page Controls not available in this step
                //Label1.Text = "Step 1";
            }
    
            //Raised after all controls have been initialized and any skin settings have been applied.
            //The Init event of individual controls occurs before the Init event of the page.
            protected void Page_Init(object sender, EventArgs e)
            {
                PageSteps += "2 - Page_Init<br>";
    
                Label1.Text = "Step 2";
            }
    
            //Raised at the end of the page's initialization stage.
            //Only one operation takes place between the Init and InitComplete events: tracking of view state changes is turned on.
            //View state tracking enables controls to persist any values that are programmatically added to the ViewState collection.
            //Until view state tracking is turned on, any values added to view state are lost across postbacks.
            //Controls typically turn on view state tracking immediately after they raise their Init event.
            protected void Page_InitComplete(object sender, EventArgs e)
            {
                PageSteps += "3 - Page_InitComplete<br>";
    
                Label1.Text = "Step 3";
            }
    
            //Raised after the page loads view state for itself and all controls, and after it processes postback data that is included with the Request instance.
            protected override void OnPreLoad(EventArgs e)
            {
                PageSteps += "4 - OnPreLoad<br>";
    
                Label1.Text = "Step 4";
            }
    
            //The Page object calls the OnLoad method on the Page object, and then recursively does the same for each child control until the page and all controls are loaded.
            //The Load event of individual controls occurs after the Load event of the page.
            protected void Page_Load(object sender, EventArgs e)
            {
                PageSteps += "5 - Page_Load<br>";
    
                Label1.Text = "Step 5";
            }
    
            //Use these events to handle specific control events, such as a Button control's Click event or a TextBox control's TextChanged event.
            protected void btnSubmit_Click(object sender, EventArgs e)
            {
                //Step only visible on PostBack
                PageSteps += "6 - btnSubmit_Click<br>";
    
                Label1.Text = "Step 6";
            }
    
            //Raised at the end of the event-handling stage.
            protected void Page_LoadComplete(object sender, EventArgs e)
            {
                PageSteps += "7 - Page_LoadComplete<br>";
    
                Label1.Text = "Step 7";
            }
    
            //Raised after the Page object has created all controls that are required in order to render the page, including child controls of composite controls.
            //(To do this, the Page object calls EnsureChildControls for each control and for the page.)
            protected override void OnPreRender(EventArgs e)
            {
                PageSteps += "8 - OnPreRender<br>";
    
                Label1.Text = "Step 8";
            }
    
            //Raised after each data bound control whose DataSourceID property is set calls its DataBind method.
            protected override void OnPreRenderComplete(EventArgs e)
            {
                PageSteps += "9 - OnPreRenderComplete<br>";
    
                Label1.Text = "Step 9";
            }
    
    
            //Raised after view state and control state have been saved for the page and for all controls.
            //Any changes to the page or controls at this point affect rendering, but the changes will not be retrieved on the next postback.
            protected override void OnSaveStateComplete(EventArgs e)
            {
                PageSteps += "10 - OnSaveStateComplete<br><hr><br>";
    
                Label1.Text = "Step 10";
            }
    
            // Render
            //This is not an event; instead, at this stage of processing, the Page object calls this method on each control.
            //All ASP.NET Web server controls have a Render method that writes out the control's markup to send to the browser.
    
            //Raised for each control and then for the page.
            //Controls use this event to do final cleanup for specific controls, such as closing control-specific database connections
            protected void Page_UnLoad(object sender, EventArgs e)
            {
                //This last PageSteps addition will not be visible on the page
                PageSteps += "11 - Page_UnLoad<br>";
    
                //Access to page Controls not available in this step
                //Label1.Text = "Step 11";      
            }
        }
    }

Add the following code to the .aspx page to visualize the Steps in the Life Cycle.

    <b>Page Life Cycle Visualization:</b>
    <br />
    <%= PageSteps %>

[![enter image description here][1]][1]

**More information** 

 - https://msdn.microsoft.com/en-us/library/ms178472.aspx
 - https://www.tutorialspoint.com/asp.net/asp.net_life_cycle.htm
 - http://www.c-sharpcorner.com/UploadFile/8911c4/page-life-cycle-with-examples-in-Asp-Net/
 - https://www.codeproject.com/Articles/667308/ASP-NET-Page-Life-Cycle-Events


  [1]: https://i.stack.imgur.com/Qn2xF.gif

