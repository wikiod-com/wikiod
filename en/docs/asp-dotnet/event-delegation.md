---
title: "Event Delegation"
slug: "event-delegation"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
 1. `public delegate void ActionClick();` 
    
        public event ActionClick OnResetClick;




I haven't found any disadvantages in this approach but there are a few things which make this a little problematic.

 1. You need to add an event handler for each and every event. If you do
    not add the event handlers in the OnInit event of the page, you
    might face some problems that on page post back, you will lose the
    event assignment (as ASP.NET is stateless, which is not the case
    with Windows controls).
 2. In this approach, you need to respect the page life cycle events.
    Some times when you are working on the Designer, there might be a
    case when the event handler gets lost without your notice.
 3. Even if you have not added the event handler, you will not get any
    errors or warnings. If you have multiple pages for performing the
    same action, there is no guarantee that all the method names will be
    same; the developer can choose their own method names, which reduces
    the maintainability of the code.

## Delegation of Event from User Control to aspx

Normally, we opt this approach if we want complete encapsulation and don't want to make our methods public.



**Ascx**

 

    <div style="width: 100%;">
      <asp:Button ID="btnAdd" runat="server" 
        Text="Add" OnClick="btnAdd_Click"></asp:button>
      <asp:button id="btnEdit" runat="server" 
        text="Edit" onclick="btnEdit_Click"> </asp:button>
      <asp:button id="btnDelete" runat="server" 
        text="Delete" onclick="btnDelete_Click">    </asp:Button>
      <asp:button id="btnReset" runat="server" 
        text="Reset" onclick="btnReset_Click"></asp:button>
    </div>

**Ascx.cs**

 

    public delegate void ActionClick();
    
    public partial class EventDelegation : System.Web.UI.UserControl
    {
        public event ActionClick OnAddClick;
        public event ActionClick OnDeleteClick;
        public event ActionClick OnEditClick;
        public event ActionClick OnResetClick;
        protected void btnAdd_Click(object sender, EventArgs e)
        {
            if(OnAddClick!= null)
            {
                OnAddClick();
            }
        }
    
        protected void btnEdit_Click(object sender, EventArgs e)
        {
            if (OnEditClick != null)
            {
                OnEditClick();
            }
        }
    
        protected void btnDelete_Click(object sender, EventArgs e)
        {
            if(OnDeleteClick!= null)
            {
                OnDeleteClick();
            }
        }
    
        protected void btnReset_Click(object sender, EventArgs e)
        {
            if(OnResetClick!= null)
            {
                OnResetClick();
            }
        }
    }

The user control specifies some public events like `OnAddClick`, `OnEditClick`,etc., which declare a delegate. Anyone who wants to use these events needs to add the EventHandler to execute when the corresponding button click event occurs.

**Aspx Design**

   

    <%@ Register src="Controls/EventDelegation.ascx" 
           tagname="EventDelegation" tagprefix="uc1" %>
        
        <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" 
           "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
        
        <html xmlns="http://www.w3.org/1999/xhtml" >
        <head runat="server">
            <title></title>
        </head>
        <body>
            <form id="form1" runat="server">
            <div>
                <uc1:Direct ID="Direct1" runat="server" />
            </div>
            </form>
        </body>
        </html>

**Aspx.cs**

 

    public partial class EventDelegation : System.Web.UI.Page
    {
        protected override void OnInit(EventArgs e)
        {
            base.OnInit(e);
            ActionControl.OnAddClick += ActionControl_OnAddClick;
            ActionControl.OnDeleteClick += ActionControl_OnDeleteClick;
            ActionControl.OnEditClick += ActionControl_OnEditClick;
            ActionControl.OnResetClick += ActionControl_OnResetClick;
        }
    
        private void ActionControl_OnResetClick()
        {
            Response.Write("Reset done.");
        }
    
        private void ActionControl_OnEditClick()
        {
            Response.Write("Updated.");
        }
    
        private void ActionControl_OnDeleteClick()
        {
            Response.Write("Deleted.");
        }
    
        private void ActionControl_OnAddClick()
        {
            Response.Write("Added.");
        }
    }



