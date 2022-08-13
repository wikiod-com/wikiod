---
title: "ASP.NET Caching"
slug: "aspnet-caching"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Data Cache
ASP.Net exposes Cache API to store data in the cache for retrieval later. 

[Getting Started](http://www.4guysfromrolla.com/articles/100902-1.aspx)

**Store string**

       Cache["key"]="value";

**Retrieve string**

    var value="";
    if (Cache["key"] != null)
       value = Cache["key"].ToString();

You can also use the **Add** or the **Insert** methods.

    protected void Page_Load( object sender, EventArgs e)
    {
        if ( this.IsPostBack )
        {
            label1.Text + = "Page is posted back";
        }
        else
        {
            label1.Text + = "Page is created";
        }
        
        if ( Cache [ "item"] == null )
        {
            label1.Text + = "New item is created";
            DateTime item = DateTime.Now;
            label1.Text + = "Item is stored";
            Cache.Insert ( "item", item, null );
            DateTime.Now.AddSeconds ( 20 ), TimeSpan.Zero;
        }
    
        else
        {
            label1.Text + = "Item is accesses";
            DateTime item = ( DateTime) Cache [ "item" ];
            label1.Text + = "Time is: " + item.ToString();
            label1.Text + = <br/>";
        }
        
        label1.Text + = "<br/>";
    }
        

