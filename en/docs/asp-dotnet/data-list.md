---
title: "Data List"
slug: "data-list"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Syntax
 1. **ItemTemplate**:It potrays the content and layout of items within the list.This is mandatory Required
 2. **AlternatingItemTemplate**:If mentioned, determines the content and layout of alternating items. If not mentioned, ItemTemplate is used.
 3. **SeparatorTemplate** : If mentioned, is rendered between items (and alternating items). If not mentioned, a separator is not rendered.
 4. **SelectedItemTemplate** : If mentioned, determines the content and layout of the selected item. If not mentioned, ItemTemplate (AlternatingItemTemplate) is used.
 5. **EditItemTemplate** :If mentioned, determines the content and layout of the item being edited. If not mentioned, ItemTemplate (AlternatingItemTemplate, SelectedItemTemplate) is used.
 6. **HeaderTemplate**:If mentioned, determines the content and layout of the list header. If not mentioned, the header is not rendered.
 7. **FooterTemplate**:If mentioned, determines the content and layout of the list footer. If not mentioned, the footer is not rendered.

## Data Binding in asp.net
**Aspx**

    <asp:DataList runat="server" CssClass="sample" RepeatLayout="Flow" ID="dlsamplecontent" RepeatDirection="Vertical" OnItemCommand="dlsamplecontent_ItemCommand">
           <ItemStyle CssClass="tdContainer" />
               <ItemTemplate>
                 //you code 
                        </ItemTemplate>
                    </asp:DataList>


**Aspx.cs**


    public void GetSamplingContentType()
        {
            try
            {
                ErrorLogger.gstrClientMethodName = this.GetType().FullName + "_" + System.Reflection.MethodBase.GetCurrentMethod().Name + " : ";
    
             DataTable dt = new DataTable();
                dlsamplecontent.DataSource = dt;
                dlsamplecontent.DataBind();
    
            }
            catch (Exception ex)
            {
                ErrorLogger.ClientErrorLogger(ex);
            }
        }


**Item Command and Retrieving Id using Command argument**

 

     protected void dlsamplecontent_ItemCommand(object source, DataListCommandEventArgs e)
        {
    
            try
            {
                int BlogId = Convert.ToInt32(e.CommandArgument.ToString());
                if (e.CommandName == "SampleName")
                {
                   //your code 
    
                }
            }
            catch (Exception ex)
            {
                ErrorLogger.ClientErrorLogger(ex);
            }
        }



