---
title: "Working with Managed Client Side Object Model (CSOM)"
slug: "working-with-managed-client-side-object-model-csom"
draft: false
images: []
weight: 9914
type: docs
toc: true
---

- Most examples are from [MSDN][1].
- To create a .NET managed client application that uses the client object model, you must set references to two client library DLLs: Microsoft.SharePoint.Client.dll and Microsoft.SharePoint.Client.Runtime.dll. You can find it in %ProgramFiles%\Common Files\Microsoft Shared\web server extensions\16\ISAPI folder or your SharePoint server.
- or Install the Microsoft.SharePointOnline.CSOM NuGet Package, which will work "on prem" as well as in SP O365.
- Most properties are value properties and before accessing them you need to explicitly call clientContext.Load() and clientContext.ExecuteQuery(). More info here: [Call Load and ExecuteQuery Before Accessing Value Properties][2]


  [1]: https://msdn.microsoft.com/en-us/library/office/ee537247(v=office.14).aspx
  [2]: https://msdn.microsoft.com/en-us/library/office/ee535262(v=office.14).aspx

## Hello world (getting site title)
All versions of SharePoint are based around Sites (SPSite (SSOM) or Site (CSOM)) and Webs (SPWeb(SSOM) or Web(CSOM)). A site is not rendered in the UI although it does contain metadata and features that are applied to its children. A web is the basic building block that renders a UI to the user accessing the site. All Sites have a root web that holds information and/or metadata like Document Libraries. This example shows a basic call to fetch the web located on the server `MyServer` under the virtual path `sites`.

    using System;
    using Microsoft.SharePoint.Client;
    
    namespace Microsoft.SDK.SharePointServices.Samples
    {
        class RetrieveWebsite
        {
            static void Main()
            {
                // This is the URL of the target web we are interested in.
                string siteUrl = "http://MyServer/sites/MySiteCollection";    
                // The client context is allows us to queue up requests for the server
                // Note that the context can only ask questions about the site it is created for
                using (ClientContext clientContext = new ClientContext(siteUrl))
                {
                    // To make it easier to read the code, pull the target web
                    // context off of the client context and store in a variable
                    Web oWebsite = clientContext.Web;
                    // Tell the client context we want to request information about the
                    // Web from the server    
                    clientContext.Load(oWebsite);    
                    // After we are done creating the batch of information we need from the sever,
                    // request the data from SharePoint
                    clientContext.ExecuteQuery();
                    // Print the results of the query    
                    Console.WriteLine("Title: {0} Description: {1}", oWebsite.Title, oWebsite.Description);
                }
            }
        }
    }

## Item. Retrieving items (using the Include method)
This example shows how to retrieve items from the server as well as get deeper properties of each list item. By default, the server will only return the minimum amount of data to represent the object. It is up to the caller to request additional information from the server.

    ClientContext clientContext = new ClientContext(siteUrl);
    List oList = clientContext.Web.Lists.GetByTitle("Announcements");
    
    CamlQuery camlQuery = new CamlQuery();
    camlQuery.ViewXml = "<View><RowLimit>100</RowLimit></View>";
    
    ListItemCollection collListItem = oList.GetItems(camlQuery);

    // The first line of this request indicates the list item collection to load from the server
    // The second line uses a lambda to request that from the server 
    // also include additional properties in the response
    // The third though fifth lines are the properties being requested from the server
    clientContext.Load(collListItem,
         items => items.Include(
            item => item.Id,
            item => item.DisplayName,
            item => item.HasUniqueRoleAssignments));
    
    clientContext.ExecuteQuery();
    
    foreach (ListItem oListItem in collListItem)
    {
        Console.WriteLine("ID: {0} \nDisplay name: {1} \nUnique role assignments: {2}",
            oListItem.Id, oListItem.DisplayName, oListItem.HasUniqueRoleAssignments);
    }

## Web. Retrieving the properties of a Web site
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = clientContext.Web;
    clientContext.Load(oWebsite);
    clientContext.ExecuteQuery();
    Console.WriteLine("Title: {0} Description: {1}", oWebsite.Title, oWebsite.Description);

## Web. Retrieving only specified properties of a Web site
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = clientContext.Web;
    clientContext.Load(
                    oWebsite,
                    website => website.Title,
                    website => website.Created);
    clientContext.ExecuteQuery();
    Console.WriteLine("Title: {0} Created: {1}", oWebsite.Title, oWebsite.Created);

## Web. Updating the title and description of a Web site
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = context.Web;
    oWebsite.Title = "Updated Web Site";
    oWebsite.Description = "This is an updated Web site.";
    oWebsite.Update();
    clientContext.ExecuteQuery();

## Web. Creating a Web site
    string siteUrl = "http://MyServer/sites/MySiteCollection";
    string blogDescription = "A new blog Web site.";
    int blogLanguage = 1033;
    string blogTitle = "Blog Web Site";
    string blogUrl = "blogwebsite";
    bool blogPermissions = false;
    string webTemplate = "BLOG#0";
    
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = clientContext.Web;
    
    WebCreationInformation webCreateInfo = new WebCreationInformation();
    webCreateInfo.Description = blogDescription;
    webCreateInfo.Language = blogLanguage;
    webCreateInfo.Title = blogTitle;
    webCreateInfo.Url = blogUrl;
    webCreateInfo.UseSamePermissionsAsParentSite = blogPermissions;
    webCreateInfo.WebTemplate = webTemplate;
    
    Web oNewWebsite = oWebsite.Webs.Add(webCreateInfo);
    
    clientContext.Load(
        oNewWebsite,
        website => website.ServerRelativeUrl,
        website => website.Created);
    
    clientContext.ExecuteQuery();
    
    Console.WriteLine("Server-relative Url: {0} Created: {1}", oNewWebsite.ServerRelativeUrl, oNewWebsite.Created);



## List. Retrieving all properties of all lists in a Web site
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = clientContext.Web;
    ListCollection collList = oWebsite.Lists;
    
    clientContext.Load(collList);
    
    clientContext.ExecuteQuery();
    
    foreach (List oList in collList)
    {
        Console.WriteLine("Title: {0} Created: {1}", oList.Title, oList.Created.ToString());
    }

## List. Retrieving only specified properties of lists
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = clientContext.Web;
    ListCollection collList = oWebsite.Lists;
    
    clientContext.Load(
        collList,
        lists => lists.Include(
            list => list.Title, 
            list => list.Id));
    
    clientContext.ExecuteQuery();
    
    foreach (List oList in collList)
    {
        Console.WriteLine("Title: {0} ID: {1}", oList.Title, oList.Id.ToString("D"));
    }

## List. Storing retrieved lists in a collection
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = clientContext.Web;
    ListCollection collList = oWebsite.Lists;
    
    IEnumerable<List> resultCollection = clientContext.LoadQuery(
        collList.Include(
            list=>list.Title,
            list=>list.Id));
    
    clientContext.ExecuteQuery();
    
    foreach (List oList in resultCollection)
    {
        Console.WriteLine("Title: {0} ID: {1}", oList.Title, oList.Id.ToString("D"));
    }

## List. Retrieving list fields from a Web site
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = clientContext.Web;
    ListCollection collList = oWebsite.Lists;
    
    IEnumerable<SP.List> listInfo = clientContext.LoadQuery(
        collList.Include(
            list => list.Title,
            list => list.Fields.Include(
                field => field.Title,
                field => field.InternalName)));
     
     clientContext.ExecuteQuery();
    
    foreach (SP.List oList in listInfo)
    {
        FieldCollection collField = oList.Fields;
    
        foreach (SP.Field oField in collField)
        {
            Regex regEx = new Regex("name", RegexOptions.IgnoreCase);
            
            if (regEx.IsMatch(oField.InternalName))
            {
                Console.WriteLine("List: {0} \n\t Field Title: {1} \n\t Field Internal Name: {2}", 
                    oList.Title, oField.Title, oField.InternalName);
            }
        }
    }

## List. Creating and updating a list
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = clientContext.Web;
    
    ListCreationInformation listCreationInfo = new ListCreationInformation();
    listCreationInfo.Title = "My Announcements List";
    listCreationInfo.TemplateType = (int)ListTemplateType.Announcements;
    
    List oList = oWebsite.Lists.Add(listCreationInfo);
    
    clientContext.ExecuteQuery();

## List. Adding a field to a list
    ClientContext clientContext = new ClientContext(siteUrl);
    
    SP.List oList = clientContext.Web.Lists.GetByTitle("Announcements");
    
    SP.Field oField = oList.Fields.AddFieldAsXml("<Field DisplayName='MyField' Type='Number' />",
    
        true, AddFieldOptions.DefaultValue);
    
    SP.FieldNumber fieldNumber = clientContext.CastTo<FieldNumber>(oField);
    fieldNumber.MaximumValue = 100;
    fieldNumber.MinimumValue = 35;
    
    fieldNumber.Update();
    
    clientContext.ExecuteQuery();

## List. Deleting a list
    ClientContext clientContext = new ClientContext(siteUrl);
    Web oWebsite = clientContext.Web;
    
    List oList = oWebsite.Lists.GetByTitle("My Announcements List");
    
    oList.DeleteObject();
    
    clientContext.ExecuteQuery();

## Item. Retrieving items from a list
    ClientContext clientContext = new ClientContext(siteUrl);
    SP.List oList = clientContext.Web.Lists.GetByTitle("Announcements");
    
    CamlQuery camlQuery = new CamlQuery();
    camlQuery.ViewXml = "<View><Query><Where><Geq><FieldRef Name='ID'/>" +
        "<Value Type='Number'>10</Value></Geq></Where></Query><RowLimit>100</RowLimit></View>";
    ListItemCollection collListItem = oList.GetItems(camlQuery);
    
    clientContext.Load(collListItem);
    
    clientContext.ExecuteQuery();
    
    foreach (ListItem oListItem in collListItem)
    {
        Console.WriteLine("ID: {0} \nTitle: {1} \nBody: {2}", oListItem.Id, oListItem["Title"], oListItem["Body"]);
    }

## Item. Retrieving specific fields from a specified number of items
    ClientContext clientContext = new ClientContext(siteUrl);
    SP.List oList = clientContext.Web.Lists.GetByTitle("Announcements");
    
    CamlQuery camlQuery = new CamlQuery();
    ListItemCollection collListItem = oList.GetItems(camlQuery);
    
    clientContext.Load(
        collListItem,
        items => items.Take(5).Include(
        item => item["Title"],
        item => item["Body"]));
    
    clientContext.ExecuteQuery();
    
    foreach (ListItem oListItem in collListItem)
    {
        Console.WriteLine("Title: {0} \nBody: {1}\n", oListItem["Title"], oListItem["Body"]);
    }

## Item. Retrieving items from all the lists in a Web site
    ClientContext clientContext = new ClientContext(siteUrl);
    ListCollection collList = clientContext.Web.Lists;
    
    clientContext.Load(
        collList,
        lists => lists.Where(
            list => list.Hidden == false).Include(
            list => list.Title,
            list => list.Items.Take(10)));
    
    clientContext.ExecuteQuery();
    
    foreach (SP.List oList in clientContext.Web.Lists)
    {
        string listTitle = oList.Title;
        int itemCount = oList.Items.Count;
    
        Console.WriteLine("List {0} returned with {1} items", listTitle, itemCount);
    }

## Item. Retrieving items using list item collection position
    ClientContext clientContext = new ClientContext(siteUrl);
    SP.List oList = clientContext.Web.Lists.GetByTitle("Announcements");
    
    ListItemCollectionPosition itemPosition = null;
    
    while (true)
    {
        CamlQuery camlQuery = new CamlQuery();
    
        camlQuery.ListItemCollectionPosition = itemPosition;
    
        camlQuery.ViewXml = "<View><ViewFields><FieldRef Name='ID'/>" + 
            "<FieldRef Name='Title'/><FieldRef Name='Body'/>" + 
            "</ViewFields><RowLimit>5</RowLimit></View>";
    
        ListItemCollection collListItem = oList.GetItems(camlQuery);
    
        clientContext.Load(collListItem);
    
        clientContext.ExecuteQuery();
    
        itemPosition = collListItem.ListItemCollectionPosition;
    
        foreach (ListItem oListItem in collListItem)
        {
            Console.WriteLine("Title: {0}: \nBody: {1}", oListItem["Title"], oListItem["Body"]);
        }
    
        if (itemPosition == null)
        {
            break;
        }
    
        Console.WriteLine("\n" + itemPosition.PagingInfo + "\n");
    }

## Item. Creating a list item
When creating a new list item, its fields can be set using syntax similar to string arrays. Note that these fields are not created on the fly and are defined by the schema of the list. These fields (or columns) must exist on the server otherwise the create will fail. All list items will have the Title field. Some lists may have required fields that must be filled out before the item will be published in the list. 

In this example, the list is using the Announcements template. In addition to the title field, the list includes the Body field that will display the contents of the announcement on the list. 

    ClientContext clientContext = new ClientContext(siteUrl);
    List oList = clientContext.Web.Lists.GetByTitle("Announcements");
    
    ListItemCreationInformation itemCreateInfo = new ListItemCreationInformation();
    ListItem oListItem = oList.AddItem(itemCreateInfo);
    oListItem["Title"] = "My New Item!";
    oListItem["Body"] = "Hello World!";
    
    oListItem.Update();
    
    clientContext.ExecuteQuery(); 

## Item. Updating a list item
    ClientContext clientContext = new ClientContext(siteUrl);
    SP.List oList = clientContext.Web.Lists.GetByTitle("Announcements");
    ListItem oListItem = oList.Items.GetById(3);
    
    oListItem["Title"] = "My Updated Title.";
    
    oListItem.Update();
    
    clientContext.ExecuteQuery();

## Item. Deleting a list item
    ClientContext clientContext = new ClientContext(siteUrl);
    SP.List oList = clientContext.Web.Lists.GetByTitle("Announcements");
    ListItem oListItem = oList.GetItemById(2);
    
    oListItem.DeleteObject();
    
    clientContext.ExecuteQuery(); 

## Groups. Retrieving all users from a SharePoint group
    ClientContext clientContext = new ClientContext("http://MyServer/sites/MySiteCollection");
    GroupCollection collGroup = clientContext.Web.SiteGroups;
    Group oGroup = collGroup.GetById(7);
    UserCollection collUser = oGroup.Users;
    
    clientContext.Load(collUser);
    
    clientContext.ExecuteQuery();
    
    foreach (User oUser in collUser)
    {
        Console.WriteLine("User: {0}  ID: {1} Email: {2} Login Name: {3}", 
            oUser.Title, oUser.Id, oUser.Email, oUser.LoginName);
    }

## Groups. Retrieving specific properties of users
    ClientContext clientContext = new ClientContext("http://MyServer/sites/MySiteCollection");
    GroupCollection collGroup = clientContext.Web.SiteGroups;
    Group oGroup = collGroup.GetById(7);
    UserCollection collUser = oGroup.Users;
    
    clientContext.Load(collUser,
        users => users.Include(
            user => user.Title,
            user => user.LoginName,
            user => user.Email));
    
    clientContext.ExecuteQuery();
    
    foreach (User oUser in collUser)
    {
        Console.WriteLine("User: {0} Login name: {1} Email: {2}", 
            oUser.Title, oUser.LoginName, oUser.Email);
    }

## Groups. Retrieving all users in all groups of a site collection
    ClientContext clientContext = new ClientContext("http://MyServer/sites/MySiteCollection");
    GroupCollection collGroup = clientContext.Web.SiteGroups;
    
    clientContext.Load(collGroup);
    
    clientContext.Load(collGroup,
        groups => groups.Include(
            group => group.Users));
    
    clientContext.ExecuteQuery();
    
    foreach (Group oGroup in collGroup)
    {
        UserCollection collUser = oGroup.Users;
    
        foreach (User oUser in collUser)
        {
            Console.WriteLine("Group ID: {0} Group Title: {1} User: {2} Login Name: {3}", 
                oGroup.Id, oGroup.Title, oUser.Title, oUser.LoginName);
        }
    }  

## Groups. Adding a user to a SharePoint group
    ClientContext clientContext = new ClientContext("http://MyServer/sites/MySiteCollection ");
    GroupCollection collGroup = clientContext.Web.SiteGroups;
    Group oGroup = collGroup.GetById(6);
    
    UserCreationInformation userCreationInfo = new UserCreationInformation();
    userCreationInfo.Email = "alias@somewhere.com";
    userCreationInfo.LoginName = @"DOMAIN\alias";
    userCreationInfo.Title = "John";
    
    User oUser = oGroup.Users.Add(userCreationInfo);
    
    clientContext.ExecuteQuery(); 

## Roles. Creating a role definition
    ClientContext oClientContext = new ClientContext("http://MyServer/sites/MySiteCollection");
    
    Web oWebsite = clientContext.Web;
    
    BasePermissions permissions = new BasePermissions();
    permissions.Set(PermissionKind.CreateAlerts);
    permissions.Set(PermissionKind.ManageAlerts);
    
    RoleDefinitionCreationInformation roleCreationInfo = new RoleDefinitionCreationInformation();
    
    roleCreationInfo.BasePermissions = permissions;
    roleCreationInfo.Description = "A new role with create and manage alerts permission";
    roleCreationInfo.Name = "Create and Manage Alerts";
    roleCreationInfo.Order = 4;
    
    RoleDefinition oRoleDefinition = oWebsite.RoleDefinitions.Add(roleCreationInfo);
    
    clientContext.ExecuteQuery();
    
    Console.WriteLine("{0} role created.", oRoleDefinition.Name);

## Roles. Assigning a user to a role on a Web site
    ClientContext oClientContext = new ClientContext("http://MyServer/sites/MySiteCollection/MyWebSite");
    Web oWebsite = clientContext.Web;
    
    Principal oUser = oWebsite.SiteUsers.GetByLoginName(@"DOMAIN\alias");
    
    RoleDefinition oRoleDefinition = oWebsite.RoleDefinitions.GetByName("Create and Manage Alerts");
    RoleDefinitionBindingCollection collRoleDefinitionBinding = new RoleDefinitionBindingCollection(clientContext);
    collRoleDefinitionBinding.Add(oRoleDefinition);
    
    RoleAssignment oRoleAssignment = oWebsite.RoleAssignments.Add(oUser, collRoleDefinitionBinding);
    
    clientContext.Load(oUser,
        user => user.Title);
    
    clientContext.Load(oRoleDefinition,
        role => role.Name);
    
    clientContext.ExecuteQuery();
    
    Console.WriteLine("{0} added with {1} role.", oUser.Title, oRoleDefinition.Name);

## Roles. Creating a SharePoint group and adding the group to a role
    ClientContext oClientContext = new ClientContext("http://MyServer/sites/MySiteCollection/MyWebSite");
    Web oWebsite = clientContext.Web;
    
    GroupCreationInformation groupCreationInfo = new GroupCreationInformation();
    groupCreationInfo.Title = "My New Group";
    groupCreationInfo.Description = "Description of new group.";
    Group oGroup = oWebsite.SiteGroups.Add(groupCreationInfo);
    
    RoleDefinitionBindingCollection collRoleDefinitionBinding = new RoleDefinitionBindingCollection(clientContext);
    
    RoleDefinition oRoleDefinition = oWebsite.RoleDefinitions.GetByType(RoleType.Contributor);
    
    collRoleDefinitionBinding.Add(oRoleDefinition);
    
    oWebsite.RoleAssignments.Add(oGroup, collRoleDefinitionBinding);
    
    clientContext.Load(oGroup,
        group => group.Title);
    
    clientContext.Load(oRoleDefinition,
        role => role.Name);
    
    clientContext.ExecuteQuery();
    
    Console.WriteLine("{0} created and assigned {1} role.", oGroup.Title, oRoleDefinition.Name);        }



## Permissions. Breaking the security inheritance of a list
    string siteUrl = "http://MyServer/sites/MySiteCollection";
    ClientContext oContext = new ClientContext(siteUrl);
    SP.List oList = oContext.Web.Lists.GetByTitle("Announcements");
    
    oList.BreakRoleInheritance(true, false);
    
    oContext.ExecuteQuery();

## Permissions. Breaking the security inheritance of a document and adding a user as reader
    ClientContext clientContext = new ClientContext(siteUrl);
    SP.List oList = clientContext.Web.Lists.GetByTitle("MyList");
    
    int itemId = 3;
    ListItem oListItem = oList.Items.GetById(itemId);
    
    oListItem.BreakRoleInheritance(false);
    
    User oUser = clientContext.Web.SiteUsers.GetByLoginName(@"DOMAIN\alias");
    
    RoleDefinitionBindingCollection collRoleDefinitionBinding = new RoleDefinitionBindingCollection(clientContext);
    
    collRoleDefinitionBinding.Add(clientContext.Web.RoleDefinitions.GetByType(RoleType.Reader));
    
    oListItem.RoleAssignments.Add(oUser, collRoleDefinitionBinding);
    
    clientContext.ExecuteQuery();

## Permissions. Breaking the security inheritance of a document and changing the permissions of a user
    ClientContext clientContext = new ClientContext(siteUrl);
    SP.List oList = clientContext.Web.Lists.GetByTitle("MyList");
    
    int itemId = 2;
    ListItem oListItem = oList.Items.GetById(itemId);
    
    oListItem.BreakRoleInheritance(true);
    
    User oUser = clientContext.Web.SiteUsers.GetByLoginName(@"DOMAIN\alias");
    oListItem.RoleAssignments.GetByPrincipal(oUser).DeleteObject();
    
    RoleDefinitionBindingCollection collRollDefinitionBinding = new RoleDefinitionBindingCollection(clientContext);
    
    collRollDefinitionBinding.Add(clientContext.Web.RoleDefinitions.GetByType(RoleType.Reader));
    
    oListItem.RoleAssignments.Add(oUser, collRollDefinitionBinding);
    
    clientContext.ExecuteQuery();

## Custom action. Adding a user custom action for list items
    string urlWebsite = "http://MyServer/sites/MySiteCollection";
    ClientContext clientContext = new ClientContext(urlWebsite);
    Web oWebsite = clientContext.Web;
    
    List oList = oWebsite.Lists.GetByTitle("My List");
    UserCustomActionCollection collUserCustomAction = oList.UserCustomActions;
    
    UserCustomAction oUserCustomAction = collUserCustomAction.Add();
    oUserCustomAction.Location = "EditControlBlock";
    oUserCustomAction.Sequence = 100;
    oUserCustomAction.Title = "My First User Custom Action";
    oUserCustomAction.Url = urlWebsite + @"/_layouts/MyPage.aspx";
    oUserCustomAction.Update();
    
    clientContext.Load(oList,
        list => list.UserCustomActions);
    
    clientContext.ExecuteQuery();

## Custom action. Modifying a user custom action
    string urlWebsite = "http://MyServer/sites/SiteCollection";
    ClientContext clientContext = new ClientContext(urlWebsite);
    Web oWebsite = clientContext.Web;
    
    List oList = oWebsite.Lists.GetByTitle("My List");
    UserCustomActionCollection collUserCustomAction = oList.UserCustomActions;
    
    clientContext.Load(collUserCustomAction,
        userCustomActions => userCustomActions.Include(
            userCustomAction => userCustomAction.Title));
    
    clientContext.ExecuteQuery();
    
    foreach (UserCustomAction oUserCustomAction in collUserCustomAction)
    {
        if (oUserCustomAction.Title == "My First User Custom Action")
        {
            oUserCustomAction.ImageUrl = "http://MyServer/_layouts/images/MyIcon.png";
            oUserCustomAction.Update();
    
            clientContext.ExecuteQuery();
        }
    }

## Custom action. Adding a user custom action to the site actions of a Web site 
    string urlWebsite = "http://MyServer/sites/MySiteCollection";
    ClientContext clientContext = new ClientContext(urlWebsite);
    
    Web oWebsite = clientContext.Web;
    UserCustomActionCollection collUserCustomAction = oWebsite.UserCustomActions;
    
    UserCustomAction oUserCustomAction = collUserCustomAction.Add();
    
    oUserCustomAction.Location = "Microsoft.SharePoint.StandardMenu";
    oUserCustomAction.Group = "SiteActions";
    oUserCustomAction.Sequence = 101;
    oUserCustomAction.Title = "Website User Custom Action";
    oUserCustomAction.Description = "This description appears on the Site Actions menu.";
    oUserCustomAction.Url = urlWebsite + @"/_layouts/MyPage.aspx";
    
    oUserCustomAction.Update();
    
    clientContext.Load(oWebsite,
        webSite => webSite.UserCustomActions);
    
    clientContext.ExecuteQuery();

## Web part. Updating the title of a Web Part
    ClientContext oClientContext = new ClientContext("http://MyServer/sites/MySiteCollection");
    File oFile = oClientContext.Web.GetFileByServerRelativeUrl("Default.aspx");
    LimitedWebPartManager limitedWebPartManager = oFile.GetLimitedWebPartManager(PersonalizationScope.Shared);
    
    oClientContext.Load(limitedWebPartManager.WebParts,
        wps => wps.Include(
        wp => wp.WebPart.Title));
    
    oClientContext.ExecuteQuery();
    
    if (limitedWebPartManager.WebParts.Count == 0)
    {
        throw new Exception("No Web Parts on this page.");
    }
    
    WebPartDefinition oWebPartDefinition = limitedWebPartManager.WebParts[1];
    WebPart oWebPart = oWebPartDefinition.WebPart;
    oWebPart.Title = "My New Web Part Title";
    
    oWebPartDefinition.SaveWebPartChanges();
    
    oClientContext.ExecuteQuery();

## Web part. Adding a Web Part to a page
    ClientContext oClientContext = new ClientContext("http://MyServer/sites/MySiteCollection");
    File oFile = oClientContext.Web.GetFileByServerRelativeUrl("Default.aspx");
    LimitedWebPartManager limitedWebPartManager = oFile.GetLimitedWebPartManager(PersonalizationScope.Shared);
    
    string xmlWebPart = "<?xml version=\"1.0\" encoding=\"utf-8\"?>" + 
        "<WebPart xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" + 
        " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" + 
        " xmlns=\"http://schemas.microsoft.com/WebPart/v2\">" + 
        "<Title>My Web Part</Title><FrameType>Default</FrameType>" + 
        "<Description>Use for formatted text, tables, and images.</Description>" +
        "<IsIncluded>true</IsIncluded><ZoneID></ZoneID><PartOrder>0</PartOrder>" +
        "<FrameState>Normal</FrameState><Height /><Width /><AllowRemove>true</AllowRemove>" +
        "<AllowZoneChange>true</AllowZoneChange><AllowMinimize>true</AllowMinimize>" +
        "<AllowConnect>true</AllowConnect><AllowEdit>true</AllowEdit>" +
        "<AllowHide>true</AllowHide><IsVisible>true</IsVisible><DetailLink /><HelpLink />" +
        "<HelpMode>Modeless</HelpMode><Dir>Default</Dir><PartImageSmall />" +
        "<MissingAssembly>Cannot import this Web Part.</MissingAssembly>" +
        "<PartImageLarge>/_layouts/images/mscontl.gif</PartImageLarge><IsIncludedFilter />" +
        "<Assembly>Microsoft.SharePoint, Version=13.0.0.0, Culture=neutral, " + 
        "PublicKeyToken=94de0004b6e3fcc5</Assembly>" +
        "<TypeName>Microsoft.SharePoint.WebPartPages.ContentEditorWebPart</TypeName>" +
        "<ContentLink xmlns=\"http://schemas.microsoft.com/WebPart/v2/ContentEditor\" />" +
        "<Content xmlns=\"http://schemas.microsoft.com/WebPart/v2/ContentEditor\">" +
        "<![CDATA[This is a first paragraph!<DIV>&nbsp;</DIV>And this is a second paragraph.]]></Content>" + 
        "<PartStorage xmlns=\"http://schemas.microsoft.com/WebPart/v2/ContentEditor\" /></WebPart>";
    
    WebPartDefinition oWebPartDefinition = limitedWebPartManager.ImportWebPart(xmlWebPart);
    
    limitedWebPartManager.AddWebPart(oWebPartDefinition.WebPart, "Left", 1);
    
    oClientContext.ExecuteQuery();  

## Web part. Deleting a Web Part from a page
    ClientContext oClientContext = new ClientContext("http://MyServer/sites/MySiteCollection");
    File oFile = oClientContext.Web.GetFileByServerRelativeUrl("/sites/MySiteCollection/SitePages/Home.aspx ");
    LimitedWebPartManager limitedWebPartManager = oFile.GetLimitedWebPartManager(PersonalizationScope.Shared);
    
    oClientContext.Load(limitedWebPartManager.WebParts);
                
    oClientContext.ExecuteQuery();
    
    if (limitedWebPartManager.WebParts.Count == 0)
    {
        throw new Exception("No Web Parts to delete.");
    }
    
    WebPartDefinition webPartDefinition = limitedWebPartManager.WebParts[0];
    
    webPartDefinition.DeleteWebPart();
    
    oClientContext.ExecuteQuery();      

## Context. Using a credential cache for elevated execution of code
While server side-code can run with elevated privileges, there is not an equivalent method to elevate privileges in client-side code (for obvious security reasons). As an alternative, you can specify credentials to emulate the access of a specific user or service account.

To specify credentials, build and populate a [**`CredentialCache`**](https://msdn.microsoft.com/en-us/library/system.net.credentialcache(v=vs.110).aspx) object, then assign it to your `ClientContext` object's `Credentials` property.

The example below emulates the application pool account, and assumes an on-premises SharePoint 2013 environment with NTLM.


    using System.Net;
    using Microsoft.SharePoint.Client;

    using (ClientContext ctx = new ClientContext("https://onpremises.local/sites/demo/"))
    {
        // need the web object
        ctx.Load(ctx.Web);
        ctx.ExecuteQuery();

        // here the default network credentials relate to the identity of the account
        // running the App Pool of your web application.
        CredentialCache credCache = new CredentialCache();        
        cc.Add(new Uri(ctx.Web.Url), "NTLM", CredentialCache.DefaultNetworkCredentials);

        ctx.Credentials = credCache;
        ctx.AuthenticationMode = ClientAuthentication.Default;
        ctx.ExecuteQuery();

        // do stuff as elevated app pool account
    }

Note that granting the application pool account elevated privileges in SharePoint is against best practice, but that any relevant network credentials could be used in its place.

