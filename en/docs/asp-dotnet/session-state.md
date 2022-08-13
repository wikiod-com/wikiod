---
title: "Session State"
slug: "session-state"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Syntax
 - Session["Session_Key"] = Obj_Value;

HTTP is stateless. ASP.NET session state is a framework that facilitates maintaining state between HTTP page requests.

Session differs from the class level variables in its ability to remain available across post-backs and different pages. For instance, a session variable created in Page1.aspx will be available if the user is redirected to Page2.aspx afterwards, within the same application.

Also, in contrast to static variables declared at the page level, the session variables are independent for different users. Meaning, changing the value of one user's session variable will not affect the value of the same variable for other users.

While `ViewState` can be used to store user's data temporarily, it doesn't allow saving data across multiple pages. In addition, the `viewstate` is part of the page and is sent to the client. As a result, any critical information related to the user cannot be saved in the `ViewState`, and that's where Session variables become useful.

## Using the Session object to store values
The `System.Web.SessionState.HttpSessionState` object provides a way to persist values between HTTP requests. In the example below, a user's preference for warnings is being saved in the session. Later on, while serving another request to the user, the application can read this preference from session and hide the warnings.

    public partial class Default : System.Web.UI.Page
    {
        public void LoadPreferences(object sender, EventArgs args)
        {
            // ... 
            // ... A DB operation that loads the user's preferences.
            // ...
 
            // Store a value with the key showWarnings
            HttpContext.Current.Session["showWarnings"] = false;
        }

        public void button2Clicked(object sender, EventArgs args)
        {
            // While processing another request, access this value.
            bool showWarnings = (bool)HttpContext.Current.Session["showWarnings"];
            lblWarnings.Visible = false;
        }
    }    

Note that the session variables are not common for all users (just like cookies), and they are persisted across multiple post-backs.


The session works by setting a cookie that contains an identifier for the users session. By default this identifier is stored in the web-server memory, along with the values stored against it. 

Here is a screenshot of the cookie set in the user's browser to keep track of the session:

[![sessionStateCookie][1]][1]


  [1]: http://i.stack.imgur.com/SJfKY.png

## Using a SQL Session Store
If you find that you have multiple servers that need to share session state, storing it in the ASP.NET process memory will not work. For example you may deploy into a web-farm environment with a load balancer that distributes requests in a round-robin fashion. In this environment a single user's requests could be served by multiple servers. 
    
In the web.config file you can configure a SQL server session store. 

    <configuration>
      <system.web>
        <sessionState 
          mode="SQLServer"
          sqlConnectionString="Data Source=localhost;Integrated Security=SSPI"
          cookieless="true"
          timeout="30" />
      </system.web>
    </configuration>

To create the sql schema use the aspnet_regsql tool.
[SampleSqlServerName] is the hostname of the SQL server. -ssadd tells the tool to create the session state database. -sstype p tells the tool to create a new database with the default name ASPState.

    aspnet_regsql.exe -S [SampleSqlServerName] -U [Username] -P [Password] -ssadd -sstype p


## Using an Amazon DynamoDB Session Store
If you don't want to use SQL server you can use Amazon's hosted Dynamo DB nosql database as a session store.

You'll need the AWS SDK. To install this from the Visual Studio nuget package manager console use the following command

    Install-Package AWSSDK 

You can then configure your sessionState provider to use a custom provider. You must specify the region and credentials, either a profile or an IAM access and secret key combination. By default this will create a table named ASP.NET_SessionState. 

    <configuration>
      <system.web>
        <sessionState
          timeout="20"
          mode="Custom"
          customProvider="DynamoDBSessionStoreProvider">
          <providers>
            <add name="DynamoDBSessionStoreProvider"
                 type="Amazon.SessionProvider.DynamoDBSessionStateStore"
                 AWSProfileName="[PROFILE]"
                 Region="[REGION]"
                 CreateIfNotExist="true"
                 />
          </providers>
        </sessionState>
      </system.web>
    </configuration>

