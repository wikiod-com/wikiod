---
title: "Getting started with azure-web-sites"
slug: "getting-started-with-azure-web-sites"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Create a website on Azure
<h1>Create a website on Azure using portal page</h1>

<h2>Getting started</h2>
The first thing to do is to login into your Azure account and go to your [portal page](https://portal.azure.com/). If you don't have yet an account, you can start a free trial [here](https://azure.microsoft.com/en-us/free/).

After you have logged into your account, you can start following this guide.

<br /><br />

<h2>The real guide</h2>

Inside portal, press the <b>New</b> button in the left menu.

[![Portal page][1]][1]

It will appear a new panel containing a few services. In this guide we will focus only the <b>Web + Mobile</b> section.

[![Creation process][2]][2]

Now we can click on <b>Web App</b>.
In the new panel we have to enter a few details:
<ul>
    <li><b>App name</b>: This will be the name of your website</li>
    <li><b>Subscription</b>: You could have some different options based on your subscription. Pick the one you want to use
    <li><b>Resoruce group</b>: It's like a container that will hold every kind of service. In this example I created a new one, you are free to use one you had already created
    <li><b>App service plan/Location</b>: When you click on this option it will prompt out a new panel that will show all the App service plans you have created before. If it's empty, probably you need to create a new one. To do this just press the <b>Create New</b> button in the App Service panel
    <li><b>Application Insights</b>: If you press the On button, it will create a new Application Insights resource that will allow you to collect some cool statistic about your app. We will skip it for the moment as we can add it later.
</ul>
At this point, check all the data you inserted until now.
When you are sure your data are correct, put a check mark next to the <b>Pin to dashboard</b> option (so you will see your application directly on your Dashboard) and press the <b>Create</b> button.

<br />
<b>Done!</b> Now your website is <b>created!</b>

<br /><br />


<h2>Exploring your website</h2>
Now that your website has been created, click on the tile created on your dashboard. It will appear a window like this:

[![Exploring your web app][3]][3]

On the left you can see a menu that will allow you to customize your website.
Now we will focus only on the first option, the <b>Overview</b> one.
On top of that view, you can see some useful buttons (the green underlined ones). They will allow you respectively to <b>Browse</b>, <b>Stop</b>, <b>Restart</b> and <b>Delete</b> your web application.
In the same panel you can see (on the left side) also the <b>Status</b> of your website and it's <b>Location</b>.

On the right there are some very nice information such as the <b>URL</b> of our web application. We find also the <b>FTP credentials</b> and it's <b>FTP URL</b>.

Now, if you press the <b>Explore</b> button or you enter your application url in the url bar, you will see your website. Initially it will be like this:

[![Your not so cool website, for the moment][4]][4]

As you can see it's not the coolest website on the Internet, but you will learn how to transform it in one of the greatest web application ever. Or at least, the idea is that. 

  [1]: https://i.stack.imgur.com/gygP8.png
  [2]: https://i.stack.imgur.com/ymUu1.png
  [3]: https://i.stack.imgur.com/rtfWr.png
  [4]: https://i.stack.imgur.com/StXX7.png

