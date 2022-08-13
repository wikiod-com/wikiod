---
title: "Using GitHub Buttons"
slug: "using-github-buttons"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

What are GitHub buttons? GitHub buttons are buttons that you can add to your website that redirects users to any repository that you like!

Credits:

 - Gif images recorded with [Recordit][1]
 - Static images taken with Snipping Tool
 - Code editor used in full tutorials was [codepen.io][2]


  [1]: http://recordit.co/
  [2]: https://codepen.io/

## Follow Button
A follow button is a button that links to a GitHub user page and prompts the user to follow the user. Here's how to create one:

 1. Go onto [github:buttons][1]
 2. Click "Follow"

[![Gif of user clicking "Follow"][2]][2]
 
3. Place your GitHub username in the box labeled ":user"
 
[![Gif of user placing their GitHub username in the box labeled ":user"][3]][3]

4. Customize the button using the boxes "Large button", "Show count", and "Standard icon":

[![Original Button][4]][4]

[![Large button][5]][5]

[![Show count button][6]][6]

5. Place this code in the `<head>` or before the end of the `<body>` of your code:

`<a class="github-button" href="https://github.com/hubot" aria-label="Follow @hubot on GitHub">Follow @hubot</a>`

6. Place the customized button rendering code in your code.[![Full Gif tutorial][7]][7]


  [1]: https://buttons.github.io/
  [2]: https://i.stack.imgur.com/U7hri.gif
  [3]: https://i.stack.imgur.com/NK6gZ.gif
  [4]: https://i.stack.imgur.com/9dAx4.png
  [5]: https://i.stack.imgur.com/1BJP9.png
  [6]: https://i.stack.imgur.com/Snrtn.png
  [7]: https://i.stack.imgur.com/EfMz2.gif

## All Other Buttons
All of the other buttons you can create, are buttons that link to a GitHub repository and prompts the user to complete a certain action. These buttons can do actions such as:

 - **Watch** a repository
 - **Star** a repository
 - **Fork** a repository
 - **Download** a repository
 - List an **Issue** with a repository

Here's how to create some:

1. Go onto [github:buttons][1]
 2. Click the button type you want to create (Watch, Star, Fork, Download, or Issue)

[![Button click example][2]][2]

3. Place your GitHub username in the box labeled ":user", and your repository in the box ":repo"

[![Repo insert example][3]][3]

4. Customize the button using the boxes "Large button", "Show count", and "Standard icon":

[![Download Button][4]][4]

[![Issue Button][5]][5]

[![Star Button][6]][6]

5. Place this code in the `<head>` or before the end of the `<body>` of your code:

`<a class="github-button" href="https://github.com/hubot" aria-label="Follow @hubot on GitHub">Follow @hubot</a>`

6. Place the customized button rendering code in your code.

[![Full tutorial][7]][7]


  [1]: https://buttons.github.io/
  [2]: https://i.stack.imgur.com/UiZ4J.gif
  [3]: https://i.stack.imgur.com/4uOhR.gif
  [4]: https://i.stack.imgur.com/7Dx6x.png
  [5]: https://i.stack.imgur.com/xFgF4.png
  [6]: https://i.stack.imgur.com/8zUsT.png
  [7]: https://i.stack.imgur.com/EHn5c.gif

