---
title: "Code Styling in PhpStorm"
slug: "code-styling-in-phpstorm"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Define the code style for a project
PhpStorm offers default settings for code styling for a large amount of languages based on best practices and common standards.
But you can customize the styling for each language on a per-project base within the PhpStorm Settings > *Editor* > *Code Style*.

[![Code Style Settings][1]][1]

**Schemes**

Schemes are collections of code style guidelines and settings. You can select a scheme for a project and it will be applied instantly. There also is a Project scheme that is only available while you have a project open. The Project scheme does not save the guidelines into general user settings but in the projects own settings.

If you click the *Manage* button you are able to add a new scheme to quickly set up a new set of code style guidelines. The *Manage* box can also be used to export or import schemes which is quite helpful if you want to share schemes with your friends or colleagues.

[![Code Style Manage Box][2]][2]

# Change the style for a specific language

To change the code style settings for a specific language, simply click on the available language in the sidebar. You will then be presented a settings page that is different for each language.

For example the PHP language will have settings for tabs and spaces, braces or PHP Docs.

[![Code Style for PHP][3]][3]

If you want to know what each different setting means you can look them up in the official documentation.

[Code Style Documentation for PhpStorm 2016 and up](https://www.jetbrains.com/help/phpstorm/2016.1/code-style.html)


  [1]: http://i.stack.imgur.com/QBYmC.png
  [2]: http://i.stack.imgur.com/SI9ij.png
  [3]: http://i.stack.imgur.com/xbOnd.png

## Set Code Styles from a predefined Standard like PSR-2
PhpStorm already ships with a lot of predefined language schemes that are based on common code style guidelines and standards like [PSR-2][1].
There is kind of a hidden feature in the code style settings pages where you can import these standards and set them as your current configuration. To do so simply choose your coding language in the left panel. Then there is a small link in the top right called *Set from...*

By clicking this link PhpStorm will present you a small popup where you can choose from the predefined standards under the *Predefined Style* tab.

[![PhpStorm set style from][2]][2]


  [1]: https://github.com/php-fig/fig-standards/blob/master/accepted/PSR-2-coding-style-guide.md
  [2]: http://i.stack.imgur.com/RDval.png

## Enforce a specified code style for a project across multiple team members
Currently there is no one-click-button method to actually enforce any code style guidelines across a team but there are two methods to make sure a certain code style is applied to your product.

# Import PhpStorm Code Style Schemes

The first and more easier solution is to set up a code style scheme on your own PhpStorm instance, export the scheme to a portable drive or network drive and import the scheme on all development machines.  
This way a developer can easily use the keyboard shortcuts <kbd>Cmd</kbd>+<kbd>Shift</kbd>+<kbd>L</kbd> (MacOS) or <kbd>Ctl</kbd>+<kbd>Alt</kbd>+<kbd>L</kbd> (Windows / Linux) to automatically format the complete source code.

You can find more detailed information about this in the following documentation:

[Reformatting Source Code in PhpStorm 2016 and up](https://www.jetbrains.com/help/phpstorm/2016.1/reformatting-source-code.html)

**Cons**  
Unfortunately there is no way to check if a developer really applied the code reformatting. You would have to rely on the promises by the developers that they take care of the reformatting.

# Automated Code Format Checks with a CI Server

A very strict way to control source code formatting is to implement a continuos integration server like [Jenkins](https://jenkins.io/) that is able to check if the source code matches a predefined code style.

Let's assume a developer worked on a new feature on his own development branch and wants to push his changes to the main repository. First he pushes the changes to his own branch where the new feature will be checked by the CI server. If the check failed because the code is not formatted properly the developer will be notified so he will be able to correct the issues.

There are various ways on how to set up code quality and formatting checks with all the different integration servers so explaining how to set up a server with checks should be done in the corresponding tags.

