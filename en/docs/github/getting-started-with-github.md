---
title: "Getting started with github"
slug: "getting-started-with-github"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## GitHub Flavored Markdown
GitHub expands [Markdown][1] syntax to provide new useful features.

# Header

```
# Header1
## Header2
### Header3
#### Header4
##### Header5
###### Header6
H1
===
H2
---
```

[![header][2]][2]

# Emphasis

```
*Italic1* _Italic2_
**Bold1** __Bold2__
***Bold_Italic***
~~Strikethrough~~
```

[![Emphasis][3]][3]

# Horizontal Line

```
---
***
___
```

[![horizontal line][4]][4]

# List

```
unordered list:

* item-1
  * sub-item-1
  * sub-item-2
- item-2
  - sub-item-3
  - sub-item-4
+ item-3
  + sub-item-5
  + sub-item-6


ordered list:

1. item-1
  1. sub-item-1
  2. sub-item-2
2. item-2
  1. sub-item-3
  2. sub-item-4
3. item-3
```

[![list][5]][5]

# Table

```
Table Header-1 | Table Header-2 | Table Header-3
:--- | :---: | ---:
Table Data-1 | Table Data-2 | Table Data-3
TD-4 | Td-5 | TD-6
Table Data-7 | Table Data-8 | Table Data-9
```

[![table][6]][6]

# Code

    inline code- `int i=0`
    
    block code-
    ``` C
    for(int i=0; i<10; i++){
        printf("Hallow World! \n");
    }
    ```

[![code][7]][7]

# Quote

```
> Stay hungry; stay foolish.
>> Quality is better than quantity.
>>> Life is not fair; get used to it.
```

[![quote][8]][8]

# Link

```
https://github.com    
[GitHub](https://github.com)    
[GitHub](https://github.com "github website")    
[GitHub][1]    

[1]: https://github.com
```

[![Link][9]][9]

# Image

```
![GitHub Logo](https://assets-cdn.github.com/images/icons/emoji/octocat.png "GitHub")
```

[![image][10]][10]

# Task Lists

```
- [x] completed item
- [ ] incomplete item
```

[![task list][11]][11]

# Emoji

```
:octocat: :+1: :book: :ghost: :bulb: :imp:
```

<img src="https://assets-cdn.github.com/images/icons/emoji/octocat.png" width="20" height="20"/> 
<img src="https://assets-cdn.github.com/images/icons/emoji/unicode/1f44d.png" width="20" height="20"/>
<img src="https://assets-cdn.github.com/images/icons/emoji/unicode/1f4d6.png" width="20" height="20"/>
<img src="https://assets-cdn.github.com/images/icons/emoji/unicode/1f47b.png" width="20" height="20"/>
<img src="https://assets-cdn.github.com/images/icons/emoji/unicode/1f4a1.png" width="20" height="20"/>
<img src="https://assets-cdn.github.com/images/icons/emoji/unicode/1f47f.png" width="20" height="20"/>

For all GitHub emojies visit- [Emoji Cheat Sheet][12].

# SHA references

Any reference to a SHA1 hash of a commit will be converted into a link to the commit itself on GitHub:

```
e7909ea4fbb162db3f7f543d43c30684a3fb745f
```

[![sha1][13]][13]

# Pull Request and Issue References

Any reference to a pull request or an issue will automatically be linked to that pull request or issue.

This can be done by putting a `#` in front of the issue/Pull Request number.


  [1]: https://www.wikiod.com/markdown/text-formatting
  [2]: http://i.stack.imgur.com/cqpyj.png
  [3]: http://i.stack.imgur.com/BPzpc.png
  [4]: http://i.stack.imgur.com/fbLlM.png
  [5]: http://i.stack.imgur.com/LbesJ.png
  [6]: http://i.stack.imgur.com/AQIlG.png
  [7]: http://i.stack.imgur.com/kWuYH.png
  [8]: http://i.stack.imgur.com/WVRKq.png
  [9]: http://i.stack.imgur.com/Z18Ro.png
  [10]: http://i.stack.imgur.com/rDfU0.png
  [11]: http://i.stack.imgur.com/ve7lw.png
  [12]: http://www.emoji-cheat-sheet.com/
  [13]: http://i.stack.imgur.com/pNfI8.png

## Installation or Setup
GitHub is a huge collection of Git repositories. In other words, you can think of GitHub as a collection of many projects!

# Creating An Account

- Visit GitHub's main page [Here](https://github.com/)
- Pick a username, enter in your email address, and pick a secure password and you're ready to go!

# Useful Tools

For Git/GitHub beginners, understanding how version control works might be confusing at first. There exists a GUI version of GitHub that you can download and use. [GitHub Desktop](https://desktop.github.com/) is just that tool.

# Creating Your First Repository #

You can think of a repository as a project. You can create a repository online or offline. Follow the steps below:

## Online

1. First log in and go to your profile. 
2. Navigate to the "Repositories" tab near the top of the page
3. Hit the green "New" button and you're ready to rumble!

## Offline

1. Download and install [git][1] (choose the operating system you are running)
2. After downloading and installation, you can either use the command line tool, or you can download a GUI client.
3. After installation, create an account on [github][2]
4. From the top right, click on the + and choose either creating a new repository or import an existing on.
5. If you choose a new one, enter the repository name and choose either to have it public or private. 
6. Click: Create Repository

N.B. Private repositories are not available for free users.


  [1]: https://git-scm.com/downloads
  [2]: https://github.com/

## README file
If your project doesn't have README.md, GitHub may parse README.rdoc to display details. If it has both, it will use README.md, silently ignoring rdoc.

---

# A README file may include-
<br/>

## Project Title

Describe briefly about your project. You may also provide project's website link, badges, community & contact info (i.e. 
email, social site).

## Download

Runnable file (executable or minified or installation file) link. There can be links to previous versions too.

## Installation

How your work can be used. It may include the prerequisites, settings, third party libraries, usage, cautions, etc.

## Demonstration

It may include code sample, gif file, video link, or even screen shots.

## Authors

Author names, contact info, etc.

## Acknowledgments

List of people or community helped and inspired throughout the project

## Contributing

Instructions to contribute (i.e. add feature, report bug, submit patch) to the project. May include documentation link too.

## License

Give a short intro over your license. You can give a link to the license site too.




## LICENSE file
GitHub helps you quickly add a license to your repository, as an alternative for adding your own text/markdown file. 

1. In your repository, click 'Create new file'

   [![step 1][1]][1]

 2. On next page:
    1. Type `LICENSE.md` or `LICENSE.txt` as the new file's file name. 
    2. The _Want to use a new template?_ dialog will appear.  

    [![step 2][2]][2]

3. Choose your preferred license. 

    [![step 3][3]][3]

4. The licence you could see in the repository details:

    [![step 4][4]][4]

*From [Q&A - How to add license to a existing Github project][5]*


  [1]: http://i.stack.imgur.com/fULI2.png
  [2]: http://i.stack.imgur.com/5pTQt.png
  [3]: http://i.stack.imgur.com/sKiKg.png
  [4]: https://i.stack.imgur.com/ZB89m.png
  [5]: http://stackoverflow.com/a/37895659/3193542

