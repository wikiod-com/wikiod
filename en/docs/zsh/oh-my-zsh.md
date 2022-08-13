---
title: "oh-my-zsh"
slug: "oh-my-zsh"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

[Oh-my-zsh](http://ohmyz.sh/) is one of [many configuration frameworks](https://github.com/unixorn/awesome-zsh-plugins) for interactive use of zsh (it is not relevant for writing zsh scripts). It can be useful if you'd like to start with an experienced user's configuration.

## Installation
Oh-my-zsh is a community-driven framework for managing your zsh configuration. It contains many plugins, which extend functionality, themes, which manage the appearance of the shell and the prompt, and helpful functions that will make your terminal much more powerful and customizable. You can create your own plugins and themes if you find there's something missing.

You can only install oh-my-zsh after you have installed zsh. To do so, run the following command:
## via curl ##

```
$ sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
```

## via wget ##
```
$ sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
```


Once that's done, zsh will be your default shell (it will ask for your password to do this). Feel free to edit your `~/.zshrc` configuration to setup plugins and themes. Remember to run `$ source ~/.zshrc` for changes to take effect, or log out and log back in.

