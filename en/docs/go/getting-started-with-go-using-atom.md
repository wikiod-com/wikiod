---
title: "Getting Started With Go Using Atom"
slug: "getting-started-with-go-using-atom"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

After installing go (https://www.wikiod.com/go/getting-started-with-go ) you'll need an environment. An efficient and free way to get you started is using Atom text editor (https://atom.io ) and gulp.
A question that maybe crossed your mind is *why use gulp?*.We need gulp for auto-completion.
Let's get Started!

## Create $GO_PATH/gulpfile.js
    var gulp = require('gulp');
    var path = require('path');
    var shell = require('gulp-shell');
    
    var goPath = 'src/mypackage/**/*.go';
    
    
    gulp.task('compilepkg', function() {
      return gulp.src(goPath, {read: false})
        .pipe(shell(['go install <%= stripPath(file.path) %>'],
          {
              templateData: {
                stripPath: function(filePath) {
                  var subPath = filePath.substring(process.cwd().length + 5);
                  var pkg = subPath.substring(0, subPath.lastIndexOf(path.sep));
                  return pkg;
                }
              }
          })
        );
    });
    
    gulp.task('watch', function() {
      gulp.watch(goPath, ['compilepkg']);
    });
    
In the code above we defined a *compliepkg* task that will be triggered every time any go file in goPath (src/mypackage/) or subdirectories changes. the task will run the shell command go install changed_file.go

After creating the gulp file in go path and define the task open a command line and run:

> gulp watch

You'll se something like this everytime any file changes:
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/qwEvS.png

## Create $GO_PATH/mypackage/source.go
    package mypackage
    
    var PublicVar string = "Hello, dear reader!"
    
    //Calculates the factorial of given number recursively!
    func Factorial(x uint) uint {
        if x == 0 {
            return 1
        }
        return x * Factorial(x-1)
    }
    

## Creating $GO_PATH/main.go
Now you can start writing your own go code with auto-completion using Atom and Gulp:
[![package-img][1]][1]
[![in-package-img][2]][2]

    package main

    import (
        "fmt"
        "mypackage"
    )
    
    func main() {
    
        println("4! = ", mypackage.Factorial(4))
    
    }

[![app-output][4]][4]


  [1]: https://i.stack.imgur.com/olQXd.png
  [2]: https://i.stack.imgur.com/0SuI3.png
  [3]: https://i.stack.imgur.com/wCVO8.png
  [4]: https://i.stack.imgur.com/H0geN.png

## Get, Install And Setup Atom & Gulp

1. Install Atom. You can get atom from [here][1]
2. Go to Atom settings (ctrl+,). Packages -> Install go-plus package ([go-plus][2])

After Installing go-plus in Atom:
[![atom-setting-img][3]][3]

3. Get these dependencies using go get or another dependency manager: (open a console and run these commands)

> go get -u golang.org/x/tools/cmd/goimports

> go get -u golang.org/x/tools/cmd/gorename 

> go get -u github.com/sqs/goreturns 

> go get -u github.com/nsf/gocode

> go get -u github.com/alecthomas/gometalinter

> go get -u github.com/zmb3/gogetdoc

> go get -u github.com/rogpeppe/godef

> go get -u golang.org/x/tools/cmd/guru

4. Install Gulp ([Gulpjs][4]) using npm or any other package manager ([gulp-getting-started-doc][5]):

> $ npm install --global gulp


  [1]: https://github.com/atom/atom/releases/tag/v1.12.7 "here"
  [2]: https://atom.io/packages/go-plus
  [3]: https://i.stack.imgur.com/HSbug.png
  [4]: http://gulpjs.com/
  [5]: https://github.com/gulpjs/gulp/blob/master/docs/getting-started.md

