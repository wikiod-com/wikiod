---
title: "Packaging an electron app"
slug: "packaging-an-electron-app"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

When ready for distribution, your electron app can be packaged into an executable file.

Electron applications can be packaged to run on Windows (32/64 bit), OSX (macOS) and Linux (x86/x86_64).

To package your code, use the npm package 'electron-packager\

https://github.com/electron-userland/electron-packager

## Syntax
- $ electron-packager 
- sourcedir
- appname 
- --platform=platform
- --arch=arch
- [optional flags...]

## Parameters

| Parameter | Details |
| ------ | ------ |
| sourcedir   | The directory of your electron application files |
| appname | The name of your application |
| platform | The platform you want to compile your code for. Omitting this will compile for the host OS
| arch | The system architecture you want to compile your code for. Omitting this will compile for the host arch

## Installing electron-packager
    # for use in npm scripts
    npm install electron-packager --save-dev

    # for use from cli
    npm install electron-packager -g

## Packaging from script
    var packager = require('electron-packager');

    packager({
        dir: '/',
    }, function(err, path){
        if(err) throw err;
        // Application has been packaged
    });

## Making npm scripts to automate Electron packaging
A convenient way to package your application is to write the scripts in your `packages.json` file and run them with the `npm run` command

    {
        "name": "AppName",
        "productName": "AppName",
        "version": "0.1.1",
        "main": "main.js",
        "devDependencies": {
            "electron": "^1.6.6",
            "electron-packager": "^8.7.0"
        },
        "scripts": {
            "package-mac": "electron-packager . --overwrite --platform=darwin --arch=x64 --icon=images/icon.png --prune=true --out=release-builds",
            "package-win": "electron-packager . --overwrite --platform=win32 --arch=ia32 --icon=images/icon.png --prune=true --out=release-builds",
            "package-linux" : "electron-packager . --overwrite --platform=linux --arch=x64 --icon=images/icon.png --prune=true --out=release-builds"
        }
    }

And to run them you just write:

    npm run package-mac
    npm run package-win
    npm run package-linux

A breakdown of the command flags is:
    
    electron-packager .     // this runs the packager in the current folder
    --overwrite             // overwrite any previous build
    --platform=darwin       // platform for which the binaries should be created
    --arch=x64              // the OS architecture
    --icon=images/icon.png  // the icon for the app executable
    --prune=true            // this does not copy your dev-dependencies that appear in your packages.json
    --out=release-builds    // the name of the folder were the binaries will be outputed

Before, running the scripts change the devDependencies to dependencies as electron-packager cannot bundle the packages in the devDependencies into the app. In packager.json, change the word (if it's there or if packages are installed using --save-dev in npm install) devDependencies to only dependencies.    

## Packaging from CLI
    electron-packager C:/my-app MyApp

