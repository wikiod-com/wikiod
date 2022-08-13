---
title: "Main and renderer process."
slug: "main-and-renderer-process"
draft: false
images: []
weight: 9930
type: docs
toc: true
---


Process that runs `package.json`’s main script is called the **main process**. The main process creates web pages by creating `BrowserWindow` instances. Each web page in Electron runs in its own process, which is called the **renderer process**. The main process manages all web pages and their corresponding renderer processes. Each renderer process is isolated and only cares about the web page running in it.

## Asynchronous IPC communication
Main process source code `index.js`:

<!-- language: lang-js -->    
    const {app, BrowserWindow, ipcMain} = require('electron')
    let win = null

    app.on('ready', () => {
      win = new BrowserWindow()
      win.loadURL(`file://${__dirname}/index.html`)
      win.webContents.openDevTools()
      win.on('closed', () => {
        win = null
      })
      win.webContents.on('did-finish-load', () => {
        win.webContents.send('asyncChannelToRenderer', 'hello')
      })
    })

    ipcMain.on('asyncChannelToMain', (event, arg) => {
      console.log(arg + ' from renderer')
      if (arg === 'hello') {
        event.sender.send('asyncChannelToRenderer', 'world')
      }
    })

Renderer process in `index.html`:
<!-- language: lang-html -->  
    <!DOCTYPE html>
    <html>
      <head>
        <title>Hello World IPC</title>
        <script>
          require('electron').ipcRenderer.on('asyncChannelToRenderer', (event, arg) => {
            console.log(arg + ' from main')
            if (arg === 'hello') {
              event.sender.send('asyncChannelToMain', 'world')
            }
          })
        </script>
      </head>
      <body>
        <button onclick="require('electron').ipcRenderer.send('asyncChannelToMain', 'hello')">click me</button>
      </body>
    </html>


## Remote module RMI
The `remote` module allows simple RMI (remote method invocation) of main process objects from renderer process. First create the main process in `index.js`

<!-- language: lang-js -->
    const {app, BrowserWindow} = require('electron')
    let win = null

    app.on('ready', () => {
      win = new BrowserWindow()
      win.loadURL(`file://${__dirname}/index.html`)
      win.on('closed', () => {
        win = null
      })
    })

and then remote process `index.html`

<!-- language: lang-html -->
    <!DOCTYPE html>
    <html>
      <head>
        <script>
          const {BrowserWindow, app} = require('electron').remote
        </script>
      </head>
      <body>
        <button onclick= "let win = new BrowserWindow(); win.loadURL(`file://${__dirname}/index.html`)">new window</button>
        <button onclick= "app.quit()">quit</button>
      </body>
    </html>

## Synchronous IPC communication
Create `index.js` as

<!-- language: lang-js -->    
    const {app, BrowserWindow, ipcMain} = require('electron')
    let win = null

    app.on('ready', () => {
      win = new BrowserWindow()
      win.loadURL(`file://${__dirname}/index.html`)
      win.webContents.openDevTools()
      win.on('closed', () => {
        win = null
      })
    })

    ipcMain.on('syncChannelToMain', (event, arg) => {
      console.log(arg + ' from renderer')
      event.returnValue = 'world'
    })

and renderer process `index.html` as
<!-- language: lang-html -->  
    <!DOCTYPE html>
    <html>
      <head>
        <title>Hello World IPC</title>
      </head>
      <body>
        <button onclick="console.log(require('electron').ipcRenderer.sendSync('syncChannelToMain', 'world') + ' from main')">click me</button>
      </body>
    </html>


