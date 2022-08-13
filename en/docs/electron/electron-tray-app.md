---
title: "Electron-tray-app"
slug: "electron-tray-app"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Electron Tray App
Adding a icon to your tray-bar

    let tray = null;
    let mainWindow = null;
    let user = null;
    
    app.on('ready', () => {
        /**
         * Tray related code.
         */
        const iconName = 'icon.png';
        const iconPath = path.join(__dirname, iconName);
        tray = new Tray(iconPath);
        tray.setToolTip('AMP Notifier App');
        const contextMenu = Menu.buildFromTemplate([{
            label: 'Quit',
            click: destroyApp
        }]);
        tray.setContextMenu(contextMenu);

        tray.on('click', () => {
            app.quit();
        });
    });


