---
title: "Qt Resource System"
slug: "qt-resource-system"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

The Qt Resource system is a way to embed files within your project. Each resource file can have one or more *prefixes* and each *prefix* can have files in it.

Each file in the resources is a link to a file on the file system. When the executable is built, the files are bundled into the executable, so the original file does not need to be distributed with the binary.

## Referencing files within code
Let's say that inside a resources file, you had a file called /icons/ok.png

The full url of this file within code is ```qrc:/icons/ok.png```. In most cases, this can be shortened to ```:/icons/ok.png```

For example, if you wanted to create a QIcon and set it as the icon of a button from that file, you could use

    QIcon icon(":/icons/ok.png"); //Alternatively use qrc:/icons/ok.png
    ui->pushButton->setIcon(icon);

