---
title: "QMessageBox"
slug: "qmessagebox"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

`QMessageBox` is the simplest way to give (or ask) an information to (or from) the user. It's a modal dialog, inheriting the `QDialog` class. It also has four convenience static functions: `information`, `question`, `warning` and `critical`.

## Basic usage: Hello World
    app = QApplication( sys.argv )
    box = QMessageBox()

    # Window Title
    box.setWindowTitle( "Hello World." )

    # Icon: Information, Warning, Question, Critical
    box.setIcon( QMessageBox.Information )

    # Short version of the information
    box.setText( "Hello World!" )

    # Informative text
    box.setInformativeText( "Hello World! We are using Qt to display this beautiful dialog to you." )

    # Show the messagebox as a modal dialog
    box.exec_()
    
    return 0

