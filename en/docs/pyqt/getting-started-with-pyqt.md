---
title: "Getting started with pyqt"
slug: "getting-started-with-pyqt"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## A basic application
The following example shows a basic main GUI window with a label widget, a toolbar, and a status bar using PyQt4.

[![][1]][1]

<!-- language: lang-python -->

    import sys
    from PyQt4 import QtGui
    

    class App(QtGui.QApplication):
        def __init__(self, sys_argv):
            super(App, self).__init__(sys_argv)
            self.build_ui()
    
        def build_ui(self):
            # build a main GUI window
            self.main_window = QtGui.QMainWindow()
            self.main_window.setWindowTitle('App')
            self.main_window.show()
    
            # add a label to the main window
            label = QtGui.QLabel('Label')
            self.main_window.setCentralWidget(label)
    
            # add a toolbar with an action button to the main window
            action = QtGui.QAction('Toolbar action', self)
            toolbar = QtGui.QToolBar()
            toolbar.addAction(action)
            self.main_window.addToolBar(toolbar)
    
            # add a status bar to the main window
            status_bar = QtGui.QStatusBar()
            status_bar.showMessage('Status bar')
            self.main_window.setStatusBar(status_bar)
    
    
    if __name__ == '__main__':
        app = App(sys.argv)
        sys.exit(app.exec_())


  [1]: http://i.stack.imgur.com/eFdBI.png

## Hello world
This basic code will launch a "Hello world" GUI window using PyQt4:

<!-- language: lang-python -->

    import sys
    from PyQt4 import QtGui
    
    # create instance of QApplication
    app = QtGui.QApplication(sys.argv)

    # create QLabel, without parent it will be shown as window
    label = QtGui.QLabel('Hello world!')
    label.show()

    # start the execution loop of the application
    sys.exit(app.exec_())

This is the same code using PyQt5.

<!-- language: lang-python -->

    import sys
    from PyQt5 import QtWidgets
    
    # create instance of QApplication    
    app = QtWidgets.QApplication(sys.argv)

    # create QLabel, without parent it will be shown as window
    label = QtWidgets.QLabel('Hello world!')
    label.show()

    # start the execution loop of the application
    sys.exit(app.exec_())

## A Simple Drag & Drop Sample
Make a simple GUI application in 3 easy steps.

**1. Design**

Open ```Qt Creator```, create a new project and make your design. Save your result as ```.ui``` file (here: ```mainwindow.ui```). 

[![An example of a widget][1]][1]

**2. Generate corresponding .py file**

Now you can create a .py file from your .ui file that you generated in the previous step. Enter the following into your command line:
```
$ pyuic4 mainwindow.ui -o GUI.py
```

If the above line is run successfully a ```GUI.py``` file is created.

**3. Python codes**

You can add your own code (e.g. signals and slots) in the ```GUI.py``` file but it's better to add them in a new file. If you ever want to make changes to your GUI, the ```GUI.py``` file will be overwritten. That's why using another file to add functionality is better in most cases.

Let's call the new file ```main.py```.

```
from PyQt4 import QtGui
import sys
import GUI # Your generated .py file


class MyApp(QtGui.QMainWindow, GUI.Ui_MainWindow):
    def __init__(self, parent=None):
        super(ExampleApp, self).__init__(parent)
        self.setupUi(self)

        # Connect a button to a function
        self.btn_run.clicked.connect(self.run)

    def run(self):
        # Write here what happens after the button press
        print("run")


if __name__ == '__main__':
    app = QtGui.QApplication(sys.argv)
    form = ExampleApp()
    form.show()
    app.exec_()
```

Now you can run ```main.py``` and see your GUI.

[![Output][2]][2]


  [1]: http://i.stack.imgur.com/NrGot.png
  [2]: http://i.stack.imgur.com/ju0fm.png

## Installation of PyQt4
**Suggested Install Method**

*Windows*: Download and run the [binary setup file][1].

*Linux(Debian)*: Run this command in your command line:

    $ apt-get install python-qt4 pyqt4-dev-tools qt4-designer

*OS X* : Run this command in your command line:

    $ brew install pyqt

**Install Manually**

You can also download the source code manually from [here][1] and then install and configure it yourself.

**Test your installation**

If pyqt is installed correctly, you will be able to run the ```pyuic4``` command. If it is installed correctly, you will see the following error:

```
$ pyuic4 
Error: one input ui-file must be specified
```

**Installation Complete**

You have now installed the PyQt4 library. Two useful applications have also been installed along side PyQt4:

 - Qt Designer: An application for 'drag & drop' design of graphical interfaces (creates `.ui` files),
 - pyuic4: A command line application that can convert `.ui` files into Python code.

  [1]: https://riverbankcomputing.com/software/pyqt/download

