---
title: "Creating PyQt GUI With Maya"
slug: "creating-pyqt-gui-with-maya"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Creating PyQt Window
This is a very basic example how to load a pyqt ui file to maya with pyqt libs. In this solution you really don't need to convert your pyqt ui file a python file. You can simply load your pyqt ui.

    from PyQt4 import QtCore, QtGui, uic
    import maya.OpenMayaUI as mui
    import sip
    
    baseUI                    = "/user/foo/bar/basic.ui"
    baseUIClass, baseUIWidget = uic.loadUiType(baseUI)
    
    class Ui_MainWindow(baseUIWidget, baseUIClass):
        def __init__(self,parent=None):
            super(baseUIWidget, self).__init__(parent)
            self.setupUi(self)
    
    
    def getMayaWindow():
        ptr = mui.MQtUtil.mainWindow()
        return sip.wrapinstance(long(ptr), QtCore.QObject)
    
    def mayaMain():
        global maya_basicTest_window
        try:
            maya_basicTest_window.close()
        except:
            pass
        maya_basicTest_window = Ui_MainWindow(getMayaWindow())
        maya_basicTest_window.show()
    
    mayaMain()

## Creating a PyQt Window By Code
In this example, we trying to create a gui with only through code rather than using any ui file. Its a very basic example you need to extend based on your need."


    from PyQt4 import QtCore, QtGui
    import maya.OpenMayaUI as mui
    import sip
    
    class Ui_MainWindow(QtGui.QMainWindow):
        def __init__(self,parent=None):
            QtGui.QMainWindow.__init__(self, parent)
    
            self.centralwidget = QtGui.QWidget(self)
            self.pushButton = QtGui.QPushButton(self.centralwidget)
            self.pushButton.setGeometry(QtCore.QRect(80, 50, 75, 23))
            self.pushButton_2 = QtGui.QPushButton(self.centralwidget)
            self.pushButton_2.setGeometry(QtCore.QRect(190, 50, 111, 151))
            self.pushButton_3 = QtGui.QPushButton(self.centralwidget)
            self.pushButton_3.setGeometry(QtCore.QRect(350, 60, 75, 101))
            self.setCentralWidget(self.centralwidget)
            self.menubar = QtGui.QMenuBar(self)
            self.menubar.setGeometry(QtCore.QRect(0, 0, 800, 21))
            self.setMenuBar(self.menubar)
            self.statusbar = QtGui.QStatusBar(self)
            self.setStatusBar(self.statusbar)
            self.retranslateUi()
    
        def retranslateUi(self):
            self.setWindowTitle("MainWindow")
            self.pushButton.setText("test")
            self.pushButton_2.setText( "test")
            self.pushButton_3.setText("test")
    
    def getMayaWindow():
        ptr = mui.MQtUtil.mainWindow()
        return sip.wrapinstance(long(ptr), QtCore.QObject)
    
    def mayaMain():
        global maya_basicTest_window
        try:
            maya_basicTest_window.close()
        except:
            pass
        maya_basicTest_window = Ui_MainWindow(getMayaWindow())
        maya_basicTest_window.show()
    
    mayaMain()

