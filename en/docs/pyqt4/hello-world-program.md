---
title: "Hello World Program"
slug: "hello-world-program"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

**You need to know basics of Python Programming Language before you start with PyQt.**

PyQt is a GUI widgets toolkit. It is a Python interface for Qt, one of the most powerful, and popular cross-platform GUI library. PyQt is a blend of Python programming language and the Qt library.

Here is an Hello World Program to get you started.

Creating a simple GUI application using PyQt involves the following steps −

1. Import QtGui module

2. Create an application object.

3. A QWidget object creates top level window. Add QLabel object in it.

4. Set the caption of label as “hello world”.

5. Define the size and position of window by setGeometry() method.

6. Enter the mainloop of application by app.exec_() method.

## Hello World Program
    import sys
    from PyQt4 import QtGui
    
    def window():
       app = QtGui.QApplication(sys.argv)
       w = QtGui.QWidget()
       b = QtGui.QLabel(w)
       b.setText("Hello World!")
       w.setGeometry(100,100,200,50)
       b.move(50,20)
       w.setWindowTitle("PyQt")
       w.show()
       sys.exit(app.exec_())
        
    if __name__ == '__main__':
       window()

