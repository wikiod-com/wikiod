---
title: "Signals and Slots"
slug: "signals-and-slots"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Functions or methods are executed in response to user’s actions like `clicking` on a button, selecting an item from a collection or a mouse click etc., called **`events`**.

Each PyQt widget, which is derived from `QObject` class, is designed to emit **signal** in response to one or more events. The signal on its own does not perform any action. Instead, it is **connected** to a **slot**.

In the following example, two `QPushButton` objects (`b1` and `b2`) are added in `QDialog` window. We want to call functions `b1_clicked()` and `b2_clicked()` on clicking `b1` and `b2` respectively.

When `b1` is clicked, the `clicked()` signal is connected to `b1_clicked()` function

    b1.clicked.connect(b1_clicked())

When `b2` is clicked, the `clicked()` signal is connected to `b2_clicked()` function

    QObject.connect(b2, SIGNAL("clicked()"), b2_clicked)

Widgets used to build the GUI interface act as the source of such events. 

Each PyQt widget, which is derived from `QObject` class, is designed to emit **signal** in response to one or more events. The signal on its own does not perform any action. Instead, it is **connected** to a **slot**. The slot can be any **callable Python function**.

## An Example Using Signals and Slots
    import sys
    from PyQt4.QtCore import *
    from PyQt4.QtGui import *
    
    def window():
       app = QApplication(sys.argv)
       win = QDialog()
       b1 = QPushButton(win)
       b1.setText("Button1")
       b1.move(50,20)
       b1.clicked.connect(b1_clicked)
    
       b2 = QPushButton(win)
       b2.setText("Button2")
       b2.move(50,50)
       QObject.connect(b2,SIGNAL("clicked()"),b2_clicked)
    
       win.setGeometry(100,100,200,100)
       win.setWindowTitle("PyQt")
       win.show()
       sys.exit(app.exec_())
    
    def b1_clicked():
       print ("Button 1 clicked")
    
    def b2_clicked():
       print ("Button 2 clicked")
    
    if __name__ == '__main__':
       window()


The following example will produce two buttons, if you click them, then function `b1_clicked` or `b2_clicked` will be called.

