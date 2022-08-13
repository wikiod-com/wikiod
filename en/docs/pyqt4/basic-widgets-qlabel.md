---
title: "Basic Widgets  QLabel"
slug: "basic-widgets--qlabel"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

`QLabel` is used for displaying text or an image. No user interaction functionality is provided. The visual appearance of the label can be configured in various ways, and it can be used for specifying a focus mnemonic key for another widget.


A `QLabel` object acts as a placeholder to display <i>non-editable</i> **text**,**image**, or a **animated GIF**. It can also be used as a mnemonic key for other widgets. 


In this example, `QLabel` objects `l2` and `l4` have the caption containing hyperlink. `setOpenExternalLinks` for `l2` is set to **true**. Hence, if this label is **clicked**, the associated URL will open in the browser. `linkHovered` <i>signal</i> of `l4` is connected to `hovered()` function. So, whenever the mouse hovers over it, the function will be executed.

QPixmap object prepares offscreen image from <i>python.jpg</i> file. It is displayed as label `l3` by using `setPixmap()` method


**Plain text**, **hyperlink** or **rich text** can also be displayed in `QLabel`.


In `QLabel` most of the basic HTML tags are allowed. For Example : `h1`,`h2`,`h3`,`font`,`span`,etc

[You can visit here for the list of supported HTML subset.][1]


  [1]: http://doc.qt.io/qt-4.8/richtext-html-subset.html

## QLabel example with text, hyperlink and image
Following is the example of `QLabel` that displays use of texts,images and hyperlinks.


      
    import sys
    from PyQt4.QtCore import *
    from PyQt4.QtGui import *
    
    def window():
       app = QApplication(sys.argv)
       win = QWidget() 
        
       l1 = QLabel()
       l2 = QLabel()
       l3 = QLabel()
       l4 = QLabel()
        
       l1.setText("<h1>Hello World</h1>")
       l4.setText("<b>Hello Stack OverFlow</b>")
       l2.setText("<font color='red'>Welcome To Stack Overflow Documentation</font>")
        
       l1.setAlignment(Qt.AlignCenter)
       l3.setAlignment(Qt.AlignCenter)
       l4.setAlignment(Qt.AlignRight)
       l3.setPixmap(QPixmap("python.jpg"))
        
       vbox = QVBoxLayout()
       vbox.addWidget(l1)
       vbox.addStretch()
       vbox.addWidget(l2)
       vbox.addStretch()
       vbox.addWidget(l3)
       vbox.addStretch()
       vbox.addWidget(l4)
        
       l1.setOpenExternalLinks(True)
       l4.linkActivated.connect(clicked)
       l2.linkHovered.connect(hovered)
       l1.setTextInteractionFlags(Qt.TextSelectableByMouse)
       win.setLayout(vbox)
        
       win.setWindowTitle("PyQt4 QLabel Demo")
       win.show()
       sys.exit(app.exec_())
        
    def hovered():
       print ("Come'On Click ME")
    def clicked():
       print ("You Clicked Me")
        
    if __name__ == '__main__':
       window()


The code outputs the following result:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/RueL1.png

