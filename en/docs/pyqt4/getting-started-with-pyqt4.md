---
title: "Getting started with pyqt4"
slug: "getting-started-with-pyqt4"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic Hello World Program
    import sys
    from PyQt4 import QtGui
    
    def window():
       app = QtGui.QApplication(sys.argv)
       w = QtGui.QWidget()
       b = QtGui.QLabel(w)
       b.setText("<h1>Welcome to PyQt4 SO Documentation!</h1>")
       w.setGeometry(100,100,550,65)
       b.move(50,20)
       w.setWindowTitle("PyQt4 Hello World Demo")
       w.show()
       sys.exit(app.exec_())
        
    if __name__ == '__main__':
       window()

## Installation or Setup
Detailed instructions on getting pyqt4 set up or installed.

 1. Windows [Link][1]
 2. Mac [Link][2]
 3. Linux [Link][3]


If you want to install the version specific to your system python version and you system configuration(32-bit or 64-bit) then go to this [link][4] and download and install the package. You can install the `.whl` file simply by going to command prompt `pip install PyQt4‑4.11.4‑cp34‑none‑win_amd64.whl`.


Go a head Install the software and Start Building Awesome GUI!!


  [1]: http://sourceforge.net/projects/pyqt/files/PyQt4/PyQt-4.11.4/PyQt-win-gpl-4.11.4.zip
  [2]: http://sourceforge.net/projects/pyqt/files/PyQt4/PyQt-4.11.4/PyQt-mac-gpl-4.11.4.tar.gz
  [3]: http://sourceforge.net/projects/pyqt/files/PyQt4/PyQt-4.11.4/PyQt-x11-gpl-4.11.4.tar.gz
  [4]: http://www.lfd.uci.edu/~gohlke/pythonlibs/#pyqt4

