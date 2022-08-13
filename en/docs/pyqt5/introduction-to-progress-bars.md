---
title: "Introduction to Progress Bars"
slug: "introduction-to-progress-bars"
draft: false
images: []
weight: 9850
type: docs
toc: true
---

Progress Bars are an integral part of user experience and helps users get an idea on the time left for a given process that runs on the GUI. This topic will go over the basics of implementing a progress bar in your own application.

This topic will touch lightly on QThread and the new signals/slots mechanism. Some basic knowledge of PyQt5 widgets is also expected of readers.

When adding examples only use PyQt5 and Python built-ins to demonstrate functionality. 

**PyQt5 Only**

Experimenting with these examples is the best way to get started learning.

## Basic PyQt Progress Bar
This is a very basic progress bar that only uses what is needed at the bare minimum.

It would be wise to read this whole example to the end.
<!-- language: python-3.5 -->

    import sys
    import time
    
    from PyQt5.QtWidgets import (QApplication, QDialog,
                                 QProgressBar, QPushButton)
    
    TIME_LIMIT = 100
    
    class Actions(QDialog):
        """
        Simple dialog that consists of a Progress Bar and a Button.
        Clicking on the button results in the start of a timer and
        updates the progress bar.
        """
        def __init__(self):
            super().__init__()
            self.initUI()
            
        def initUI(self):
            self.setWindowTitle('Progress Bar')
            self.progress = QProgressBar(self)
            self.progress.setGeometry(0, 0, 300, 25)
            self.progress.setMaximum(100)
            self.button = QPushButton('Start', self)
            self.button.move(0, 30)
            self.show()
    
            self.button.clicked.connect(self.onButtonClick)
    
        def onButtonClick(self):
            count = 0
            while count < TIME_LIMIT:
                count += 1
                time.sleep(1)
                self.progress.setValue(count)
    
    if __name__ == "__main__":
        app = QApplication(sys.argv)
        window = Actions()
        sys.exit(app.exec_())

The progress bar is first imported like so `from PyQt5.QtWidgets import QProgressBar`

Then it is initialized like any other widget in `QtWidgets`

The line `self.progress.setGeometry(0, 0, 300, 25)` method defines the `x,y` positions on the dialog and width and height of the progress bar.

We then move the button using `.move()` by `30px` downwards so that there will be a gap of `5px` between the two widgets.

Here `self.progress.setValue(count)` is used to update the progress. Setting a maximum value using `.setMaximum()` will also automatically calculated the values for you. For example, if the maximum value is set as 50 then since `TIME_LIMIT` is 100 it will hop from 0 to 2 to 4 percent instead of 0 to 1 to 2 every second. You can also set a minimum value using `.setMinimum()` forcing the  progress bar to start from a given value. 

Executing this program will produce a GUI similar to this.

[![Progress Bar Dialog Not Responding][1]][1]

As you can see, the GUI will most definitely freeze and be unresponsive until the counter meets the `TIME_LIMIT` condition. This is because `time.sleep` causes the OS to believe that program has become stuck in an infinite loop.

**QThread**

So how do we overcome this issue ? We can use the threading class that PyQt5 provides.

<!-- language: python-3.5 -->

    import sys
    import time
    
    from PyQt5.QtCore import QThread, pyqtSignal
    from PyQt5.QtWidgets import (QApplication, QDialog,
                                 QProgressBar, QPushButton)
    
    TIME_LIMIT = 100
    
    class External(QThread):
        """
        Runs a counter thread.
        """
        countChanged = pyqtSignal(int)
    
        def run(self):
            count = 0
            while count < TIME_LIMIT:
                count +=1
                time.sleep(1)
                self.countChanged.emit(count)
    
    class Actions(QDialog):
        """
        Simple dialog that consists of a Progress Bar and a Button.
        Clicking on the button results in the start of a timer and
        updates the progress bar.
        """
        def __init__(self):
            super().__init__()
            self.initUI()
            
        def initUI(self):
            self.setWindowTitle('Progress Bar')
            self.progress = QProgressBar(self)
            self.progress.setGeometry(0, 0, 300, 25)
            self.progress.setMaximum(100)
            self.button = QPushButton('Start', self)
            self.button.move(0, 30)
            self.show()
    
            self.button.clicked.connect(self.onButtonClick)
    
        def onButtonClick(self):
            self.calc = External()
            self.calc.countChanged.connect(self.onCountChanged)
            self.calc.start()
    
        def onCountChanged(self, value):
            self.progress.setValue(value)
    
    if __name__ == "__main__":
        app = QApplication(sys.argv)
        window = Actions()
        sys.exit(app.exec_())

Let's break down these modifications.
<!-- language: python-3.5 -->

    from PyQt5.QtCore import QThread, pyqtSignal

This line imports `Qthread` which is a `PyQt5` implementation to divide and run some parts(eg: functions, classes) of a program in the background(also know as multi-threading). These parts are also called threads. All `PyQt5` programs by default have a main thread and the others(worker threads) are used to offload extra time consuming and process intensive tasks into the background while still keeping the main program functioning.

The second import `pyqtSignal` is used to send data(signals) between worker and main threads. In this instance we will be using it to tell the main thread to update the progress bar.


Now we have moved the while loop for the counter into a separate class called `External`.
<!-- language: python-3.5 -->

    class External(QThread):
        """
        Runs a counter thread.
        """
        countChanged = pyqtSignal(int)
    
        def run(self):
            count = 0
            while count < TIME_LIMIT:
                count +=1
                time.sleep(1)
                self.countChanged.emit(count)

By sub-classing `QThread` we are essentially converting `External` into a class that can be run in a separate thread. Threads can also be started or stopped at any time adding to it's benefits.

Here `countChanged` is the current progress and `pyqtSignal(int)` tells the worker thread that signal being sent is of type `int`. While, `self.countChanged.emit(count)` simply sends the signal to any connections in the main thread(normally it can used to  communicate with other worker threads as well).
<!-- language: python-3.5 -->

    def onButtonClick(self):
            self.calc = External()
            self.calc.countChanged.connect(self.onCountChanged)
            self.calc.start()
    
    def onCountChanged(self, value):
        self.progress.setValue(value)

When the button is clicked the `self.onButtonClick` will run and also start the thread. The thread is started with `.start()`. It should also be noted that we connected the signal `self.calc.countChanged` we created earlier to the method used to update the progress bar value. Every time `External::run::count` is updated the `int` value is also sent to `onCountChanged`.

This is how the GUI could look after making these changes.

[![QThread Progress Bar][2]][2]

It should also feel much more responsive and will not freeze.


  [1]: https://i.stack.imgur.com/vQWzR.png
  [2]: https://i.stack.imgur.com/D9e2a.png

