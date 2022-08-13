---
title: "Using threads with PyQt"
slug: "using-threads-with-pyqt"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

While some parts of the Qt framework are thread safe, much of it is not. The [Qt C++ documentation][1] provides a good overview of which classes are reentrant (can be used to instantiate objects in multiple threads). The following rules are the most widely sought:

 - You cannot create or access a Qt GUI object from outside the main thread (e.g. anything that subclasses `QWidget` or similar).
 - Even if the Qt class is reentrant, you cannot share access to a Qt object between threads unless the Qt documentation for that class explicitly states that instances are thread safe. 
 - You can use `QObject.moveToThread()` if you need to move a Qt object from one thread to another (does not apply to Qt GUI objects which must always remain in the main thread). But note that the object must not have a parent.

As per [this][2] Stack Overflow QA, it is not recommended to use Python threads if your thread intends to interact with PyQt in any way (even if that part of the Qt framework is thread safe). 


  [1]: http://doc.qt.io/qt-4.8/threads-qobject.html
  [2]: http://stackoverflow.com/q/1595649/1994235

## The worker model
<!-- language: python -->
    
    # this method can be anything and anywhere as long as it is accessible for connection
    @pyqtSlot()
    def run_on_complete():
       
        pass

    # An object containing methods you want to run in a thread
    class Worker(QObject):
        complete = pyqtSignal()
        
        @pyqtSlot()
        def a_method_to_run_in_the_thread(self):
            # your code
            
            # Emit the complete signal
            self.complete.emit() 

    # instantiate a QThread
    thread = QThread()
    # Instantiate the worker object
    worker = Worker()
    # Relocate the Worker object to the thread
    worker.moveToThread(thread)
    # Connect the 'started' signal of the QThread to the method you wish to run
    thread.started.connect(worker.a_method_to_run_in_the_thread)
    # connect to the 'complete' signal which the code in the Worker object emits at the end of the method you are running
    worker.complete.connect(run_on_complete)
    # start the thread (Which will emit the 'started' signal you have previously connected to)
    thread.start()

