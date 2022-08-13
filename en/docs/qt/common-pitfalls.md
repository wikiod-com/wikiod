---
title: "Common Pitfalls"
slug: "common-pitfalls"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Using Qt:DirectConnection when receiver object doesn't receive signal
Some times you see a signal is emitted in sender thread but connected slot doesn't called (in other words it doesn't receive signal), you have asked about it and finaly got that the connection type Qt::DirectConnection would fix it, so the problem found and everything is ok.

But generaly this is bad idea to use Qt:DirectConnection until you really know what is this and there is no other way. Lets explain it more, Each thread created by Qt (including main thread and new threads created by QThread) have Event loop, the event loop is responsible for receiving signals and call aproporiate slots in its thread. Generaly executing a blocking operation inside an slot is bad practice, because it blocks the event loop of that threads so no other slots would be called.

If you block an event loop (by making very time consuming or blocking operation) you will not receive events on that thread until the event loop will be unblocked. If the blocking operation, blocks the event loop forever (such as busy while), the slots could never be called.

In this situation you may set the connection type in connect to Qt::DirectConnection, now the slots will be called even the event loop is blocked. so how this could make broke everything? In Qt::DirectConnection Slots will be called in emiter threads, and not receiver threads and it can broke data synchronizations and ran into other problems. So never use Qt::DirectConnection unless you know what are you doing.
If your problem will be solved by using Qt::DirectConnection, you have to carefull and look at your code and finding out why your event loop is blocked. Its not a good idea to block the event loop and its not recomended in Qt.

Here is small example which shows the problem, as you can see the nonBlockingSlot would be called even the blockingSlot blocked event loop with while(1) which indicates bad coding

    class TestReceiver : public QObject{
        Q_OBJECT
    public:
        TestReceiver(){
             qDebug() << "TestReceiver Constructed in" << QThread::currentThreadId();
        }
    public slots:
        void blockingSlot()
        {
            static bool firstInstance = false;
            qDebug() << "Blocking slot called in thread" << QThread::currentThreadId();
            if(!firstInstance){
                firstInstance = true;
                while(1);
            }
        }
        void nonBlockingSlot(){
            qDebug() << "Non-blocking slot called" << QThread::currentThreadId();
        }
    };
    
    class TestSender : public QObject{
        Q_OBJECT
    public:
        TestSender(TestReceiver * receiver){
            this->nonBlockingTimer.setInterval(100);
            this->blockingTimer.setInterval(100);
    
            connect(&this->blockingTimer, &QTimer::timeout, receiver, &TestReceiver::blockingSlot);
            connect(&this->nonBlockingTimer, &QTimer::timeout, receiver, &TestReceiver::nonBlockingSlot, Qt::DirectConnection);
            this->nonBlockingTimer.start();
            this->blockingTimer.start();
        }
    private:
        QTimer nonBlockingTimer;
        QTimer blockingTimer;
    };
    
    int main(int argc, char *argv[])
    {
        QCoreApplication a(argc, argv);
    
        TestReceiver TestReceiverInstance;
        TestSender testSenderInstance(&TestReceiverInstance);
        QThread receiverThread;
        TestReceiverInstance.moveToThread(&receiverThread);
        receiverThread.start();
    
        return a.exec();
    }


