---
title: "QGraphics"
slug: "qgraphics"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Pan, zoom, and rotate with QGraphicsView
`QGraphics` can be used to organize complicated scenes of visual objects into a framework that makes them easier to handle. 

There are three major types of objects used in this framework [QGraphicsView][1], [QGraphicsScene][2], and [QGraphicsItems][3]. QGraphicsItems are the basic visual items that exist in the scene. 

There are many types that are pre-built and can be used such as [Ellipses][4], [Lines][5], [Paths][6], [Pixmaps][7], [Polygons][8], [Rectangles][9], and [Text][10]. 

You can also make your own items by inheriting `QGraphicsItem`. These items are then put into a `QGraphicsScene` which is basically the world you are planning to look at. The items can move within the scene which is like having them move in the world you are looking at. The items positioning and orientation is handled by transformation matrices called [QTransforms][11]. Qt has nice functions built in so you usually do not need to work with the `QTransforms` directly, instead you call functions such as rotate or scale which create the proper transforms for you. The scene is then viewed by the perspective defined in the `QGraphicsView` (again with `QTransforms`), which is the piece you would put into a widget in you UI.

In the following example there is a very simple scene with just one item (a pixmap), which is put into a scene and displayed in a view. By turning on the `DragMode` flag the scene can be panned around with the mouse and by using the scale and rotate functions it can be scaled in and out with the scroll on the mouse and rotated with the arrow keys. 

If you would like to run this example create a instance of View that will be displayed and create a [resource][12] file with the prefix /images containing a image my_image.png.

    #include <QGraphicsView>
    #include <QGraphicsScene>
    #include <QGraphicsPixmapItem>
    #include <QWheelEvent>
    #include <QKeyEvent>

    class View : public QGraphicsView
    {
      Q_OBJECT
    public:
      explicit View(QWidget *parent = 0) :
        QGraphicsView(parent)
      {
        setDragMode(QGraphicsView::ScrollHandDrag);

        QGraphicsPixmapItem *pixmapItem = new QGraphicsPixmapItem(QPixmap(":/images/my_image.png"));
        pixmapItem->setTransformationMode(Qt::SmoothTransformation);

        QGraphicsScene *scene = new QGraphicsScene();
        scene->addItem(pixmapItem);
        setScene(scene);
      }

    protected Q_SLOTS:
      void wheelEvent(QWheelEvent *event)
      {
        if(event->delta() > 0)
          scale(1.25, 1.25);
        else
          scale(0.8, 0.8);
      }

      void keyPressEvent(QKeyEvent *event)
      {
        if(event->key() == Qt::Key_Left)
          rotate(1);
        else if(event->key() == Qt::Key_Right)
          rotate(-1);
      }
    };


  [1]: http://doc.qt.io/qt-5/qgraphicsview.html
  [2]: http://doc.qt.io/qt-5/qgraphicsscene.html
  [3]: http://doc.qt.io/qt-5/qgraphicsitem.html
  [4]: http://doc.qt.io/qt-5/qgraphicsellipseitem.html
  [5]: http://doc.qt.io/qt-5/qgraphicslineitem.html
  [6]: http://doc.qt.io/qt-5/qgraphicspathitem.html
  [7]: http://doc.qt.io/qt-5/qgraphicspixmapitem.html
  [8]: http://doc.qt.io/qt-5/qgraphicspolygonitem.html
  [9]: http://doc.qt.io/qt-5/qgraphicsrectitem.html
  [10]: http://doc.qt.io/qt-5/qgraphicstextitem.html
  [11]: http://doc.qt.io/qt-5/qt3dcore-qtransform.html
  [12]: http://doc.qt.io/qt-5/resources.html

