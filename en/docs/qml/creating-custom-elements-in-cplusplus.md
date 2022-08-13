---
title: "Creating custom elements in C++"
slug: "creating-custom-elements-in-c++"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Creating custom elements in C++
QML came with rich set of visual elements. Using only QML we can build complex applications with these elements. 
Also it's very easy to build your own element based on set of standard items like Rectangle, Button, Image etc. 
Moreover, we can use items like Canvas to build element with custom painting.
It would seem that we can build a variety of applications in QML only, without touching the capabilities of C++.
And it's actually true but still sometimes we would like to make our application faster or we want to extend it with power of Qt or to add some opportunity which are not available in QML.
And certainly there is such possibility in *QML*.
Basically *QtQuick* uses *Scene Graph* to paint its content a high-performance rendering engine based on *OpenGL*. 
To implement our own visual element we can use 2 ways:
1. The traditional for Qt way using [QPainter][1] ([QQuickPaintedItem][2]).
2. The common QML way using [QQuickItem][3] and OpenGL functionality. 

It is possible that the first method seems easier but it's worth considering that it is also slower than the first one since QtQuick paints the item's content on a surface and then 
insert it into scene graph so the rendering is a two-step operation. So using scene graph API directly is always significantly faster.

In order to explore both methods closer let's create our own element which definitely doesn't exist in QML, for example a triangle.

***Class declaration***

<!-- language: lang-cpp -->

    class QQuickCustomItem : public QQuickItem
    {
        Q_OBJECT
        Q_PROPERTY(QColor color READ color WRITE setColor NOTIFY colorChanged)
    public:
        QQuickCustomItem(QQuickItem *parent = Q_NULLPTR);
    
    protected:
        QSGNode *updatePaintNode(QSGNode *oldNode, UpdatePaintNodeData *updatePaintNodeData);
    
        QColor color() const;
        void setColor(const QColor &color);
    
    private:
        QColor m_color;
        bool m_needUpdate;
    
    signals:
        void colorChanged();
    };

We add [Q_OBJECT][4] macro to work with signals. Also we add custom property to specify color of our Rectangle.
To make it works all we need is reimplement virtual function [QQuiclItem::updatePaintNode()][5].

***Class implementation.*** 

Firstly we define a constructor.

<!-- language: lang-cpp -->

    QQuickCustomItem::QQuickCustomItem(QQuickItem *parent) :
        QQuickItem(parent),
        m_color(Qt::red),
        m_needUpdate(true)
    {
        setFlag(QQuickItem::ItemHasContents);
    }

Please note that the [setFlag()][6] function call is mandatory otherwise your object will not be added to the scene graph.
Next, we define a function for the paining.

<!-- language: lang-cpp -->

    QSGNode *QQuickCustomItem::updatePaintNode(QSGNode *oldNode, QQuickItem::UpdatePaintNodeData *updatePaintNodeData)
    {
        Q_UNUSED(updatePaintNodeData)
        QSGGeometryNode *root = static_cast<QSGGeometryNode *>(oldNode);
    
        if(!root) {
            root = new QSGGeometryNode;
            QSGGeometry *geometry = new QSGGeometry(QSGGeometry::defaultAttributes_Point2D(), 3);
            geometry->setDrawingMode(GL_TRIANGLE_FAN);
            geometry->vertexDataAsPoint2D()[0].set(width() / 2, 0);
            geometry->vertexDataAsPoint2D()[1].set(width(), height());
            geometry->vertexDataAsPoint2D()[2].set(0, height());
    
            root->setGeometry(geometry);
            root->setFlag(QSGNode::OwnsGeometry);
            root->setFlag(QSGNode::OwnsMaterial);
        }
    
        if(m_needUpdate) {
            QSGFlatColorMaterial *material = new QSGFlatColorMaterial;
            material->setColor(m_color);
            root->setMaterial(material);
            m_needUpdate = false;
        }
    
        return root;
    }

At the first call to the function our node isn't created yet so `oldNode` will be NULL. So we create the node and assign geometry and material to it.
Here we use *GL_TRIANGLE_FAN* for our geometry to paint solid rectangle. This point is the same as in OpenGL. For example to draw triangle frame we can change the code to:

<!-- language: lang-cpp -->

    geometry->setDrawingMode(GL_LINE_LOOP);
    geometry->setLineWidth(5);
        
You can refer to *OpenGL* manual to check for other shapes.
So, all that remains is to define setter/getter for our property:

<!-- language: lang-cpp -->

    QColor QQuickCustomItem::color() const
    {
        return m_color;
    }
    
    void QQuickCustomItem::setColor(const QColor &color)
    {
        if(m_color != color) {
            m_color = color;
            m_needUpdate = true;
            update();
            colorChanged();
        }
    }

Now there is only one small detail to make it works. We need to notify *QtQuick* of the new item.
For example, you can add this code to your main.cpp:

<!-- language: lang-cpp -->

    qmlRegisterType<QQuickCustomItem>("stackoverflow.qml", 1, 0, "Triangle");
        
And here is our QML test file:        
    
<!-- language: lang-js -->

    import QtQuick 2.7
    import QtQuick.Window 2.0
    import stackoverflow.qml 1.0
    
    Window {
        width: 800
        height: 800
        visible: true
    
        Rectangle {
            width: 200
            height: 200
            anchors.centerIn: parent
            color: "lightgrey"
    
            Triangle {
                id: rect
                width: 200
                height: 200
                transformOrigin: Item.Top
                color: "green"
                onColorChanged: console.log("color was changed");
                PropertyAnimation on rotation {
                    from: 0
                    to: 360
                    duration: 5000
                    loops: Animation.Infinite
                }
            }
        }
        Timer {
            interval: 1000
            repeat: true
            running: true
            onTriggered: rect.color = Qt.rgba(Math.random(),Math.random(),Math.random(),1);
        }
    }

As you see our item behaves like all other QML items.
Now let's create the same item using [QPainter][1]:

All we need is to replace 

    QSGNode *updatePaintNode(QSGNode *oldNode, UpdatePaintNodeData *updatePaintNodeData);

with

    void paint(QPainter *painter);

and, of cource inherit our class from `QQuickPaintedItem` instead of `QQuickItem`.
Here is our painting function:

<!-- language: lang-cpp -->

    void QQuickCustomItem::paint(QPainter *painter)
    {
        QPainterPath path;
        path.moveTo(width() / 2, 0);
        path.lineTo(width(), height());
        path.lineTo(0, height());
        path.lineTo(width() / 2, 0);
        painter->fillPath(path, m_color);
    }

Everything else remains unchanged.


  [1]: http://doc.qt.io/qt-5/qpainter.html
  [2]: http://doc.qt.io/qt-5/qquickpainteditem.html
  [3]: http://doc.qt.io/qt-5/qquickitem.html
  [4]: http://doc.qt.io/qt-5/qobject.html#Q_OBJECT
  [5]: http://doc.qt.io/qt-5/qquickitem.html#updatePaintNode
  [6]: http://doc.qt.io/qt-5/qquickitem.html#setFlag

