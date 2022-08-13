---
title: "Integration with C++"
slug: "integration-with-c++"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Creating a simple model for TreeView
Since Qt 5.5 we have a new wonderful [TreeView][1], a control we've all been waiting for.
A [TreeView][1] implements a tree representation of items from a model. 
In general it looks like other QML views - [ListView][2] or [TableView][3]. But data structure of TreeView is more complex.

A data in [ListView][2] or [TableView][3] is represented by one-dimensional array of nodes. In [TreeView][1] each node can contain its own array of nodes.
Therefore, unlike the others views in [TreeView][1] to get specified node we must know parent node, not only row or column of element.

Another major difference is that [TreeView][1] doesn't support [ListModel][4]. To provide a data we must subclass [QAbstractItemModel][5].
In Qt there are ready to use model classes like [QFileSystemModel][6] which
provides access to local file system, or [QSqlTableModel][7] which provides access to a data base.

In following example we will create such model derived from [QAbstractItemModel][6]. 
But to make the example more realistic I suggest to make the model like [ListModel][4] but specified for trees so we can add nodes from QML.
It's necessary to clarify that model itself doesn't contain any data but only provide access to it. 
So providing and organization of data is entirely our responsibility. 

Since model data is organized in a tree the simplest node structure is seen as follows, in pseudo code:

    Node {
        var data;
        Node parent;
        list<Node> children;
    }

In `C++` the node declaration should be as following:

<!-- language: lang-cpp -->

    class MyTreeNode : public QObject
    {
        Q_OBJECT
    public:
        Q_PROPERTY(QQmlListProperty<MyTreeNode> nodes READ nodes)
        Q_CLASSINFO("DefaultProperty", "nodes")
        MyTreeNode(QObject *parent = Q_NULLPTR);
    
        void setParentNode(MyTreeNode *parent);
        Q_INVOKABLE MyTreeNode *parentNode() const;
        bool insertNode(MyTreeNode *node, int pos = (-1));
        QQmlListProperty<MyTreeNode> nodes();
        
        MyTreeNode *childNode(int index) const;
        void clear();
    
        Q_INVOKABLE int pos() const;
        Q_INVOKABLE int count() const;
    
    private:
        QList<MyTreeNode *> m_nodes;
        MyTreeNode *m_parentNode;
    };


We derive our class from [QObject][8] to be able to create a node in `QML`. All the children nodes will be added to `nodes` property so next 2 part of code are the same:

<!-- language: lang-js -->

    TreeNode {
        nodes:[
            TreeNode {}
            TreeNode {}
        ]
    }
    
    TreeNode {
        TreeNode {}
        TreeNode {}
    }

See [this][9] acticle to know more about default property. 

**Node class implementation:**

<!-- language: lang-cpp -->

    MyTreeNode::MyTreeNode(QObject *parent) :
        QObject(parent),
        m_parentNode(nullptr) {}
    
    void MyTreeNode::setParentNode(MyTreeNode *parent)
    {
        m_parentNode = parent;
    }
    
    MyTreeNode *MyTreeNode::parentNode() const
    {
        return m_parentNode;
    }
    
    QQmlListProperty<MyTreeNode> MyTreeNode::nodes()
    {
        QQmlListProperty<MyTreeNode> list(this,
                                          0,
                                          &append_element,
                                          &count_element,
                                          &at_element,
                                          &clear_element);
        return list;
    }
    
    MyTreeNode *MyTreeNode::childNode(int index) const
    {
        if(index < 0 || index >= m_nodes.length())
            return nullptr;
        return m_nodes.at(index);
    }
    
    void MyTreeNode::clear()
    {
        qDeleteAll(m_nodes);
        m_nodes.clear();
    }
    
    bool MyTreeNode::insertNode(MyTreeNode *node, int pos)
    {
        if(pos > m_nodes.count())
            return false;
        if(pos < 0)
            pos = m_nodes.count();
        m_nodes.insert(pos, node);
        return true;
    }
    
    int MyTreeNode::pos() const
    {
        MyTreeNode *parent = parentNode();
        if(parent)
            return parent->m_nodes.indexOf(const_cast<MyTreeNode *>(this));
        return 0;
    }
    
    int MyTreeNode::count() const
    {
        return m_nodes.size();
    }
    
    MyTreeNode *MyTreeModel::getNodeByIndex(const QModelIndex &index)
    {
        if(!index.isValid())
            return nullptr;
        return static_cast<MyTreeNode *>(index.internalPointer());
    }
    
    QModelIndex MyTreeModel::getIndexByNode(MyTreeNode *node)
    {
        QVector<int> positions;
        QModelIndex result;
        if(node) {
            do
            {
                int pos = node->pos();
                positions.append(pos);
                node = node->parentNode();
            } while(node != nullptr);
    
    
            for (int i = positions.size() - 2; i >= 0 ; i--)
            {
                result = index(positions[i], 0, result);
            }
        }
        return result;
    }
    
    bool MyTreeModel::insertNode(MyTreeNode *childNode, const QModelIndex &parent, int pos)
    {
        MyTreeNode *parentElement = getNode(parent);
        if(pos >= parentElement->count())
            return false;
        if(pos < 0)
            pos = parentElement->count();
    
        childNode->setParentNode(parentElement);
        beginInsertRows(parent, pos, pos);
        bool retValue = parentElement->insertNode(childNode, pos);
        endInsertRows();
        return retValue;
    }
    
    MyTreeNode *MyTreeModel::getNode(const QModelIndex &index) const
    {
        if(index.isValid())
            return static_cast<MyTreeNode *>(index.internalPointer());
        return m_rootNode;
    }

 
To expose list-like property to QML through [QQmlListProperty][10] we need next 4 function: 
 
<!-- language: lang-cpp -->

    void append_element(QQmlListProperty<MyTreeNode> *property, MyTreeNode *value)
    {
        MyTreeNode *parent = (qobject_cast<MyTreeNode *>(property->object));
        value->setParentNode(parent);
        parent->insertNode(value, -1);
    }
    
    int count_element(QQmlListProperty<MyTreeNode> *property)
    {
        MyTreeNode *parent = (qobject_cast<MyTreeNode *>(property->object));
        return parent->count();
    }
    
    MyTreeNode *at_element(QQmlListProperty<MyTreeNode> *property, int index)
    {
        MyTreeNode *parent = (qobject_cast<MyTreeNode *>(property->object));
        if(index < 0 || index >= parent->count())
            return nullptr;
        return parent->childNode(index);
    }
    
    void clear_element(QQmlListProperty<MyTreeNode> *property)
    {
        MyTreeNode *parent = (qobject_cast<MyTreeNode *>(property->object));
        parent->clear();
    }

Now let's declare the model class:

<!-- language: lang-cpp -->

    class MyTreeModel : public QAbstractItemModel
    {
        Q_OBJECT
    public:
        Q_PROPERTY(QQmlListProperty<MyTreeNode> nodes READ nodes)
        Q_PROPERTY(QVariantList roles READ roles WRITE setRoles NOTIFY rolesChanged)
        Q_CLASSINFO("DefaultProperty", "nodes")
    
        MyTreeModel(QObject *parent = Q_NULLPTR);
        ~MyTreeModel();
    
        QHash<int, QByteArray> roleNames() const Q_DECL_OVERRIDE;
        QVariant data(const QModelIndex &index, int role) const Q_DECL_OVERRIDE;
        Qt::ItemFlags flags(const QModelIndex &index) const Q_DECL_OVERRIDE;
        QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const Q_DECL_OVERRIDE;
        QModelIndex parent(const QModelIndex &index) const Q_DECL_OVERRIDE;
        int rowCount(const QModelIndex &parent = QModelIndex()) const Q_DECL_OVERRIDE;
        int columnCount(const QModelIndex &parent = QModelIndex()) const Q_DECL_OVERRIDE;
        QQmlListProperty<MyTreeNode> nodes();
    
        QVariantList roles() const;
        void setRoles(const QVariantList &roles);
    
        Q_INVOKABLE MyTreeNode * getNodeByIndex(const QModelIndex &index);
        Q_INVOKABLE QModelIndex getIndexByNode(MyTreeNode *node);
        Q_INVOKABLE bool insertNode(MyTreeNode *childNode, const QModelIndex &parent = QModelIndex(), int pos = (-1));
    
    protected:
        MyTreeNode *getNode(const QModelIndex &index) const;
    
    private:
        MyTreeNode *m_rootNode;
        QHash<int, QByteArray> m_roles;
    
    signals:
        void rolesChanged();
    };

Since we derived out model class from abstract [QAbstractItemModel][5] we must redefine next function: [data()][11], [flags()][12], [index()][13], [parent()][14], [columnCount()][15] and [rowCount()][16]. In order our model could work with `QML`
we define [roleNames()][17]. Also, as well as in node class we define default property to be able to add nodes to the model in `QML`. `roles` property will hold a list of role names.

**The implementation:**

<!-- language: lang-cpp -->

    MyTreeModel::MyTreeModel(QObject *parent) :
        QAbstractItemModel(parent)
    {
        m_rootNode = new MyTreeNode(nullptr);
    }
    MyTreeModel::~MyTreeModel()
    {
        delete m_rootNode;
    }
    
    QHash<int, QByteArray> MyTreeModel::roleNames() const
    {
        return m_roles;
    }
    
    QVariant MyTreeModel::data(const QModelIndex &index, int role) const
    {
        if (!index.isValid())
            return QVariant();
    
        MyTreeNode *item = static_cast<MyTreeNode*>(index.internalPointer());
        QByteArray roleName = m_roles[role];
        QVariant name = item->property(roleName.data());
        return name;
    }
    
    Qt::ItemFlags MyTreeModel::flags(const QModelIndex &index) const
    {
        if (!index.isValid())
            return 0;
    
        return QAbstractItemModel::flags(index);
    }
    
    QModelIndex MyTreeModel::index(int row, int column, const QModelIndex &parent) const
    {
        if (!hasIndex(row, column, parent))
            return QModelIndex();
    
        MyTreeNode *parentItem = getNode(parent);
        MyTreeNode *childItem = parentItem->childNode(row);
        if (childItem)
            return createIndex(row, column, childItem);
        else
            return QModelIndex();
    }
    
    QModelIndex MyTreeModel::parent(const QModelIndex &index) const
    {
        if (!index.isValid())
            return QModelIndex();
    
        MyTreeNode *childItem = static_cast<MyTreeNode*>(index.internalPointer());
        MyTreeNode *parentItem = static_cast<MyTreeNode *>(childItem->parentNode());
    
        if (parentItem == m_rootNode)
            return QModelIndex();
    
        return createIndex(parentItem->pos(), 0, parentItem);
    }
    
    int MyTreeModel::rowCount(const QModelIndex &parent) const
    {
        if (parent.column() > 0)
            return 0;
        MyTreeNode *parentItem = getNode(parent);
        return parentItem->count();
    }
    
    int MyTreeModel::columnCount(const QModelIndex &parent) const
    {
        Q_UNUSED(parent);
        return 1;
    }
    
    QQmlListProperty<MyTreeNode> MyTreeModel::nodes()
    {
        return m_rootNode->nodes();
    }
    
    QVariantList MyTreeModel::roles() const
    {
        QVariantList list;
        QHashIterator<int, QByteArray> i(m_roles);
        while (i.hasNext()) {
            i.next();
            list.append(i.value());
        }
    
        return list;
    }
    
    void MyTreeModel::setRoles(const QVariantList &roles)
    {
        static int nextRole = Qt::UserRole + 1;
        foreach(auto role, roles) {
            m_roles.insert(nextRole, role.toByteArray());
            nextRole ++;
        }
    }
    
    MyTreeNode *MyTreeModel::getNodeByIndex(const QModelIndex &index)
    {
        if(!index.isValid())
            return nullptr;
        return static_cast<MyTreeNode *>(index.internalPointer());
    }
    
    QModelIndex MyTreeModel::getIndexByNode(MyTreeNode *node)
    {
        QVector<int> positions;
        QModelIndex result;
        if(node) {
            do
            {
                int pos = node->pos();
                positions.append(pos);
                node = node->parentNode();
            } while(node != nullptr);
    
    
            for (int i = positions.size() - 2; i >= 0 ; i--)
            {
                result = index(positions[i], 0, result);
            }
        }
        return result;
    }
    
    
    bool MyTreeModel::insertNode(MyTreeNode *childNode, const QModelIndex &parent, int pos)
    {
        MyTreeNode *parentElement = getNode(parent);
        if(pos >= parentElement->count())
            return false;
        if(pos < 0)
            pos = parentElement->count();
    
        childNode->setParentNode(parentElement);
        beginInsertRows(parent, pos, pos);
        bool retValue = parentElement->insertNode(childNode, pos);
        endInsertRows();
        return retValue;
    }
    
    MyTreeNode *MyTreeModel::getNode(const QModelIndex &index) const
    {
        if(index.isValid())
            return static_cast<MyTreeNode *>(index.internalPointer());
        return m_rootNode;
    }

In general, this code it's not much different from the standard implementation, for example [Simple tree example][18]

Instead of defining roles in `C++` we provide a way to do that from `QML`. [TreeView][1] events and methods basically work with [QModelIndex][19]. 
I personally don't see much sense to pass that to qml as the only thing you can do with it is to pass it back to the model. 

Anyway, our class provides a way to convert index to node and vice versa.
To be able to use our classes in QML we need to register it:

<!-- language: lang-cpp -->

    qmlRegisterType<MyTreeModel>("qt.test", 1, 0, "TreeModel");
    qmlRegisterType<MyTreeNode>("qt.test", 1, 0, "TreeElement");
    
And finelly, en example of how we can use our model with TreeView in `QML`:

<!-- language: lang-js -->

    import QtQuick 2.7
    import QtQuick.Window 2.2    
    import QtQuick.Dialogs 1.2
    import qt.test 1.0

    
    Window {
        visible: true
        width: 800
        height: 800
        title: qsTr("Tree example")
    
        Component {
            id: fakePlace
            TreeElement {
                property string name: getFakePlaceName()
                property string population: getFakePopulation()
                property string type: "Fake place"
                function getFakePlaceName() {
                    var rez = "";
                    for(var i = 0;i < Math.round(3 + Math.random() * 7);i ++) {
                        rez += String.fromCharCode(97 + Math.round(Math.random() * 25));
                    }
                    return rez.charAt(0).toUpperCase() + rez.slice(1);
                }
                function getFakePopulation() {
                    var num = Math.round(Math.random() * 100000000);
                    num = num.toString().split("").reverse().join("");
                    num = num.replace(/(\d{3})/g, '$1,');
                    num = num.split("").reverse().join("");
                    return num[0] === ',' ? num.slice(1) : num;
                }
            }
        }
    
        TreeModel {
            id: treemodel
            roles: ["name","population"]
    
            TreeElement {
                property string name: "Asia"
                property string population: "4,164,252,000"
                property string type: "Continent"
                TreeElement {
                    property string name: "China";
                    property string population: "1,343,239,923"
                    property string type: "Country"
                    TreeElement { property string name: "Shanghai"; property string population: "20,217,700"; property string type: "City" }
                    TreeElement { property string name: "Beijing"; property string population: "16,446,900"; property string type: "City" }
                    TreeElement { property string name: "Chongqing"; property string population: "11,871,200"; property string type: "City" }
                }
                TreeElement {
                    property string name: "India";
                    property string population: "1,210,193,422"
                    property string type: "Country"
                    TreeElement { property string name: "Mumbai"; property string population: "12,478,447"; property string type: "City" }
                    TreeElement { property string name: "Delhi"; property string population: "11,007,835"; property string type: "City" }
                    TreeElement { property string name: "Bengaluru"; property string population: "8,425,970"; property string type: "City" }
                }
                TreeElement {
                    property string name: "Indonesia";
                    property string population: "248,645,008"
                    property string type: "Country"
                    TreeElement {property string name: "Jakarta"; property string population: "9,588,198"; property string type: "City" }
                    TreeElement {property string name: "Surabaya"; property string population: "2,765,487"; property string type: "City" }
                    TreeElement {property string name: "Bandung"; property string population: "2,394,873"; property string type: "City" }
                }
            }
            TreeElement { property string name: "Africa"; property string population: "1,022,234,000"; property string type: "Continent" }
            TreeElement { property string name: "North America"; property string population: "542,056,000"; property string type: "Continent" }
            TreeElement { property string name: "South America"; property string population: "392,555,000"; property string type: "Continent" }
            TreeElement { property string name: "Antarctica"; property string population: "4,490"; property string type: "Continent" }
            TreeElement { property string name: "Europe"; property string population: "738,199,000"; property string type: "Continent" }
            TreeElement { property string name: "Australia"; property string population: "29,127,000"; property string type: "Continent" }
        }
    
        TreeView {
            anchors.fill: parent
            model: treemodel
            TableViewColumn {
                title: "Name"
                role: "name"
                width: 200
            }
            TableViewColumn {
                title: "Population"
                role: "population"
                width: 200
            }
    
            onDoubleClicked: {
                var element = fakePlace.createObject(treemodel);
                treemodel.insertNode(element, index, -1);
            }
            onPressAndHold: {
                var element = treemodel.getNodeByIndex(index);
                messageDialog.text = element.type + ": " + element.name + "\nPopulation: " + element.population;
                messageDialog.open();
            }
        }
        MessageDialog {
              id: messageDialog
              title: "Info"
          }
    }

Double click for adding a node, press and hold for node info.     


  [1]: http://doc.qt.io/qt-5/qml-qtquick-controls-treeview.html
  [2]: http://doc.qt.io/qt-5/qml-qtquick-listview.html
  [3]: http://doc.qt.io/qt-5/qml-qtquick-controls-tableview.html
  [4]: http://doc.qt.io/qt-5/qml-qtqml-models-listmodel.html
  [5]: http://doc.qt.io/qt-5/qabstractitemmodel.html
  [6]: http://doc.qt.io/qt-5/qfilesystemmodel.html
  [7]: http://doc.qt.io/qt-5/qsqltablemodel.html
  [8]: http://doc.qt.io/qt-5/qobject.html
  [9]: http://doc.qt.io/qt-5/qtqml-cppintegration-definetypes.html#specifying-default-properties-for-qml-object-types
  [10]: http://doc.qt.io/qt-5/qqmllistproperty.html
  [11]: http://doc.qt.io/qt-5/qabstractitemmodel.html#data
  [12]: http://doc.qt.io/qt-5/qabstractitemmodel.html#flags
  [13]: http://doc.qt.io/qt-5/qabstractitemmodel.html#index
  [14]: http://doc.qt.io/qt-5/qabstractitemmodel.html#parent
  [15]: http://doc.qt.io/qt-5/qabstractitemmodel.html#columnCount
  [16]: http://doc.qt.io/qt-5/qabstractitemmodel.html#rowCount
  [17]: http://doc.qt.io/qt-5/qabstractitemmodel.html#roleNames
  [18]: http://doc.qt.io/qt-5/qtwidgets-itemviews-simpletreemodel-example.html
  [19]: http://doc.qt.io/qt-5/qmodelindex.html

## Creating a QtQuick view from C++
It is possible to create a QtQuick view directly from C++ and to expose to QML C++ defined properties. In the code below the C++ program creates a QtQuick view and exposes to QML the height and width of the view as properties.

**main.cpp**

<!-- language: cpp -->
    #include <QApplication>
    #include <QQmlContext>
    #include <QQuickView>
    
    int main(int argc, char *argv[])
    {
        QApplication app(argc, argv);
    
        // Creating the view and manually setting the QML file it should display
        QQuickView view;
        view.setSource(QStringLiteral("main.qml"));
        
        // Retrieving the QML context. This context allows us to expose data to the QML components
        QQmlContext* rootContext = view.rootContext();

        // Creating 2 new properties: the width and height of the view
        rootContext->setContextProperty("WINDOW_WIDTH", 640);
        rootContext->setContextProperty("WINDOW_HEIGHT", 360);

        // Let's display the view
        view.show();
    
        return app.exec();
    }

**main.qml**

<!-- language: lang-js -->
    import QtQuick 2.0

    Rectangle {
        // We can now access the properties we defined from C++ from the whole QML file
        width: WINDOW_WIDTH
        height: WINDOW_HEIGHT
    
        Text {
            text: qsTr("Hello World")
            anchors.centerIn: parent
        }
    }


## Creating a QtQuick Window from C++
As of Qt 5.1 and later you can use QQmlApplicationEngine instead of QQuickView to load and render a QML script.

With QQmlApplicationEngine you do need to use a QML Window type as your root element.

You can obtain the root context from the engine where you can then add global properties to the context which can be access by the engine when processing QML scripts.

**main.cpp**

<!-- language: cpp -->
    #include <QGuiApplication>
    #include <QQmlApplicationEngine>
    #include <QQmlContext>

    int main(int argc, char *argv[])
    {
        QGuiApplication app(argc, argv);

        QQmlApplicationEngine engine;

        QQmlContext* rootContext = engine.rootContext();
        rootContext->setContextProperty("WINDOW_WIDTH", 640);
        rootContext->setContextProperty("WINDOW_HEIGHT", 360);

        engine.load(QUrl(QStringLiteral("qrc:/main.qml")));

        return app.exec();
    }

**main.qml**

<!-- language: lang-js -->
    import QtQuick 2.5
    import QtQuick.Window 2.2

    Window { // Must be this type to be loaded by QQmlApplicationEngine.
        visible: true
        width: WINDOW_WIDTH   //Accessing global context declared in C++
        height: WINDOW_HEIGHT //Accessing global context declared in C++
        title: qsTr("Hello World")
        Component.onCompleted: {
            // We can access global context from within JavaScript too.
            console.debug( "Width: " + WINDOW_WIDTH )
            console.debug( "Height: " + WINDOW_HEIGHT )
        }

        MouseArea {
            anchors.fill: parent
            onClicked: {
                Qt.quit();
            }
        }

        Text {
            text: qsTr("Hello World")
            anchors.centerIn: parent
        }
    }


