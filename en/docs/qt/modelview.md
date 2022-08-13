---
title: "ModelView"
slug: "modelview"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## A Simple Read-only Table to View Data from a Model
This is a simple example to display read-only data that is tabular in nature using Qt's [Model/View Framework][1]. Specifically, the `Qt Objects` [QAbstractTableModel][2] (sub-classed in this example) and [QTableView][3] are used.

Implementations of the methods [rowCount()][4], [columnCount()][5], [data()][6] and [headerData()][7] are required to give the `QTableView` object a means to obtain information about the data contained in the `QAbstractTableModel` object.

The method `populateData()` was added to this example to provide a means to populate the `QAbstractTableModel` object with data from some arbitrary source.

mainwindow.h

    #ifndef MAINWINDOW_H
    #define MAINWINDOW_H
    #include <QMainWindow>
    #include <QAbstractTableModel>
    
    namespace Ui {
        class MainWindow;
    }
    
    class TestModel : public QAbstractTableModel
    {
        Q_OBJECT
    
    public:
        TestModel(QObject *parent = 0);
    
        void populateData(const QList<QString> &contactName,const QList<QString> &contactPhone);
    
        int rowCount(const QModelIndex &parent = QModelIndex()) const Q_DECL_OVERRIDE;
        int columnCount(const QModelIndex &parent = QModelIndex()) const Q_DECL_OVERRIDE;
    
        QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const Q_DECL_OVERRIDE;
        QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const Q_DECL_OVERRIDE;
    
    private:
        QList<QString> tm_contact_name;
        QList<QString> tm_contact_phone;
    
    };
    
    class MainWindow : public QMainWindow
    {
        Q_OBJECT
    
    public:
        explicit MainWindow(QWidget *parent = 0);
        ~MainWindow();
    
    private:
        Ui::MainWindow *ui;
    
    };
    
    #endif // MAINWINDOW_H

mainwindow.cpp

    #include "mainwindow.h"
    #include "ui_mainwindow.h"
    
    MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
    {
        ui->setupUi(this);
    
        QList<QString> contactNames;
        QList<QString> contactPhoneNums;
    
        // Create some data that is tabular in nature:
        contactNames.append("Thomas");
        contactNames.append("Richard");
        contactNames.append("Harrison");
        contactPhoneNums.append("123-456-7890");
        contactPhoneNums.append("222-333-4444");
        contactPhoneNums.append("333-444-5555");
    
        // Create model:
        TestModel *PhoneBookModel = new TestModel(this);
    
        // Populate model with data:
        PhoneBookModel->populateData(contactNames,contactPhoneNums);
    
        // Connect model to table view:
        ui->tableView->setModel(PhoneBookModel);

        // Make table header visible and display table:
        ui->tableView->horizontalHeader()->setVisible(true);
        ui->tableView->show();
    }
    
    MainWindow::~MainWindow()
    {
        delete ui;
    }
    
    TestModel::TestModel(QObject *parent) : QAbstractTableModel(parent)
    {
    }
    
    // Create a method to populate the model with data:
    void TestModel::populateData(const QList<QString> &contactName,const QList<QString> &contactPhone)
    {
        tm_contact_name.clear();
        tm_contact_name = contactName;
        tm_contact_phone.clear();
        tm_contact_phone = contactPhone;
        return;
    }
    
    int TestModel::rowCount(const QModelIndex &parent) const
    {
        Q_UNUSED(parent);
        return tm_contact_name.length();
    }
    
    int TestModel::columnCount(const QModelIndex &parent) const
    {
        Q_UNUSED(parent);
        return 2;
    }
    
    QVariant TestModel::data(const QModelIndex &index, int role) const
    {
        if (!index.isValid() || role != Qt::DisplayRole) {
            return QVariant();
        }
        if (index.column() == 0) {
            return tm_contact_name[index.row()];
        } else if (index.column() == 1) {
            return tm_contact_phone[index.row()];
        }
        return QVariant();
    }
    
    QVariant TestModel::headerData(int section, Qt::Orientation orientation, int role) const
    {
        if (role == Qt::DisplayRole && orientation == Qt::Horizontal) {
            if (section == 0) {
                return QString("Name");
            } else if (section == 1) {
                return QString("Phone");
            }
        }
        return QVariant();
    }


Using `Qt Creator/Design`, place a `Table View` object, named **tableView** in this example, in the **main window**:

[![enter image description here][8]][8]


The resulting program displays as:

[![enter image description here][9]][9]


  [1]: http://doc.qt.io/qt-5/model-view-programming.html
  [2]: http://doc.qt.io/qt-5/qabstracttablemodel.html#details
  [3]: http://doc.qt.io/qt-5/qtableview.html#details
  [4]: http://doc.qt.io/qt-5/qabstractitemmodel.html#rowCount
  [5]: http://doc.qt.io/qt-5/qabstractitemmodel.html#columnCount
  [6]: http://doc.qt.io/qt-5/qabstractitemmodel.html#data
  [7]: http://doc.qt.io/qt-5/qabstractitemmodel.html#headerData
  [8]: http://i.stack.imgur.com/lobjJ.png
  [9]: http://i.stack.imgur.com/1ALiZ.png

## A simple tree model
[QModelIndex][1] does not actually know about it's parent/child indexes, it only contains a **row**, a **column** and a **pointer**, and it is the models responsibility to use this data to provide information an index's relations. The model therefore needs to do a lot of conversions from the `void*` stored inside the `QModelIndex` to an internal data type and back.

TreeModel.h:

    #pragma once
    
    #include <QAbstractItemModel>

    class TreeModel : public QAbstractItemModel
    {
        Q_OBJECT
    public:
        explicit TreeModel(QObject *parent = nullptr);

        // Reimplementation of QAbstractItemModel methods
        int rowCount(const QModelIndex &index) const override;
        int columnCount(const QModelIndex &index) const override;
        QModelIndex index(const int row, const int column,
            const QModelIndex &parent) const override;
        QModelIndex parent(const QModelIndex &childIndex) const override;
        QVariant data(const QModelIndex &index, const int role) const override;
        bool setData(const QModelIndex &index, const QVariant &value,
            const int role) override;
        Qt::ItemFlags flags(const QModelIndex &index) const override;

        void addRow(const QModelIndex &parent, const QVector<QVariant> &values);
        void removeRow(const QModelIndex &index);

    private:
        struct Item
        {
            ~Item();

            // This could individual members, or maybe some other object that
            // contains the data we want to display/edit
            QVector<QVariant> values;

            // It is this information that the model needs to be able to answer
            // questions like "What's the parent QModelIndex of this QModelIndex?"
            QVector<Item *> children;
            Item *parent = nullptr;

            // Convenience method that's used in several places
            int rowInParent() const;
        };
        Item *m_root;
    };

TreeModel.cpp:

    #include "TreeModel.h"

    // Adapt this to own needs
    static constexpr int COLUMNS = 3;

    TreeModel::Item::~Item()
    {
        qDeleteAll(children);
    }
    int TreeModel::Item::rowInParent() const
    {
        if (parent) {
            return parent->children.indexOf(const_cast<Item *>(this));
        } else {
            return 0;
        }
    }

    TreeModel::TreeModel(QObject *parent)
        : QAbstractItemModel(parent), m_root(new Item) {}

    int TreeModel::rowCount(const QModelIndex &parent) const
    {
        // Parent being invalid means we ask for how many rows the root of the
        // model has, thus we ask the root item
        // If parent is valid we access the Item from the pointer stored
        // inside the QModelIndex
        return parent.isValid()
            ? static_cast<Item *>(parent.internalPointer())->children.size()
            : m_root->children.size();
    }
    int TreeModel::columnCount(const QModelIndex &parent) const
    {
        return COLUMNS;
    }

    QModelIndex TreeModel::index(const int row, const int column,
        const QModelIndex &parent) const
    {
        // hasIndex checks if the values are in the valid ranges by using
        // rowCount and columnCount
        if (!hasIndex(row, column, parent)) {
            return QModelIndex();
        }

        // In order to create an index we first need to get a pointer to the Item
        // To get started we have either the parent index, which contains a pointer
        // to the parent item, or simply the root item

        Item *parentItem = parent.isValid()
            ? static_cast<Item *>(parent.internalPointer())
            : m_root;

        // We can now simply look up the item we want given the parent and the row
        Item *childItem = parentItem->children.at(row);

        // There is no public constructor in QModelIndex we can use, instead we need
        // to use createIndex, which does a little bit more, like setting the
        // model() in the QModelIndex to the model that calls createIndex
        return createIndex(row, column, childItem);
    }
    QModelIndex TreeModel::parent(const QModelIndex &childIndex) const
    {
        if (!childIndex.isValid()) {
            return QModelIndex();
        }

        // Simply get the parent pointer and create an index for it
        Item *parentItem = static_cast<Item*>(childIndex.internalPointer())->parent;
        return parentItem == m_root
            ? QModelIndex() // the root doesn't have a parent
            : createIndex(parentItem->rowInParent(), 0, parentItem);
    }

    QVariant TreeModel::data(const QModelIndex &index, const int role) const
    {
        // Usually there will be more stuff here, like type conversion from
        // QVariant, handling more roles etc.
        if (!index.isValid() || role != Qt::DisplayRole) {
            return QVariant();
        }
        Item *item = static_cast<Item *>(index.internalPointer());
        return item->values.at(index.column());
    }
    bool TreeModel::setData(const QModelIndex &index, const QVariant &value,
        const int role)
    {
        // As in data there will usually be more stuff here, like type conversion to
        // QVariant, checking values for validity etc.
        if (!index.isValid() || role != Qt::EditRole) {
            return false;
        }
        Item *item = static_cast<Item *>(index.internalPointer());
        item->values[index.column()] = value;
        emit dataChanged(index, index, QVector<int>() << role);
        return true;
    }
    Qt::ItemFlags TreeModel::flags(const QModelIndex &index) const
    {
        if (index.isValid()) {
            return Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsEditable;
        } else {
            return Qt::NoItemFlags;
        }
    }
    
    // Simple add/remove functions to illustrate {begin,end}{Insert,Remove}Rows
    // usage in a tree model
    void TreeModel::addRow(const QModelIndex &parent,
        const QVector<QVariant> &values)
    {
        Item *parentItem = parent.isValid()
            ? static_cast<Item *>(parent.internalPointer())
            : m_root;
        beginInsertRows(parent,
            parentItem->children.size(), parentItem->children.size());
        Item *item = new Item;
        item->values = values;
        item->parent = parentItem;
        parentItem->children.append(item);
        endInsertRows();
    }
    void TreeModel::removeRow(const QModelIndex &index)
    {
        if (!index.isValid()) {
            return;
        }
        Item *item = static_cast<Item *>(index.internalPointer());
        Q_ASSERT(item != m_root);
        beginRemoveRows(index.parent(), item->rowInParent(), item->rowInParent());
        item->parent->children.removeOne(item);
        delete item;
        endRemoveRows();
    }


  [1]: http://doc.qt.io/qt-5/qmodelindex.html#details

