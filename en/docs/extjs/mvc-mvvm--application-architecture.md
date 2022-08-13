---
title: "MVC  MVVM - Application Architecture"
slug: "mvc--mvvm---application-architecture"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Introduction to models
A model represents some data object in an application. For example you can have a model such as: Fruit, Car, Building, etc. in your application. Models are normally used by stores. Here is example how you would define a new model class. e.g.

    Ext.define('MyApp.model.Person', {
        extend: 'Ext.data.Model',
        fields: [
            {name: 'name', type: 'string'},
            {name: 'surname', type: 'string'},
            {name: 'age', type: 'int'}    
        ],
    
        getFullName: function() {
            return this.get('name') + " " + this.get('surname'); 
        }
    });

After defining our model class we would possibly like to create an instance of it and probably call some methods. For example:

    // Create person instance
    var person = Ext.create('MyApp.model.Person', {
        name : 'Jon',
        surname: 'Doe',
        age  : 24
    });
    
    alert(person.getFullName()); // Display person full name

## ExtJS 4 MVC CRUD App Example
Online demo is here: http://ext4all.com/post/extjs-4-mvc-application-architecture.html 

Define a model:

    // /scripts/app/model/User.js
    Ext.define('AM.model.User', {
        extend: 'Ext.data.Model',
        fields: ['id', 'name', 'email']
    });

Define a store with proxy:

    // /scripts/app/store/Users.js
    Ext.define('AM.store.Users', {
        extend: 'Ext.data.Store',
        model: 'AM.model.User',
        autoLoad: true,
        autoSync: true,
        proxy: {
            type: 'ajax',
            limitParam: 'size',
            startParam: undefined,
            api: {
                create: '/user/add',
                read: '/user/list',
                update: '/user/update',
                destroy: '/user/delete'
            },
            reader: {
                type: 'json',
                root: 'data',
                successProperty: 'success'
            },
            writer: {
                type: 'json',
                writeAllFields: false
            }
        }
    });

Define add user view - it's a window with a form inside:

    // /scripts/app/view/user/Add.js
    Ext.define('AM.view.user.Add', {
        extend: 'Ext.window.Window',
        alias: 'widget.useradd',
        title: 'Add User',
        layout: 'fit',
        autoShow: true,
        initComponent: function () {
            this.items = [
                {
                    xtype: 'form',
                    bodyStyle: {
                        background: 'none',
                        padding: '10px',
                        border: '0'
                    },
                    items: [
                        {
                            xtype: 'textfield',
                            name: 'name',
                            allowBlank: false,
                            fieldLabel: 'Name'
                        },
                        {
                            xtype: 'textfield',
                            name: 'email',
                            allowBlank: false,
                            vtype: 'email',
                            fieldLabel: 'Email'
                        }
                    ]
                }
            ];
            this.buttons = [
                {
                    text: 'Save',
                    action: 'save'
                },
                {
                    text: 'Cancel',
                    scope: this,
                    handler: this.close
                }
            ];
            this.callParent(arguments);
        }
    });

Define edit user view - it's also window with form inside:

    // /scripts/app/view/user/Edit.js
    Ext.define('AM.view.user.Edit', {
        extend: 'Ext.window.Window',
        alias: 'widget.useredit',
        title: 'Edit User',
        layout: 'fit',
        autoShow: true,
        initComponent: function () {
            this.items = [
                {
                    xtype: 'form',
                    bodyStyle: {
                        background: 'none',
                        padding: '10px',
                        border: '0'
                    },
                    items: [
                        {
                            xtype: 'textfield',
                            name: 'name',
                            allowBlank: false,
                            fieldLabel: 'Name'
                        },
                        {
                            xtype: 'textfield',
                            name: 'email',
                            allowBlank: false,
                            vtype: 'email',
                            fieldLabel: 'Email'
                        }
                    ]
                }
            ];
            this.buttons = [
                {
                    text: 'Save',
                    action: 'save'
                },
                {
                    text: 'Cancel',
                    scope: this,
                    handler: this.close
                }
            ];
            this.callParent(arguments);
        }
    });

Define a user list view - it's a grid with columns Id, Name, Email

    // /scripts/app/view/user/List.js
    Ext.define('AM.view.user.List', {
        extend: 'Ext.grid.Panel',
        alias: 'widget.userlist',
        title: 'All Users',
        store: 'Users',
        initComponent: function () {
            this.tbar = [{
                text: 'Create User', action: 'create'
            }];
            this.columns = [
                { header: 'Id', dataIndex: 'id', width: 50 },
                { header: 'Name', dataIndex: 'name', flex: 1 },
                { header: 'Email', dataIndex: 'email', flex: 1 }
            ];
            this.addEvents('removeitem');
            this.actions = {
                removeitem: Ext.create('Ext.Action', {
                    text: 'Remove User',
                    handler: function () { this.fireEvent('removeitem', this.getSelected()) },
                    scope: this
                })
            };
            var contextMenu = Ext.create('Ext.menu.Menu', {
                items: [
                    this.actions.removeitem
                ]
            });
            this.on({
                itemcontextmenu: function (view, rec, node, index, e) {
                    e.stopEvent();
                    contextMenu.showAt(e.getXY());
                    return false;
                }
            });
            this.callParent(arguments);
        },
        getSelected: function () {
            var sm = this.getSelectionModel();
            var rs = sm.getSelection();
            if (rs.length) {
                return rs[0];
            }
            return null;
        }
    });

Define a controller to handle views events:

    // /scripts/app/controller/Users.js
    Ext.define('AM.controller.Users', {
        extend: 'Ext.app.Controller',
        stores: [
            'Users'
        ],
        models: [
            'User'
        ],
        views: [
            'user.List',
            'user.Add',
            'user.Edit'
        ],
        init: function () {
            this.control({
                'userlist': {
                    itemdblclick: this.editUser,
                    removeitem: this.removeUser
                },
                'userlist > toolbar > button[action=create]': {
                    click: this.onCreateUser
                },
                'useradd button[action=save]': {
                    click: this.doCreateUser
                },
                'useredit button[action=save]': {
                    click: this.updateUser
                }
            });
        },
        editUser: function (grid, record) {
            var view = Ext.widget('useredit');
            view.down('form').loadRecord(record);
        },
        removeUser: function (user) {
            Ext.Msg.confirm('Remove User', 'Are you sure?', function (button) {
                if (button == 'yes') {
                    this.getUsersStore().remove(user);
                }
            }, this);
        },
        onCreateUser: function () {
            var view = Ext.widget('useradd');
        },
        doCreateUser: function (button) {
            var win = button.up('window'),
                form = win.down('form'),
                values = form.getValues(),
                store = this.getUsersStore();
            if (form.getForm().isValid()) {
                store.add(values);
                win.close();
            }
        },
        updateUser: function (button) {
            var win = button.up('window'),
                form = win.down('form'),
                record = form.getRecord(),
                values = form.getValues(),
                store = this.getUsersStore();
            if (form.getForm().isValid()) {
                record.set(values);
                win.close();
            }
        }
    });

Define your app in app.js:

    // /scripts/app/app.js
    Ext.Loader.setConfig({  enabled: true });

    Ext.application({
        name: 'AM',
        appFolder: 'scripts/app',
        controllers: [
            'Users'
        ],
        launch: function () {
            Ext.create('Ext.container.Viewport', {
                layout: 'border',
                items: {
                    xtype: 'userlist',
                    region: 'center',
                    margins: '5 5 5 5'
                }
            });
        }
    });

Online demo is here: http://ext4all.com/post/extjs-4-mvc-application-architecture.html 

