---
title: "Preferences"
slug: "preferences"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Adding event listeners
There are two types of events emitted by a [`Preferences`][1] object: [`PreferenceChangeEvent`][2] and [`NodeChangeEvent`][3].

### PreferenceChangeEvent

A `PreferenceChangeEvent` gets emitted by a `Properties` object every time one of the node's key-value-pairs changes. `PreferenceChangeEvent`s can be listened for with a [`PreferenceChangeListener`][4]:

<!-- if version [gte Java SE 8] -->

    preferences.addPreferenceChangeListener(evt -> {
        String newValue = evt.getNewValue();
        String changedPreferenceKey = evt.getKey();
        Preferences changedNode = evt.getNode();
    });

<!-- end version if -->

<!-- if version [lt Java SE 8] -->

    preferences.addPreferenceChangeListener(new PreferenceChangeListener() {
        @Override
        public void preferenceChange(PreferenceChangeEvent evt) {
            String newValue = evt.getNewValue();
            String changedPreferenceKey = evt.getKey();
            Preferences changedNode = evt.getNode();
        }
    });

<!-- end version if -->

This listener will not listen to changed key-value pairs of child nodes.

### NodeChangeEvent

This event will be fired whenever a child node of a `Properties` node is added or removed.

    preferences.addNodeChangeListener(new NodeChangeListener() {
        @Override
        public void childAdded(NodeChangeEvent evt) {
            Preferences addedChild = evt.getChild();
            Preferences parentOfAddedChild = evt.getParent();
        }

        @Override
        public void childRemoved(NodeChangeEvent evt) {
            Preferences removedChild = evt.getChild();
            Preferences parentOfRemovedChild = evt.getParent();
        }
    });

 [1]: https://docs.oracle.com/javase/8/docs/api/java/util/prefs/Preferences.html
 [2]: https://docs.oracle.com/javase/8/docs/api/java/util/prefs/PreferenceChangeEvent.html
 [3]: https://docs.oracle.com/javase/8/docs/api/java/util/prefs/NodeChangeEvent.html
 [4]: https://docs.oracle.com/javase/8/docs/api/java/util/prefs/PreferenceChangeListener.html

## Getting sub-nodes of Preferences
`Preferences` objects always represent a specific node in a whole `Preferences` tree, kind of like this:

<!-- Sorry for the HTML tags, but there is no other way to make bold text in code blocks than to use HTML tags. Markdown cannot be used. -->
<!-- language: lang-none -->

<pre><code><b>/userRoot</b>
├── <b>com</b>
│   └── <b>mycompany</b>
│       └── <b>myapp</b>
│           ├── darkApplicationMode=true
│           ├── showExitConfirmation=false
│           └── windowMaximized=true
└── <b>org</b>
    └── <b>myorganization</b>
        └── <b>anotherapp</b>
            ├── defaultFont=Helvetica
            ├── defaultSavePath=/home/matt/Documents
            └── <b>exporting</b>
                ├── defaultFormat=pdf
                └── openInBrowserAfterExport=false</pre></code>

To select the `/com/mycompany/myapp` node:

1. By convention, based on the package of a class:

       package com.mycompany.myapp;

       // ...

       // Because this class is in the com.mycompany.myapp package, the node
       // /com/mycompany/myapp will be returned.
       Preferences myApp = Preferences.userNodeForPackage(getClass());

2. By relative path:

       Preferences myApp = Preferences.userRoot().node("com/mycompany/myapp");

   Using a relative path (a path not starting with a `/`) will cause the path to be resolved relative to the parent node it is resolved on. For example, the following example will return the node of the path `/one/two/three/com/mycompany/myapp`:

       Preferences prefix = Preferences.userRoot().node("one/two/three");
       Preferences myAppWithPrefix = prefix.node("com/mycompany/myapp");
       // prefix          is /one/two/three
       // myAppWithPrefix is /one/two/three/com/mycompany/myapp

3. By absolute path:

       Preferences myApp = Preferences.userRoot().node("/com/mycompany/myapp");

   Using an absolute path on the root node will not be different from using a relative path. The difference is that, if called on a sub-node, the path will be resolved relative to the root node.

       Preferences prefix = Preferences.userRoot().node("one/two/three");
       Preferences myAppWitoutPrefix = prefix.node("/com/mycompany/myapp");
       // prefix            is /one/two/three
       // myAppWitoutPrefix is /com/mycompany/myapp

## Coordinating preferences access across multiple application instances
All instances of `Preferences` are always thread-safe across the threads of a single Java Virtual Machine (JVM). Because `Preferences` can be shared across multiple JVMs, there are special methods that deal with synchronizing changes across virtual machines.

If you have an application which is supposed to run in a **single instance** only, then **no external synchronization** is required.

If you have an application which runs in **multiple instances** on a single system and therefore `Preferences` access needs to be coordinated between the JVMs on the system, then the **`sync()` method** of any `Preferences` node may be used to ensure changes to the `Preferences` node are visible to other JVMs on the system:

    // Warning: don't use this if your application is intended
    // to only run a single instance on a machine once
    // (this is probably the case for most desktop applications)
    try {
        preferences.sync();
    } catch (BackingStoreException e) {
        // Deal with any errors while saving the preferences to the backing storage
        e.printStackTrace();
    }

## Exporting preferences
`Preferences` nodes can be exported into a XML document representing that node. The resulting XML tree can be imported again. The resulting XML document will remember whether it was exported from the user or system `Preferences`.

To export a single node, but **not its child nodes**:

<!-- if version [gte Java SE 7] -->

    try (OutputStream os = ...) {
        preferences.exportNode(os);
    } catch (IOException ioe) {
        // Exception whilst writing data to the OutputStream
        ioe.printStackTrace();
    } catch (BackingStoreException bse) {
        // Exception whilst reading from the backing preferences store
        bse.printStackTrace();
    }

<!-- end version if -->

<!-- if version [lt Java SE 7] -->

    OutputStream os = null;
    try {
        os = ...;
        preferences.exportSubtree(os);
    } catch (IOException ioe) {
        // Exception whilst writing data to the OutputStream
        ioe.printStackTrace();
    } catch (BackingStoreException bse) {
        // Exception whilst reading from the backing preferences store
        bse.printStackTrace();
    } finally {
        if (os != null) {
            try {
                os.close();
            } catch (IOException ignored) {}
        }
    }

<!-- end version if -->

To export a single node **with its child nodes**:

<!-- if version [gte Java SE 7] -->

    try (OutputStream os = ...) {
        preferences.exportNode(os);
    } catch (IOException ioe) {
        // Exception whilst writing data to the OutputStream
        ioe.printStackTrace();
    } catch (BackingStoreException bse) {
        // Exception whilst reading from the backing preferences store
        bse.printStackTrace();
    }

<!-- end version if -->

<!-- if version [lt Java SE 7] -->

    OutputStream os = null;
    try {
        os = ...;
        preferences.exportSubtree(os);
    } catch (IOException ioe) {
        // Exception whilst writing data to the OutputStream
        ioe.printStackTrace();
    } catch (BackingStoreException bse) {
        // Exception whilst reading from the backing preferences store
        bse.printStackTrace();
    } finally {
        if (os != null) {
            try {
                os.close();
            } catch (IOException ignored) {}
        }
    }

<!-- end version if -->

## Importing preferences
`Preferences` nodes can be imported from a XML document. Importing is meant to be used in conjunction with the exporting functionality of `Preferences`, since it creates the correct corresponding XML documents.

The XML documents will remember whether they were exported from the user or system `Preferences`. Therefore, they can be imported into their respective `Preferences` trees again, without you having to figure out or know where they came from. The static function will automatically find out whether the XML document was exported from the user or system `Preferences` and will automatically import them into the tree they were exported from.

<!-- if version [gte Java SE 7] -->

    try (InputStream is = ...) {
        // This is a static call on the Preferences class
        Preferences.importPreferences(is);
    } catch (IOException ioe) {
        // Exception whilst reading data from the InputStream
        ioe.printStackTrace();
    } catch (InvalidPreferencesFormatException ipfe) {
        // Exception whilst parsing the XML document tree
        ipfe.printStackTrace();
    }

<!-- end version if -->

<!-- if version [lt Java SE 7] -->

    InputStream is = null;
    try {
        is = ...;
        // This is a static call on the Preferences class
        Preferences.importPreferences(is);
    } catch (IOException ioe) {
        // Exception whilst reading data from the InputStream
        ioe.printStackTrace();
    } catch (InvalidPreferencesFormatException ipfe) {
        // Exception whilst parsing the XML document tree
        ipfe.printStackTrace();
    } finally {
        if (is != null) {
            try {
                is.close();
            } catch (IOException ignored) {}
        }
    }

<!-- end version if -->

## Removing event listeners
Event listeners can be removed again from any `Properties` node, but the instance of the listener has to be kept around for that.

<!-- if version [gte Java SE 8] -->

    Preferences preferences = Preferences.userNodeForPackage(getClass());

    PreferenceChangeListener listener = evt -> {
        System.out.println(evt.getKey() + " got new value " + evt.getNewValue());
    };
    preferences.addPreferenceChangeListener(listener);

    //
    // later...
    //

    preferences.removePreferenceChangeListener(listener);

<!-- end version if -->

<!-- if version [lt Java SE 8] -->


    Preferences preferences = Preferences.userNodeForPackage(getClass());

    PreferenceChangeListener listener = new PreferenceChangeListener() {
        @Override
        public void preferenceChange(PreferenceChangeEvent evt) {
            System.out.println(evt.getKey() + " got new value " + evt.getNewValue());
        }
    };
    preferences.addPreferenceChangeListener(listener);

    //
    // later...
    //

    preferences.removePreferenceChangeListener(listener);

<!-- end version if -->

The same applies for `NodeChangeListener`.

## Getting preferences values
A value of a `Preferences` node can be of the type `String`, `boolean`, `byte[]`, `double`, `float`, `int` or `long`. All invocations must provide a default value, in case the specified value is not present in the `Preferences` node.

    Preferences preferences = Preferences.userNodeForPackage(getClass());

    String someString = preferences.get("someKey", "this is the default value");
    boolean someBoolean = preferences.getBoolean("someKey", true);
    byte[] someByteArray = preferences.getByteArray("someKey", new byte[0]);
    double someDouble = preferences.getDouble("someKey", 887284.4d);
    float someFloat = preferences.getFloat("someKey", 38723.3f);
    int someInt = preferences.getInt("someKey", 13232);
    long someLong = preferences.getLong("someKey", 2827637868234L);

## Setting preferences values
To store a value into the `Preferences` node, one of the `putXXX()` methods is used. A value of a `Preferences` node can be of the type `String`, `boolean`, `byte[]`, `double`, `float`, `int` or `long`.

    Preferences preferences = Preferences.userNodeForPackage(getClass());
    
    preferences.put("someKey", "some String value");
    preferences.putBoolean("someKey", false);
    preferences.putByteArray("someKey", new byte[0]);
    preferences.putDouble("someKey", 187398123.4454d);
    preferences.putFloat("someKey", 298321.445f);
    preferences.putInt("someKey", 77637);
    preferences.putLong("someKey", 2873984729834L);

## Using preferences
`Preferences` can be used to store user settings that reflect a user's personal application settings, e.g. their editor font, whether they prefer the application to be started in full-screen mode, whether they checked a "don't show this again" checkbox and things like that.

    public class ExitConfirmer {
        private static boolean confirmExit() {
            Preferences preferences = Preferences.userNodeForPackage(ExitConfirmer.class);
            boolean doShowDialog = preferences.getBoolean("showExitConfirmation", true); // true is default value
    
            if (!doShowDialog) {
                return true;
            }
    
            //
            // Show a dialog here...
            //
            boolean exitWasConfirmed = ...; // whether the user clicked OK or Cancel
            boolean doNotShowAgain = ...; // get value from "Do not show again" checkbox
    
            if (exitWasConfirmed && doNotShowAgain) {
                // Exit was confirmed and the user chose that the dialog should not be shown again
                // Save these settings to the Preferences object so the dialog will not show again next time
                preferences.putBoolean("showExitConfirmation", false);
            }
    
            return exitWasConfirmed;
        }
    
        public static void exit() {
            if (confirmExit()) {
                System.exit(0);
            }
        }
    }


