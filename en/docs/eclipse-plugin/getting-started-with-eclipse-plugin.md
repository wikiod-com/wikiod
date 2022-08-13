---
title: "Getting started with eclipse-plugin"
slug: "getting-started-with-eclipse-plugin"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
To create a Hello World plug-in for Eclipse, click: 
*File* ➜ *New* ➜ *Other...*

[![Eclipse plug-in Selecting new project][1]][1]

Select *Plug-in Project* and click *Next >*

[![Eclipse plug-in Selecting plug-in project][2]][2]

The *New Plug-in Project* wizard will guide you through the options for creating a new plug-in.

Enter a project name (like HelloWorld), and click *Next >*

[![Eclipse plug-in New Plug-in][3]][3]

On the *Content* page, you can set the *ID*, *Version*, *Name* and *Vendor* of the plug-in.

The Version will be *1.0.0.qualifier* by default. You can leave this as-is, but it is better to change this to something meaningful. The [eclipse wiki][4] recommends a syntax like *vYYYYMMDD* (year, month day).

[![Eclipse plug-in Content][5]][5]

On the *Templates* page, you can choose to create you plug-in from any template by selecting it and clicking *Next >*. Alternatively you can combine these templates by choosing *Custom plug-in wizard*, or to create a new plug-in without a template by deselecting the checkbox in front of *Create a plug-in using one of the templates*.

[![Eclipse plug-in Template Selection][6]][6]

For the *Hello, World Command* template, there are additional settings: the package name, Handler class name and the text for the message box.

[![Eclipse plug-in Hello World settings][7]][7]

When the plug-in is created, you can run it by right-clicking the *plugin.xml* ➜ *Run As* ➜ *Eclipse Application*

This will launch a new instance of Eclipse (with its own workspace) that will have your plug-in loaded.

[![Eclipse plug-in Run As Eclipse Application][8]][8]

This *Hello World* plug-in will have made 3 contributions to the Eclipse GUI:

 **1. A Sample Menu (with Sample Command):**

[![Eclipse plug-in Menu Entry][10]][10]

Plugin.xml:

<!-- language: xml -->

    <extension
          point="org.eclipse.ui.menus">
       <menuContribution
             locationURI="menu:org.eclipse.ui.main.menu?after=additions">
          <menu
                label="Sample Menu"
                mnemonic="M"
                id="HelloWorld.menus.sampleMenu">
             <command
                   commandId="HelloWorld.commands.sampleCommand"
                   mnemonic="S"
                   id="HelloWorld.menus.sampleCommand">
             </command>
          </menu>
       </menuContribution>
    </extension>

 **2. A toolbar icon:**

[![Eclipse plug-in ToolBar Icon][9]][9]

Plugin.xml:

<!-- language: xml -->

    <extension
          point="org.eclipse.ui.menus">
       <menuContribution
             locationURI="toolbar:org.eclipse.ui.main.toolbar?after=additions">
          <toolbar
                id="HelloWorld.toolbars.sampleToolbar">
             <command
                   commandId="HelloWorld.commands.sampleCommand"
                   icon="icons/sample.gif"
                   tooltip="Say hello world"
                   id="HelloWorld.toolbars.sampleCommand">
             </command>
          </toolbar>
       </menuContribution>
    </extension>

 **3. A key shortcut (Ctrl+6)**

Plugin.xml:

<!-- language: xml -->

    <extension
          point="org.eclipse.ui.bindings">
       <key
             commandId="HelloWorld.commands.sampleCommand"
             contextId="org.eclipse.ui.contexts.window"
             sequence="M1+6"
             schemeId="org.eclipse.ui.defaultAcceleratorConfiguration">
       </key>
    </extension>


When activating any of these 3, the Handler class will be executed:

Plugin.xml:

<!-- language: xml -->

    <extension
          point="org.eclipse.ui.commands">
       <category
             name="Sample Category"
             id="HelloWorld.commands.category">
       </category>
       <command
             name="Sample Command"
             categoryId="HelloWorld.commands.category"
             id="HelloWorld.commands.sampleCommand">
       </command>
    </extension>
    <extension
          point="org.eclipse.ui.handlers">
       <handler
             commandId="HelloWorld.commands.sampleCommand"
             class="helloworld.handlers.SampleHandler">
       </handler>
    </extension>

SampleHandler.java:

<!-- language: java -->

    package helloworld.handlers;

    import org.eclipse.core.commands.AbstractHandler;
    import org.eclipse.core.commands.ExecutionEvent;
    import org.eclipse.core.commands.ExecutionException;
    import org.eclipse.ui.IWorkbenchWindow;
    import org.eclipse.ui.handlers.HandlerUtil;
    import org.eclipse.jface.dialogs.MessageDialog;

    /**
     * Our sample handler extends AbstractHandler, an IHandler base class.
     * @see org.eclipse.core.commands.IHandler
     * @see org.eclipse.core.commands.AbstractHandler
     */
    public class SampleHandler extends AbstractHandler {

        @Override
        public Object execute(ExecutionEvent event) throws ExecutionException {
            IWorkbenchWindow window = HandlerUtil.getActiveWorkbenchWindowChecked(event);
            MessageDialog.openInformation(
                    window.getShell(),
                    "HelloWorld",
                    "Hello, Eclipse world");
            return null;
        }
    }


When the Handler class is executed, MessageBox will show:

[![Eclipse plug-in Message Box][11]][11]

This is all the Hello World plug-in does.


If you want to create a plug-in with more functionality, you could have chosen a template that best fits your need or create a plug-in via the *Custom plug-in wizard* to combine these templates:

[![Eclipse plug-in Template Selection][12]][12]


  [1]: https://i.stack.imgur.com/JkKGu.png
  [2]: https://i.stack.imgur.com/xVsaN.png
  [3]: https://i.stack.imgur.com/zekJu.png
  [4]: https://wiki.eclipse.org/Version_Numbering#When_to_change_the_qualifier_segment
  [5]: https://i.stack.imgur.com/R0jHf.png
  [6]: https://i.stack.imgur.com/iZ2Ha.png
  [7]: https://i.stack.imgur.com/yby5R.png
  [8]: https://i.stack.imgur.com/NNdVA.png
  [9]: https://i.stack.imgur.com/VFCLx.png
  [10]: https://i.stack.imgur.com/efGq4.png
  [11]: https://i.stack.imgur.com/kTHZq.png
  [12]: https://i.stack.imgur.com/tM4be.png

## Installation or Setup
Assuming you have [Eclipse IDE for Java Developers][1] installed, start Eclipse, click "*Help*" -> "*Install New Software...*"

[![enter image description here][2]][2]

Select "*--All Available Sites--*" at "*Work with:*", and navigate to "*Eclipse Plugin Development Tools*".
Select "*Eclipse Plug-in Development Environment*" by ticking the checkbox in front of it.

[![enter image description here][3]][3]

Click "*Next*" to let Eclipse check for any dependencies needed. Click "*Next*" again to start the installation.

Once that has finished, restart Eclipse.


  [1]: https://www.wikiod.com/eclipse/getting-started-with-eclipse#Installation and Setup
  [2]: http://i.stack.imgur.com/yDWzj.png
  [3]: http://i.stack.imgur.com/Xjdqn.png


