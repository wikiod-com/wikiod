---
title: "Drag and Drop"
slug: "drag-and-drop"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

wxPython provides several different kinds of drag and drop. You can have one of the following types: `wx.FileDropTarget`, `wx.TextDropTarget`, or `wx.PyDropTarget`.

The first two are pretty self-explanatory. The last one, wx.PyDropTarget, is just a loose wrapper around wx.DropTarget itself. It adds a couple extra convenience methods that the plain wx.DropTarget doesn't have. We'll start with a wx.FileDropTarget example. 

## FileDropTarget
    import wx
    
    
    class MyFileDropTarget(wx.FileDropTarget):
        """"""
    
        def __init__(self, window):
            """Constructor"""
            wx.FileDropTarget.__init__(self)
            self.window = window
    
        def OnDropFiles(self, x, y, filenames):
            """
            When files are dropped, write where they were dropped and then
            the file paths themselves
            """
            self.window.SetInsertionPointEnd()
            self.window.updateText("\n%d file(s) dropped at %d,%d:\n" %
                                   (len(filenames), x, y))
            for filepath in filenames:
                self.window.updateText(filepath + '\n')  
    
            return True  
    
    
    class DnDPanel(wx.Panel):
        """"""
    
        def __init__(self, parent):
            """Constructor"""
            wx.Panel.__init__(self, parent=parent)
    
            file_drop_target = MyFileDropTarget(self)
            lbl = wx.StaticText(self, label="Drag some files here:")
            self.fileTextCtrl = wx.TextCtrl(self,
                                            style=wx.TE_MULTILINE|wx.HSCROLL|wx.TE_READONLY)
            self.fileTextCtrl.SetDropTarget(file_drop_target)
    
            sizer = wx.BoxSizer(wx.VERTICAL)
            sizer.Add(lbl, 0, wx.ALL, 5)
            sizer.Add(self.fileTextCtrl, 1, wx.EXPAND|wx.ALL, 5)
            self.SetSizer(sizer)
    
        def SetInsertionPointEnd(self):
            """
            Put insertion point at end of text control to prevent overwriting
            """
            self.fileTextCtrl.SetInsertionPointEnd()
    
        def updateText(self, text):
            """
            Write text to the text control
            """
            self.fileTextCtrl.WriteText(text)
    
    
    class DnDFrame(wx.Frame):
        """"""
    
        def __init__(self):
            """Constructor"""
            wx.Frame.__init__(self, parent=None, title="DnD Tutorial")
            panel = DnDPanel(self)
            self.Show()
    
    
    if __name__ == "__main__":
        app = wx.App(False)
        frame = DnDFrame()
        app.MainLoop()

## TextDropTarget
    import wx
    
    
    class MyTextDropTarget(wx.TextDropTarget):
    
        def __init__(self, textctrl):
            wx.TextDropTarget.__init__(self)
            self.textctrl = textctrl
    
        def OnDropText(self, x, y, text):
            self.textctrl.WriteText("(%d, %d)\n%s\n" % (x, y, text))
            return True
    
        def OnDragOver(self, x, y, d):
            return wx.DragCopy
    
    
    class DnDPanel(wx.Panel):
        """"""
    
        def __init__(self, parent):
            """Constructor"""
            wx.Panel.__init__(self, parent=parent)
    
    
            lbl = wx.StaticText(self, label="Drag some text here:")
            self.myTextCtrl = wx.TextCtrl(
                self, style=wx.TE_MULTILINE|wx.HSCROLL|wx.TE_READONLY)
            text_dt = MyTextDropTarget(self.myTextCtrl)
            self.myTextCtrl.SetDropTarget(text_dt)
    
            sizer = wx.BoxSizer(wx.VERTICAL)
            sizer.Add(self.myTextCtrl, 1, wx.EXPAND)
            self.SetSizer(sizer)
    
        def WriteText(self, text):
            self.text.WriteText(text)
    
    
    class DnDFrame(wx.Frame):
        """"""
    
        def __init__(self):
            """Constructor"""
            wx.Frame.__init__(
                self, parent=None, title="DnD Text Tutorial")
            panel = DnDPanel(self)
            self.Show()
    
    
    if __name__ == "__main__":
        app = wx.App(False)
        frame = DnDFrame()
        app.MainLoop()

## PyDropTarget
    import  wx
    
    
    class MyURLDropTarget(wx.PyDropTarget):
    
        def __init__(self, window):
            wx.PyDropTarget.__init__(self)
            self.window = window
    
            self.data = wx.URLDataObject();
            self.SetDataObject(self.data)
    
        def OnDragOver(self, x, y, d):
            return wx.DragLink
    
        def OnData(self, x, y, d):
            if not self.GetData():
                return wx.DragNone
    
            url = self.data.GetURL()
            self.window.AppendText(url + "\n")
    
            return d
    
    
    class DnDPanel(wx.Panel):
        """"""
    
        def __init__(self, parent):
            """Constructor"""
            wx.Panel.__init__(self, parent=parent)
            font = wx.Font(12, wx.SWISS, wx.NORMAL, wx.BOLD, False)
    
            # create and setup first set of widgets
            lbl = wx.StaticText(self, 
                                label="Drag some URLS from your browser here:")
            lbl.SetFont(font)
            self.dropText = wx.TextCtrl(
                self, size=(200,200), 
                style=wx.TE_MULTILINE|wx.HSCROLL|wx.TE_READONLY)
            dt = MyURLDropTarget(self.dropText)
            self.dropText.SetDropTarget(dt)
            firstSizer = self.addWidgetsToSizer([lbl, self.dropText])
    
            # create and setup second set of widgets
            lbl = wx.StaticText(self, label="Drag this URL to your browser:")
            lbl.SetFont(font)
            self.draggableURLText = wx.TextCtrl(self, 
                                                value="http://www.mousevspython.com")
            self.draggableURLText.Bind(wx.EVT_MOTION, self.OnStartDrag)
            secondSizer = self.addWidgetsToSizer([lbl, self.draggableURLText])
    
            # Add sizers to main sizer
            mainSizer = wx.BoxSizer(wx.VERTICAL)
            mainSizer.Add(firstSizer, 0, wx.EXPAND)
            mainSizer.Add(secondSizer, 0, wx.EXPAND)
            self.SetSizer(mainSizer)
    
        def addWidgetsToSizer(self, widgets):
            """
            Returns a sizer full of widgets
            """
            sizer = wx.BoxSizer(wx.HORIZONTAL)
            for widget in widgets:
                if isinstance(widget, wx.TextCtrl):
                    sizer.Add(widget, 1, wx.EXPAND|wx.ALL, 5)
                else:
                    sizer.Add(widget, 0, wx.ALL, 5)
            return sizer
    
        def OnStartDrag(self, evt):
            """"""
            if evt.Dragging():
                url = self.draggableURLText.GetValue()
                data = wx.URLDataObject()
                data.SetURL(url)
    
                dropSource = wx.DropSource(self.draggableURLText)
                dropSource.SetData(data)
                result = dropSource.DoDragDrop()
    
    
    class DnDFrame(wx.Frame):
        """"""
    
        def __init__(self):
            """Constructor"""
            wx.Frame.__init__(self, parent=None, 
                              title="DnD URL Tutorial", size=(800,600))
            panel = DnDPanel(self)
            self.Show()
    
    
    if __name__ == "__main__":
        app = wx.App(False)
        frame = DnDFrame()
        app.MainLoop()

