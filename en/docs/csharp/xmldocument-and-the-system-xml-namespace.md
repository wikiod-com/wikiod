---
title: "XmlDocument and the System.Xml namespace"
slug: "xmldocument-and-the-systemxml-namespace"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## XmlDocument vs XDocument (Example and comparison)
>There are several ways interact with an Xml file.
> 
>  1. Xml Document
>  2. XDocument
>  3. XmlReader/XmlWriter
> 
> Before LINQ to XML we were used XMLDocument for manipulations in XML
> like adding attributes, elements and so on. Now LINQ to XML uses
> XDocument for the same kind of thing. Syntaxes are much easier than
> XMLDocument and it requires a minimal amount of code.
> 
> Also XDocument is mutch faster as XmlDocument. XmlDoucument is an old
> and dirty solution for query an XML document.
> 
> **I am going to show some examples of [XmlDocument class][1] and [XDocument class][2]** class: 


> **Load XML file**


    
    string filename = @"C:\temp\test.xml";

**XmlDocument**

    XmlDocument _doc = new XmlDocument();
    _doc.Load(filename);

**XDocument**

    XDocument _doc = XDocument.Load(fileName);

> **Create XmlDocument**

    
**XmlDocument**

    XmlDocument doc = new XmlDocument();
    XmlElement root = doc.CreateElement("root");
    root.SetAttribute("name", "value");
    XmlElement child = doc.CreateElement("child");
    child.InnerText = "text node";
    root.AppendChild(child);
    doc.AppendChild(root);

**XDocument**

     XDocument doc = new XDocument(
        new XElement("Root", new XAttribute("name", "value"), 
        new XElement("Child", "text node"))
    );

    /*result*/
    <root name="value">
        <child>"TextNode"</child>
    </root>

> **Change InnerText of node in XML**

**XmlDocument**

    XmlNode node = _doc.SelectSingleNode("xmlRootNode");
    node.InnerText = value;

**XDocument**

     XElement rootNote = _doc.XPathSelectElement("xmlRootNode"); 
    rootNode.Value = "New Value";

> **Save File after edit**

Make sure to safe the xml after any change.

    // Safe XmlDocument and XDocument
    _doc.Save(filename);

> **Retreive Values from XML**

**XmlDocument**

     XmlNode node = _doc.SelectSingleNode("xmlRootNode/levelOneChildNode");
    string text = node.InnerText;

**XDocument**

     XElement node = _doc.XPathSelectElement("xmlRootNode/levelOneChildNode");
     string text = node.Value;

> **Retreive value from all from all child elements where attribute = something.**

**XmlDocument**

    List<string> valueList = new List<string>(); 
        foreach (XmlNode n in nodelist)
        {
            if(n.Attributes["type"].InnerText == "City")
            {
                valueList.Add(n.Attributes["type"].InnerText);
            }
        }

**XDocument**

    var accounts = _doc.XPathSelectElements("/data/summary/account").Where(c => c.Attribute("type").Value == "setting").Select(c => c.Value);


> **Append a node**

**XmlDocument**

    XmlNode nodeToAppend = doc.CreateElement("SecondLevelNode");
    nodeToAppend.InnerText = "This title is created by code";
    
    /* Append node to parent */
    XmlNode firstNode= _doc.SelectSingleNode("xmlRootNode/levelOneChildNode");
    firstNode.AppendChild(nodeToAppend);

    /*After a change make sure to safe the document*/
    _doc.Save(fileName);

    
**XDocument**

    _doc.XPathSelectElement("ServerManagerSettings/TcpSocket").Add(new XElement("SecondLevelNode"));

     /*After a change make sure to safe the document*/
    _doc.Save(fileName); 


  [1]: https://msdn.microsoft.com/en-us/library/system.xml.xmldocument(v=vs.110).aspx
  [2]: https://msdn.microsoft.com/en-us/library/system.xml.linq.xdocument(v=vs.110).aspx

## Reading from XML document
An example XML file

    
        <Sample>
        <Account>
            <One number="12"/>
            <Two number="14"/>
        </Account>
        <Account>
            <One number="14"/>
            <Two number="16"/>
        </Account>
        </Sample>

Reading from this XML file:

        using System.Xml;
        using System.Collections.Generic;
        
        public static void Main(string fullpath)
        {
            var xmldoc = new XmlDocument();
            xmldoc.Load(fullpath);
            
            var oneValues = new List<string>();
            
            // Getting all XML nodes with the tag name
            var accountNodes = xmldoc.GetElementsByTagName("Account");
            for (var i = 0; i < accountNodes.Count; i++)
            {
                // Use Xpath to find a node
                var account = accountNodes[i].SelectSingleNode("./One");
                if (account != null && account.Attributes != null)
                {
                    // Read node attribute
                    oneValues.Add(account.Attributes["number"].Value);
                }
            }
    }

## Basic XML document interaction
    public static void Main()
    {
        var xml  = new XmlDocument();
        var root = xml.CreateElement("element");
            // Creates an attribute, so the element will now be "<element attribute='value' />"
            root.SetAttribute("attribute", "value");

        // All XML documents must have one, and only one, root element        
        xml.AppendChild(root);

        // Adding data to an XML document
        foreach (var dayOfWeek in Enum.GetNames((typeof(DayOfWeek))))
        {
            var day = xml.CreateElement("dayOfWeek");
                day.SetAttribute("name", dayOfWeek);

            // Don't forget to add the new value to the current document!
            root.AppendChild(day);
        }

        // Looking for data using XPath; BEWARE, this is case-sensitive
        var monday = xml.SelectSingleNode("//dayOfWeek[@name='Monday']");
        if (monday != null)
        {
            // Once you got a reference to a particular node, you can delete it
            // by navigating through its parent node and asking for removal
            monday.ParentNode.RemoveChild(monday);
        }
            
        // Displays the XML document in the screen; optionally can be saved to a file
        xml.Save(Console.Out);
    }

