---
title: "XmlDocument y el espacio de nombres System.Xml"
slug: "xmldocument-y-el-espacio-de-nombres-systemxml"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## XmlDocument vs XDocument (Ejemplo y comparación)
>Hay varias formas de interactuar con un archivo Xml.
> 
> 1. Documento XML
> 2. XDocumento
> 3. XmlReader/XmlWriter
> 
> Antes de LINQ to XML usábamos XMLDocument para manipulaciones en XML
> como agregar atributos, elementos y demás. Ahora utiliza LINQ to XML
> XDocument para el mismo tipo de cosas. Las sintaxis son mucho más fáciles que
> XMLDocument y requiere una cantidad mínima de código.
> 
> También XDocument es mucho más rápido que XmlDocument. XmlDocument es un viejo
> y solución sucia para consultar un documento XML.
> 
> **Voy a mostrar algunos ejemplos de [clase XmlDocument][1] y [clase XDocument][2]** clase:


> **Cargar archivo XML**


    
    string filename = @"C:\temp\test.xml";

**Documento Xml**

    XmlDocument _doc = new XmlDocument();
    _doc.Load(filename);

**XDocumento**

    XDocument _doc = XDocument.Load(fileName);

> **Crear documento Xml**

    
**Documento Xml**

    XmlDocument doc = new XmlDocument();
    XmlElement root = doc.CreateElement("root");
    root.SetAttribute("name", "value");
    XmlElement child = doc.CreateElement("child");
    child.InnerText = "text node";
    root.AppendChild(child);
    doc.AppendChild(root);

**XDocumento**

     XDocument doc = new XDocument(
        new XElement("Root", new XAttribute("name", "value"), 
        new XElement("Child", "text node"))
    );

    /*result*/
    <root name="value">
        <child>"TextNode"</child>
    </root>

> **Cambiar texto interno del nodo en XML**

**Documento Xml**

    XmlNode node = _doc.SelectSingleNode("xmlRootNode");
    node.InnerText = value;

**XDocumento**

     XElement rootNote = _doc.XPathSelectElement("xmlRootNode"); 
    rootNode.Value = "New Value";

> **Guardar archivo después de editar**

Asegúrese de proteger el xml después de cualquier cambio.

    // Safe XmlDocument and XDocument
    _doc.Save(filename);

> **Recuperar valores de XML**

**Documento Xml**

     XmlNode node = _doc.SelectSingleNode("xmlRootNode/levelOneChildNode");
    string text = node.InnerText;

**XDocumento**

     XElement node = _doc.XPathSelectElement("xmlRootNode/levelOneChildNode");
     string text = node.Value;

> **Recuperar valor de todos los elementos secundarios donde atributo = algo.**

**Documento Xml**

    List<string> valueList = new List<string>(); 
        foreach (XmlNode n in nodelist)
        {
            if(n.Attributes["type"].InnerText == "City")
            {
                valueList.Add(n.Attributes["type"].InnerText);
            }
        }

**XDocumento**

    var accounts = _doc.XPathSelectElements("/data/summary/account").Where(c => c.Attribute("type").Value == "setting").Select(c => c.Value);


> **Agregar un nodo**

**Documento Xml**

    XmlNode nodeToAppend = doc.CreateElement("SecondLevelNode");
    nodeToAppend.InnerText = "This title is created by code";
    
    /* Append node to parent */
    XmlNode firstNode= _doc.SelectSingleNode("xmlRootNode/levelOneChildNode");
    firstNode.AppendChild(nodeToAppend);

    /*After a change make sure to safe the document*/
    _doc.Save(fileName);

    
**XDocumento**

    _doc.XPathSelectElement("ServerManagerSettings/TcpSocket").Add(new XElement("SecondLevelNode"));

     /*After a change make sure to safe the document*/
    _doc.Save(fileName); 


[1]: https://msdn.microsoft.com/en-us/library/system.xml.xmldocument(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.xml.linq.xdocument(v=vs.110).aspx

## Lectura del documento XML
Un archivo XML de ejemplo

    
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

Lectura de este archivo XML:

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

## Interacción básica de documentos XML
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

