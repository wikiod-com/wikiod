---
title: "XmlDocument e o namespace System.Xml"
slug: "xmldocument-e-o-namespace-systemxml"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## XmlDocument vs XDocument (Exemplo e comparação)
>Existem várias maneiras de interagir com um arquivo Xml.
> 
> 1. Documento XML
> 2. XDocumento
> 3. XmlReader/XmlWriter
> 
> Antes do LINQ to XML usávamos XMLDocument para manipulações em XML
> como adicionar atributos, elementos e assim por diante. Agora LINQ to XML usa
> XDocument para o mesmo tipo de coisa. Sintaxes são muito mais fáceis do que
> XMLDocument e requer uma quantidade mínima de código.
> 
> Também o XDocument é muito mais rápido que o XmlDocument. XmlDocument é um antigo
> e solução suja para consultar um documento XML.
> 
> **Vou mostrar alguns exemplos da classe [XmlDocument class][1] e [XDocument class][2]**:


> **Carregar arquivo XML**


    
    string filename = @"C:\temp\test.xml";

**XmlDocument**

    XmlDocument _doc = new XmlDocument();
    _doc.Load(filename);

**XDocumento**

    XDocument _doc = XDocument.Load(fileName);

> **Criar documento Xml**

    
**XmlDocument**

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

> **Alterar InnerText do nó em XML**

**XmlDocument**

    XmlNode node = _doc.SelectSingleNode("xmlRootNode");
    node.InnerText = value;

**XDocumento**

     XElement rootNote = _doc.XPathSelectElement("xmlRootNode"); 
    rootNode.Value = "New Value";

> **Salvar arquivo após editar**

Certifique-se de proteger o xml após qualquer alteração.

    // Safe XmlDocument and XDocument
    _doc.Save(filename);

> **Recuperar valores de XML**

**XmlDocument**

     XmlNode node = _doc.SelectSingleNode("xmlRootNode/levelOneChildNode");
    string text = node.InnerText;

**XDocumento**

     XElement node = _doc.XPathSelectElement("xmlRootNode/levelOneChildNode");
     string text = node.Value;

> **Recupere o valor de todos de todos os elementos filho em que atributo = algo.**

**XmlDocument**

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


> **Anexar um nó**

**XmlDocument**

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

## Lendo do documento XML
Um exemplo de arquivo XML

    
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

Lendo deste arquivo XML:

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

## Interação básica do documento XML
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

