---
title: "XmlDocument et l'espace de noms System.Xml"
slug: "xmldocument-et-lespace-de-noms-systemxml"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## XmlDocument vs XDocument (Exemple et comparaison)
>Il existe plusieurs manières d'interagir avec un fichier Xml.
> 
> 1. Document XML
> 2. XDocument
> 3. XmlReader/XmlWriter
> 
> Avant LINQ to XML on utilisait XMLDocument pour les manipulations en XML
> comme l'ajout d'attributs, d'éléments, etc. Désormais, LINQ to XML utilise
> XDocument pour le même genre de chose. Les syntaxes sont beaucoup plus faciles que
> XMLDocument et nécessite une quantité minimale de code.
> 
> Aussi XDocument est beaucoup plus rapide que XmlDocument. XmlDocument est un ancien
> et sale solution pour interroger un document XML.
> 
> **Je vais montrer quelques exemples de [classe XmlDocument][1] et [classe XDocument][2]** :


> **Charger le fichier XML**


    
    string filename = @"C:\temp\test.xml";

**DocumentXml**

    XmlDocument _doc = new XmlDocument();
    _doc.Load(filename);

**XDocument**

    XDocument _doc = XDocument.Load(fileName);

> **Créer un document Xml**

    
**DocumentXml**

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

> **Modifier le texte intérieur du nœud en XML**

**DocumentXml**

    XmlNode node = _doc.SelectSingleNode("xmlRootNode");
    node.InnerText = value;

**XDocument**

     XElement rootNote = _doc.XPathSelectElement("xmlRootNode"); 
    rootNode.Value = "New Value";

> **Enregistrer le fichier après modification**

Assurez-vous de sécuriser le xml après toute modification.

    // Safe XmlDocument and XDocument
    _doc.Save(filename);

> **Récupérer des valeurs à partir de XML**

**DocumentXml**

     XmlNode node = _doc.SelectSingleNode("xmlRootNode/levelOneChildNode");
    string text = node.InnerText;

**XDocument**

     XElement node = _doc.XPathSelectElement("xmlRootNode/levelOneChildNode");
     string text = node.Value;

> **Récupérer la valeur de tous à partir de tous les éléments enfants où attribut = quelque chose.**

**DocumentXml**

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


> **Ajouter un nœud**

**DocumentXml**

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


[1] : https://msdn.microsoft.com/en-us/library/system.xml.xmldocument(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.xml.linq.xdocument(v=vs.110).aspx

## Lecture d'un document XML
Un exemple de fichier XML

    
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

Lecture à partir de ce fichier XML :

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

## Interaction de document XML de base
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

