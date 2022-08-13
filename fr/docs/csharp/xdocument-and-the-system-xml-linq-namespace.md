---
title: "XDocument et l'espace de noms System.Xml.Linq"
slug: "xdocument-et-lespace-de-noms-systemxmllinq"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Générer un document XML
Le but est de générer le document XML suivant :

    <FruitBasket xmlns="http://www.fruitauthority.fake">
      <Fruit ID="F0001">
        <FruitName>Banana</FruitName>
        <FruitColor>Yellow</FruitColor>
      </Fruit>
      <Fruit ID="F0002">
        <FruitName>Apple</FruitName>
        <FruitColor>Red</FruitColor>
      </Fruit>
    </FruitBasket>

Code:

    XNamespace xns = "http://www.fruitauthority.fake";
    XDeclaration xDeclaration = new XDeclaration("1.0", "utf-8", "yes");
    XDocument xDoc = new XDocument(xDeclaration);
    XElement xRoot = new XElement(xns + "FruitBasket");
    xDoc.Add(xRoot);
    
    XElement xelFruit1 = new XElement(xns + "Fruit");
    XAttribute idAttribute1 = new XAttribute("ID", "F0001");
    xelFruit1.Add(idAttribute1);
    XElement xelFruitName1 = new XElement(xns + "FruitName", "Banana");
    XElement xelFruitColor1 = new XElement(xns + "FruitColor", "Yellow");
    xelFruit1.Add(xelFruitName1);
    xelFruit1.Add(xelFruitColor1);
    xRoot.Add(xelFruit1);
    
    XElement xelFruit2 = new XElement(xns + "Fruit");
    XAttribute idAttribute2 = new XAttribute("ID", "F0002");
    xelFruit2.Add(idAttribute2);
    XElement xelFruitName2 = new XElement(xns + "FruitName", "Apple");
    XElement xelFruitColor2 = new XElement(xns + "FruitColor", "Red");
    xelFruit2.Add(xelFruitName2);
    xelFruit2.Add(xelFruitColor2);
    xRoot.Add(xelFruit2);


## Modifier le fichier XML
Pour modifier un fichier XML avec `XDocument`, chargez le fichier dans une variable de type `XDocument`, modifiez-le en mémoire, puis enregistrez-le en écrasant le fichier d'origine.
Une erreur courante consiste à modifier le XML en mémoire et à s'attendre à ce que le fichier sur le disque change.

Étant donné un fichier XML :

    <?xml version="1.0" encoding="utf-8"?>
    <FruitBasket xmlns="http://www.fruitauthority.fake">
      <Fruit>
        <FruitName>Banana</FruitName>
        <FruitColor>Yellow</FruitColor>
      </Fruit>
      <Fruit>
        <FruitName>Apple</FruitName>
        <FruitColor>Red</FruitColor>
      </Fruit>
    </FruitBasket>

Vous souhaitez modifier la couleur de la Banane en marron :
1. Nous devons connaître le chemin d'accès au fichier sur le disque.
2. Une surcharge de `XDocument.Load` reçoit un URI (chemin de fichier).
3. Étant donné que le fichier xml utilise un espace de noms, nous devons interroger avec l'espace de noms ET le nom de l'élément.
4. Une requête Linq utilisant la syntaxe C# 6 pour tenir compte de la possibilité de valeurs nulles. Chaque `.` utilisé dans cette requête a le potentiel de renvoyer un ensemble nul si la condition ne trouve aucun élément. Avant C # 6, vous le feriez en plusieurs étapes, en vérifiant null en cours de route. Le résultat est l'élément `<Fruit>` qui contient la banane. En fait un `IEnumerable<XElement>`, c'est pourquoi l'étape suivante utilise `FirstOfDefault()`.
5. Maintenant, nous extrayons l'élément FruitColor de l'élément Fruit que nous venons de trouver. Ici, nous supposons qu'il n'y en a qu'un, ou nous ne nous soucions que du premier.
6. S'il n'est pas nul, nous définissons FruitColor sur "Brown".
7. Enfin, nous sauvegardons le `XDocument`, en écrasant le fichier d'origine sur le disque.


    // 1.
    string xmlFilePath = "c:\\users\\public\\fruit.xml";
    
    // 2.
    XDocument xdoc = XDocument.Load(xmlFilePath);
    
    // 3.
    XNamespace ns = "http://www.fruitauthority.fake";
    
    //4. 
    var elBanana = xdoc.Descendants()?.
        Elements(ns + "FruitName")?.
        Where(x => x.Value == "Banana")?.
        Ancestors(ns + "Fruit");
    
    // 5.
    var elColor = elBanana.Elements(ns + "FruitColor").FirstOrDefault();
    
    // 6.
    if (elColor != null)
    {
        elColor.Value = "Brown";
    }
    
    // 7.
    xdoc.Save(xmlFilePath);

Le fichier ressemble maintenant à ceci :

    <?xml version="1.0" encoding="utf-8"?>
    <FruitBasket xmlns="http://www.fruitauthority.fake">
      <Fruit>
        <FruitName>Banana</FruitName>
        <FruitColor>Brown</FruitColor>
      </Fruit>
      <Fruit>
        <FruitName>Apple</FruitName>
        <FruitColor>Red</FruitColor>
      </Fruit>
    </FruitBasket>
















## Générer un document XML en utilisant une syntaxe fluide
Objectif:

    <FruitBasket xmlns="http://www.fruitauthority.fake">
      <Fruit>
        <FruitName>Banana</FruitName>
        <FruitColor>Yellow</FruitColor>
      </Fruit>
      <Fruit>
        <FruitName>Apple</FruitName>
        <FruitColor>Red</FruitColor>
      </Fruit>
    </FruitBasket>

Code:

    XNamespace xns = "http://www.fruitauthority.fake";
    XDocument xDoc = 
        new XDocument(new XDeclaration("1.0", "utf-8", "yes"),
            new XElement(xns + "FruitBasket",
                new XElement(xns + "Fruit",
                    new XElement(xns + "FruitName", "Banana"),
                    new XElement(xns + "FruitColor", "Yellow")),
                new XElement(xns + "Fruit",
                    new XElement(xns + "FruitName", "Apple"),
                    new XElement(xns + "FruitColor", "Red"))
                    ));

