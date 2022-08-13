---
title: "XDocument y el espacio de nombres System.Xml.Linq"
slug: "xdocument-y-el-espacio-de-nombres-systemxmllinq"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Generar un documento XML
El objetivo es generar el siguiente documento XML:

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

Código:

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


## Modificar archivo XML
Para modificar un archivo XML con `XDocument`, carga el archivo en una variable de tipo `XDocument`, lo modifica en la memoria y luego lo guarda, sobrescribiendo el archivo original.
Un error común es modificar el XML en la memoria y esperar que cambie el archivo en el disco.

Dado un archivo XML:

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

Desea modificar el color de Banana a marrón:
1. Necesitamos saber la ruta al archivo en el disco.
2. Una sobrecarga de `XDocument.Load` recibe un URI (ruta del archivo).
3. Dado que el archivo xml usa un espacio de nombres, debemos consultar con el espacio de nombres Y el nombre del elemento.
4. Una consulta de Linq que utiliza la sintaxis de C# 6 para adaptarse a la posibilidad de valores nulos. Cada `.` utilizado en esta consulta tiene el potencial de devolver un conjunto nulo si la condición no encuentra elementos. Antes de C# 6, haría esto en varios pasos, verificando si hay valores nulos en el camino. El resultado es el elemento `<Fruit>` que contiene Banana. En realidad, un `IEnumerable<XElement>`, por lo que el siguiente paso usa `FirstOfDefault()`.
5. Ahora extraemos el elemento FruitColor del elemento Fruit que acabamos de encontrar. Aquí asumimos que solo hay uno, o solo nos importa el primero.
6. Si no es nulo, establecemos FruitColor en "Brown".
7. Finalmente, guardamos el `XDocument`, sobrescribiendo el archivo original en el disco.


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

El archivo ahora se ve así:

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
















## Genere un documento XML utilizando una sintaxis fluida
Meta:

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

Código:

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

