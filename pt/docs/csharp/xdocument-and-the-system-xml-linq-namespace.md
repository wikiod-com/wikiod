---
title: "XDocument e o namespace System.Xml.Linq"
slug: "xdocument-e-o-namespace-systemxmllinq"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Gerar um documento XML
O objetivo é gerar o seguinte documento XML:

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


## Modificar arquivo XML
Para modificar um arquivo XML com `XDocument`, carregue o arquivo em uma variável do tipo `XDocument`, modifique-o na memória e salve-o, sobrescrevendo o arquivo original.
Um erro comum é modificar o XML na memória e esperar que o arquivo no disco mude.

Dado um arquivo XML:

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

Você deseja modificar a cor da Banana para marrom:
1. Precisamos saber o caminho para o arquivo no disco.
2. Uma sobrecarga de `XDocument.Load` recebe um URI (caminho do arquivo).
3. Como o arquivo xml usa um namespace, devemos consultar com o namespace E o nome do elemento.
4. Uma consulta Linq utilizando a sintaxe C# 6 para acomodar a possibilidade de valores nulos. Cada `.` usado nesta consulta tem o potencial de retornar um conjunto nulo se a condição não encontrar elementos. Antes do C# 6, você faria isso em várias etapas, verificando se há null ao longo do caminho. O resultado é o elemento `<Fruit>` que contém a Banana. Na verdade, um `IEnumerable<XElement>`, e é por isso que o próximo passo usa `FirstOfDefault()`.
5. Agora extraímos o elemento FruitColor do elemento Fruit que acabamos de encontrar. Aqui assumimos que há apenas um, ou apenas nos importamos com o primeiro.
6. Se não for nulo, configuramos FruitColor para "Brown".
7. Por fim, salvamos o `XDocument`, sobrescrevendo o arquivo original no disco.


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

O arquivo agora está assim:

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
















## Gere um documento XML usando sintaxe fluente
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

