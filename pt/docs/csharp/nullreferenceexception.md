---
title: "Exceção de Referência Nula"
slug: "excecao-de-referencia-nula"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## NullReferenceException explicado
Uma `NullReferenceException` é lançada quando você tenta acessar um membro não estático (propriedade, método, campo ou evento) de um objeto de referência, mas é nulo.

    Car myFirstCar = new Car();
    Car mySecondCar = null;
    Color myFirstColor = myFirstCar.Color; // No problem as myFirstCar exists / is not null
    Color mySecondColor = mySecondCar.Color; // Throws a NullReferenceException 
    // as mySecondCar is null and yet we try to access its color.

Para depurar tal exceção, é bem fácil: na linha onde a exceção é lançada, você só tem que olhar antes de cada '`.`' ou '`[`', ou em raras ocasiões '`(`'.

    myGarage.CarCollection[currentIndex.Value].Color = theCarInTheStreet.Color;

De onde vem minha exceção?
Qualquer:

- `myGarage` é `null`
- `myGarage.CarCollection` é `null`
- `currentIndex` é `null`
- `myGarage.CarCollection[currentIndex.Value]` é `null`
- `theCarInTheStreet` é `null`

No modo de depuração, você só precisa colocar o cursor do mouse em cada um desses elementos e encontrará sua referência nula. Então, o que você tem que fazer é entender porque não tem valor. A correção depende totalmente do objetivo do seu método.

Você esqueceu de instanciar/inicializar?

    myGarage.CarCollection = new Car[10];

Você deveria fazer algo diferente se o objeto for nulo?

    if (myGarage == null)
    {
        Console.WriteLine("Maybe you should buy a garage first!");
    }

Ou talvez alguém tenha lhe dado um argumento nulo e não deveria:

    if (theCarInTheStreet == null)
    {
        throw new ArgumentNullException("theCarInTheStreet");
    }
De qualquer forma, lembre-se de que um método nunca deve lançar uma NullReferenceException. Se isso acontecer, isso significa que você esqueceu de verificar alguma coisa.

