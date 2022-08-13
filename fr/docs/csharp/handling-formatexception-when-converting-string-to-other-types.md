---
title: "Gestion de FormatException lors de la conversion de chaîne en d'autres types"
slug: "gestion-de-formatexception-lors-de-la-conversion-de-chaine-en-dautres-types"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Conversion d'une chaîne en entier
Il existe différentes méthodes disponibles pour convertir explicitement une "chaîne" en un "entier", telles que :
1. `Convert.ToInt16();`

2. `Convert.ToInt32();`

3. `Convert.ToInt64();`

4. `int.Parse();`

Mais toutes ces méthodes lèveront une `FormatException`, si la chaîne d'entrée contient des caractères non numériques. Pour cela, nous devons écrire une gestion supplémentaire des exceptions (`try..catch`) pour les traiter dans de tels cas.

<hr/>
 
**Explication avec exemples :**

Alors, laissez notre entrée être:

    string inputString = "10.2";


**Exemple 1 :** `Convert.ToInt32()`

    int convertedInt = Convert.ToInt32(inputString); // Failed to Convert 
    // Throws an Exception "Input string was not in a correct format."

***Remarque :** Il en va de même pour les autres méthodes mentionnées, à savoir - `Convert.ToInt16();` et `Convert.ToInt64();`*
 

**Exemple 2 :** `int.Parse()`

    int convertedInt = int.Parse(inputString); // Same result "Input string was not in a correct format.

***Comment pouvons-nous contourner cela ?***

Comme indiqué précédemment, pour gérer les exceptions, nous avons généralement besoin d'un "try..catch" comme indiqué ci-dessous :

    try
    {
        string inputString = "10.2";
        int convertedInt = int.Parse(inputString);
    }
    catch (Exception Ex)
    {
        //Display some message, that the conversion has failed.         
    }
Mais, utiliser le `try..catch` partout ne sera pas une bonne pratique, et il peut y avoir des scénarios où nous voulions donner `0` si l'entrée est erronée, _(Si nous suivons la méthode ci-dessus, nous devons attribuer `0` en `convertedInt` à partir du bloc catch)._
Pour gérer de tels scénarios, nous pouvons utiliser une méthode spéciale appelée `.TryParse()`.

La méthode `.TryParse()` ayant une gestion interne des exceptions, qui vous donnera la sortie du paramètre `out` et renvoie une valeur booléenne indiquant l'état de la conversion _(`true` si la conversion a réussi ; `false` en cas d'échec)._ Sur la base de la valeur de retour, nous pouvons déterminer l'état de la conversion. Voyons un exemple :

**Utilisation 1 :** Stocker la valeur de retour dans une variable booléenne

     int convertedInt; // Be the required integer
     bool isSuccessConversion = int.TryParse(inputString, out convertedInt);
Nous pouvons vérifier la variable `isSuccessConversion` après l'exécution pour vérifier l'état de la conversion. S'il est faux, la valeur de `convertedInt` sera `0` _ (pas besoin de vérifier la valeur de retour si vous voulez `0` pour l'échec de la conversion)._

**Utilisation 2 :** Vérifiez la valeur de retour avec "if"

    if (int.TryParse(inputString, out convertedInt))
    {
        // convertedInt will have the converted value
        // Proceed with that
    }
    else 
    {
     // Display an error message
    }
**Utilisation 3 :** Sans vérifier la valeur de retour
vous pouvez utiliser ce qui suit, si vous ne vous souciez pas de la valeur de retour _(convertie ou non, `0` sera ok)_

    int.TryParse(inputString, out convertedInt);
    // use the value of convertedInt
    // But it will be 0 if not converted

