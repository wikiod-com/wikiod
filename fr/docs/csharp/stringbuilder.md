---
title: "Générateur de chaînes"
slug: "generateur-de-chaines"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Qu'est-ce qu'un StringBuilder et quand en utiliser un
Un [`StringBuilder`][1] représente une série de caractères qui, contrairement à une chaîne normale, sont modifiables. Souvent, il est nécessaire de modifier les chaînes que nous avons déjà créées, mais l'objet chaîne standard n'est pas modifiable. Cela signifie que chaque fois qu'une chaîne est modifiée, un nouvel objet chaîne doit être créé, copié puis réaffecté.

    string myString = "Apples";
    mystring += " are my favorite fruit";

Dans l'exemple ci-dessus, `myString` n'a initialement que la valeur `"Apples"`. Cependant, lorsque nous concaténons `" sont mon fruit préféré"', ce que la classe de chaîne doit faire en interne implique :

- Création d'un nouveau tableau de caractères égal à la longueur de `myString` et de la nouvelle chaîne que nous ajoutons.
- Copier tous les caractères de `myString` au début de notre nouveau tableau et copier la nouvelle chaîne à la fin du tableau.
- Créez un nouvel objet chaîne en mémoire et réaffectez-le à `myString`.

Pour une seule concaténation, c'est relativement trivial. Cependant, que se passe-t-il si nécessaire pour effectuer de nombreuses opérations d'ajout, par exemple, dans une boucle ?

    String myString = "";
    for (int i = 0; i < 10000; i++)
        myString += " "; // puts 10,000 spaces into our string

En raison de la copie répétée et de la création d'objets, cela dégradera considérablement les performances de notre programme. Nous pouvons éviter cela en utilisant à la place un `StringBuilder`.

    StringBuilder myStringBuilder = new StringBuilder();    
    for (int i = 0; i < 10000; i++)
        myStringBuilder.Append(' ');

Désormais, lorsque la même boucle est exécutée, les performances et la vitesse d'exécution du programme seront nettement plus rapides qu'avec une chaîne normale. Pour transformer le `StringBuilder` en une chaîne normale, nous pouvons simplement appeler la méthode `ToString()` de `StringBuilder`.


----------
Cependant, ce n'est pas la seule optimisation de `StringBuilder`. Afin d'optimiser davantage les fonctions, nous pouvons tirer parti d'autres propriétés qui contribuent à améliorer les performances.

    StringBuilder sb = new StringBuilder(10000); // initializes the capacity to 10000

Si nous savons à l'avance combien de temps notre `StringBuilder` doit être, nous pouvons spécifier sa taille à l'avance, ce qui lui évitera d'avoir à redimensionner le tableau de caractères qu'il possède en interne.

    sb.Append('k', 2000);

Bien que l'utilisation de `StringBuilder` pour l'ajout soit beaucoup plus rapide qu'une chaîne, elle peut s'exécuter encore plus rapidement si vous n'avez besoin d'ajouter qu'un seul caractère plusieurs fois.

Une fois que vous avez terminé la construction de votre chaîne, vous pouvez utiliser la méthode `ToString()` sur le `StringBuilder` pour la convertir en une `chaîne` de base. Ceci est souvent nécessaire car la classe `StringBuilder` n'hérite pas de `string`.

Par exemple, voici comment vous pouvez utiliser un `StringBuilder` pour créer une `string` :

    string RepeatCharacterTimes(char character, int times)
    {
        StringBuilder builder = new StringBuilder("");
        for (int counter = 0; counter < times; counter++)
        {
            //Append one instance of the character to the StringBuilder.
            builder.Append(character);
        }
        //Convert the result to string and return it.
        return builder.ToString();
    }

----------
En conclusion, `StringBuilder` doit être utilisé à la place de string lorsque de nombreuses modifications doivent être apportées à une chaîne en tenant compte des performances.


[1] : https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

## Utilisez StringBuilder pour créer une chaîne à partir d'un grand nombre d'enregistrements
    public string GetCustomerNamesCsv()
    {
        List<CustomerData> customerDataRecords = GetCustomerData(); // Returns a large number of records, say, 10000+
    
        StringBuilder customerNamesCsv = new StringBuilder();
        foreach (CustomerData record in customerDataRecords)
        {
           customerNamesCsv
               .Append(record.LastName)
               .Append(',')
               .Append(record.FirstName)
               .Append(Environment.NewLine);
        }

        return customerNamesCsv.ToString();
    }


