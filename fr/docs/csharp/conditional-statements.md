---
title: "Expressions conditionnelles"
slug: "expressions-conditionnelles"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Instruction Si-Sinon
La programmation en général nécessite souvent une « décision » ou une « branche » dans le code pour tenir compte de la façon dont le code fonctionne sous différentes entrées ou conditions. Dans le langage de programmation C# (et la plupart des langages de programmation d'ailleurs), le moyen le plus simple et parfois le plus utile de créer une branche dans votre programme consiste à utiliser une instruction "If-Else".

Supposons que nous ayons une méthode (alias une fonction) qui prend un paramètre int qui représentera un score jusqu'à 100, et la méthode affichera un message indiquant si nous réussissons ou échouons.

    static void PrintPassOrFail(int score)
    {
        if (score >= 50) // If score is greater or equal to 50
        {
            Console.WriteLine("Pass!");
        }
        else // If score is not greater or equal to 50
        {
            Console.WriteLine("Fail!");
        }
    }

En regardant cette méthode, vous remarquerez peut-être cette ligne de code (`score >= 50`) à l'intérieur de l'instruction `If`. Cela peut être vu comme une condition `booléenne`, où si la condition est évaluée comme étant égale à `true`, alors le code qui se trouve entre les `if` `{ }` est exécuté.

Par exemple, si cette méthode était appelée comme ceci :
`PrintPassOrFail(60);`, la sortie de la méthode serait une impression de console disant ***Pass!*** puisque la valeur du paramètre de 60 est supérieure ou égale à 50.

Cependant, si la méthode était appelée comme : `PrintPassOrFail(30);`, la sortie de la méthode afficherait ***Fail!***. C'est parce que la valeur 30 n'est pas supérieure ou égale à 50, donc le code entre le `else` `{ }` est exécuté à la place de l'instruction `If`.

Dans cet exemple, nous avons dit que le *score* devrait aller jusqu'à 100, ce qui n'a pas du tout été pris en compte. Pour tenir compte du fait que le *score* ne dépasse pas 100 ou chute éventuellement en dessous de 0, consultez l'exemple **If-Else If-Else Statement**.

## If-Else If-Else Statement
Suite à l'exemple **If-Else Statement**, il est maintenant temps d'introduire l'instruction `Else If`. L'instruction `Else If` suit directement l'instruction `If` dans la structure **If-Else If-Else**, mais a intrinsèquement une syntaxe similaire à l'instruction `If`. Il est utilisé pour ajouter plus de branches au code que ce qu'une simple instruction **If-Else** peut faire.

Dans l'exemple de **If-Else Statement**, l'exemple spécifiait que le score allait jusqu'à 100 ; cependant, il n'y a jamais eu de contrôle contre cela. Pour résoudre ce problème, modifions la méthode de **If-Else Statement** pour qu'elle ressemble à ceci :

    static void PrintPassOrFail(int score)
    {
        if (score > 100) // If score is greater than 100
        {
            Console.WriteLine("Error: score is greater than 100!");
        }
        else if (score < 0) // Else If score is less than 0
        {
            Console.WriteLine("Error: score is less than 0!");
        }
        else if (score >= 50) // Else if score is greater or equal to 50
        {
            Console.WriteLine("Pass!");
        }
        else // If none above, then score must be between 0 and 49
        {
            Console.WriteLine("Fail!");
        }
    }

Toutes ces instructions s'exécuteront dans l'ordre du haut vers le bas jusqu'à ce qu'une condition soit remplie. Dans cette nouvelle mise à jour de la méthode, nous avons ajouté deux nouvelles branches pour désormais s'adapter au score allant *hors limites*.

Par exemple, si nous appelions maintenant la méthode dans notre code en tant que `PrintPassOFail(110);`, la sortie serait une impression de console indiquant ***Erreur : le score est supérieur à 100 !*** ; et si nous appelions la méthode dans notre code comme `PrintPassOrFail(-20);`, la sortie indiquerait ***Erreur : le score est inférieur à 0 !***.

## Si les conditions de l'instruction sont des expressions et des valeurs booléennes standard
La déclaration suivante

    if (conditionA && conditionB && conditionC) //...
est exactement équivalent à

    bool conditions = conditionA && conditionB && conditionC;
    if (conditions) // ...
en d'autres termes, les conditions à l'intérieur de l'instruction "if" forment simplement une expression booléenne ordinaire.

Une erreur courante lors de l'écriture d'instructions conditionnelles consiste à comparer explicitement `true` et `false` :

    if (conditionA == true && conditionB == false && conditionC == true) // ...

Cela peut être réécrit comme

    if (conditionA && !conditionB && conditionC)

## Instructions de commutation
Une instruction switch permet de tester l'égalité d'une variable par rapport à une liste de valeurs. Chaque valeur est appelée un cas et la variable activée est vérifiée pour chaque cas de commutation.

Une instruction `switch` est souvent plus concise et compréhensible que les instructions `if...else if... else..` lors du test de plusieurs valeurs possibles pour une seule variable.

    
La syntaxe est la suivante

    switch(expression) {
       case constant-expression:
          statement(s);
          break;
       case constant-expression:
          statement(s);
          break;
      
       // you can have any number of case statements
       default : // Optional
          statement(s);
          break;
    }

il y a plusieurs choses à considérer lors de l'utilisation de l'instruction switch

- L'expression utilisée dans une instruction switch doit avoir un type intégral ou énuméré, ou être d'un type de classe dans lequel la classe a une seule fonction de conversion en un type intégral ou énuméré.
- Vous pouvez avoir n'importe quel nombre d'instructions de cas dans un commutateur. Chaque cas est suivi de la valeur à comparer et de deux-points. Les valeurs à comparer doivent être uniques dans chaque instruction switch.
- Une instruction switch peut avoir une casse par défaut facultative. Le cas par défaut peut être utilisé pour effectuer une tâche lorsqu'aucun des cas n'est vrai.
- Chaque cas doit se terminer par une instruction `break` à moins qu'il ne s'agisse d'une instruction vide. Dans ce cas, l'exécution se poursuivra dans la case inférieure. L'instruction break peut également être omise lorsqu'une instruction `return`, `throw` ou `goto case` est utilisée.


Un exemple peut être donné avec les notes sages

    char grade = 'B';

    switch (grade)
    {
        case 'A':
            Console.WriteLine("Excellent!");
            break;
        case 'B':
        case 'C':
            Console.WriteLine("Well done");
            break;
        case 'D':
            Console.WriteLine("You passed");
            break;
        case 'F':
            Console.WriteLine("Better try again");
            break;
        default:
            Console.WriteLine("Invalid grade");
            break;
    }

