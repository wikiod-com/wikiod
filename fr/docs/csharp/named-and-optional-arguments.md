---
title: "Arguments nommés et facultatifs"
slug: "arguments-nommes-et-facultatifs"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

**Arguments nommés**

*Réf : MSDN* Les arguments nommés vous permettent de spécifier un argument pour un paramètre particulier en associant l'argument au nom du paramètre plutôt qu'à la position du paramètre dans la liste des paramètres.

Comme dit par MSDN, Un argument nommé ,

- Permet de passer l'argument à la fonction en associant le
nom du paramètre.
- Pas besoin de se souvenir de la position des paramètres que nous ne sommes pas
conscient de toujours.
- Inutile de regarder l'ordre des paramètres dans la liste des paramètres de
appelée fonction.
- Nous pouvons spécifier un paramètre pour chaque argument par son nom.

**Arguments facultatifs**

*Réf : MSDN* La définition d'une méthode, d'un constructeur, d'un indexeur ou d'un délégué peut spécifier que ses paramètres sont obligatoires ou qu'ils sont facultatifs. Tout appel doit fournir des arguments pour tous les paramètres requis, mais peut omettre des arguments pour les paramètres facultatifs.

Comme l'a dit MSDN, un argument facultatif,

- Nous pouvons omettre l'argument dans l'appel si cet argument est un Facultatif
Dispute
- Chaque argument facultatif a sa propre valeur par défaut
- Il prendra la valeur par défaut si nous ne fournissons pas la valeur
- La valeur par défaut d'un argument facultatif doit être un
- Expression constante.
- Doit être un type de valeur tel qu'enum ou struct.
- Doit être une expression de la forme default(valueType)
- Il doit être défini à la fin de la liste des paramètres

## Arguments facultatifs
Considérez que ce qui précède est notre définition de fonction avec des arguments facultatifs.

    private static double FindAreaWithOptional(int length, int width=56)
           {
               try
               {
                   return (length * width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

Ici, nous avons défini la valeur de la largeur comme facultative et donné la valeur 56. Si vous le notez, IntelliSense lui-même vous montre l'argument facultatif, comme indiqué dans l'image ci-dessous.

[![entrez la description de l'image ici][1]][1]

    Console.WriteLine("Area with Optional Argument : ");
    area = FindAreaWithOptional(120);
    Console.WriteLine(area);
    Console.Read();

Notez que nous n'avons pas eu d'erreur lors de la compilation et cela vous donnera une sortie comme suit.

[![entrez la description de l'image ici][2]][2]



**Utilisation d'un attribut facultatif.**

Une autre façon d'implémenter l'argument optionnel consiste à utiliser le mot-clé `[Optional]`. Si vous ne transmettez pas la valeur de l'argument facultatif, la valeur par défaut de ce type de données est affectée à cet argument. Le mot-clé "Optional" est présent dans l'espace de noms "Runtime.InteropServices".

    using System.Runtime.InteropServices;  
    private static double FindAreaWithOptional(int length, [Optional]int width)
       {
           try
           {
               return (length * width);
           }
           catch (Exception)
           {
               throw new NotImplementedException();
           }
       } 

    area = FindAreaWithOptional(120);  //area=0
Et lorsque nous appelons la fonction, nous obtenons 0 car le deuxième argument n'est pas passé et la valeur par défaut de int est 0 et donc le produit est 0.
    


[1] : http://i.stack.imgur.com/Uaszw.png
[2] : http://i.stack.imgur.com/3BWQA.png

## Arguments nommés
Considérez ce qui suit est notre appel de fonction.

    FindArea(120, 56);
Dans ce cas, notre premier argument est la longueur (c'est-à-dire 120) et le deuxième argument est la largeur (c'est-à-dire 56). Et nous calculons la surface par cette fonction. Et voici la définition de la fonction.

    private static double FindArea(int length, int width)
           {
               try
               {
                   return (length* width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

Ainsi, dans le premier appel de fonction, nous avons juste passé les arguments par sa position. Droit?

    double area;
    Console.WriteLine("Area with positioned argument is: ");
    area = FindArea(120, 56);
    Console.WriteLine(area);
    Console.Read();
Si vous exécutez ceci, vous obtiendrez une sortie comme suit.

[![entrez la description de l'image ici][1]][1]

Maintenant, voici les caractéristiques d'un argument nommé. Veuillez consulter l'appel de fonction précédent.


    Console.WriteLine("Area with Named argument is: ");
    area = FindArea(length: 120, width: 56);
    Console.WriteLine(area);
    Console.Read();

Ici, nous donnons les arguments nommés dans l'appel de méthode.

    area = FindArea(length: 120, width: 56);
Maintenant, si vous exécutez ce programme, vous obtiendrez le même résultat. Nous pouvons donner les noms inversement dans l'appel de méthode si nous utilisons les arguments nommés.

    Console.WriteLine("Area with Named argument vice versa is: ");
    area = FindArea(width: 120, length: 56);
    Console.WriteLine(area);
    Console.Read();

L'une des utilisations importantes d'un argument nommé est que, lorsque vous l'utilisez dans votre programme, cela améliore la lisibilité de votre code. Il dit simplement ce que votre argument est censé être, ou ce qu'il est ?.

Vous pouvez également donner les arguments positionnels. Cela signifie, une combinaison d'argument positionnel et d'argument nommé.

    Console.WriteLine("Area with Named argument Positional Argument : ");
                area = FindArea(120, width: 56);
                Console.WriteLine(area);
                Console.Read();

Dans l'exemple ci-dessus, nous avons passé 120 comme longueur et 56 comme argument nommé pour le paramètre largeur.

Il y a aussi quelques limitations. Nous allons maintenant discuter de la limitation des arguments nommés.

**Limitation de l'utilisation d'un argument nommé**

La spécification de l'argument nommé doit apparaître après que tous les arguments fixes ont été spécifiés.

Si vous utilisez un argument nommé avant un argument fixe, vous obtiendrez une erreur de compilation comme suit.

[![entrez la description de l'image ici][2]][2]

La spécification de l'argument nommé doit apparaître après que tous les arguments fixes ont été spécifiés


[1] : http://i.stack.imgur.com/aCYyR.png
[2] : http://i.stack.imgur.com/n8z4Y.png

