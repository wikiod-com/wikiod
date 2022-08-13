---
title: "Code non sécurisé dans .NET"
slug: "code-non-securise-dans-net"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

- Afin de pouvoir utiliser le mot clé `unsafe` dans un projet .Net, vous devez cocher "Allow unsafe code" dans Project Properties => Build
- L'utilisation d'un code non sécurisé peut améliorer les performances, mais cela se fait au détriment de la sécurité du code (d'où le terme « non sécurisé »).
    
Par exemple, lorsque vous utilisez une boucle for, un tableau comme celui-ci :

    for (int i = 0; i < array.Length; i++)
    {
        array[i] = 0;
    }

.NET Framework garantit que vous ne dépassez pas les limites du tableau, en lançant une `IndexOutOfRangeException` si l'index dépasse les limites.

Cependant, si vous utilisez du code non sécurisé, vous pouvez dépasser les limites du tableau comme ceci :


    unsafe
    {
        fixed (int* ptr = array)
        {
            for (int i = 0; i <= array.Length; i++)
            {
                *(ptr+i) = 0;
            }
        }
    }


## Utilisation d'unsafe avec des tableaux
Lors de l'accès à des tableaux avec des pointeurs, il n'y a pas de vérification des limites et donc aucune `IndexOutOfRangeException` ne sera levée. Cela rend le code plus rapide.

Affectation de valeurs à un tableau avec un pointeur :

    class Program
    {
        static void Main(string[] args)
        {
            unsafe
            {
                int[] array = new int[1000]; 
                fixed (int* ptr = array)
                {
                    for (int i = 0; i < array.Length; i++)
                    {
                        *(ptr+i) = i; //assigning the value with the pointer
                    }
                }
            }
        }
    }

Alors que la contrepartie sûre et normale serait :

   
    class Program
    {
        static void Main(string[] args)
        {            
            int[] array = new int[1000]; 

            for (int i = 0; i < array.Length; i++)
            {
                array[i] = i;
            }
        }
    }

La partie non sécurisée sera généralement plus rapide et la différence de performances peut varier en fonction de la complexité des éléments du tableau ainsi que de la logique appliquée à chacun. Même s'il peut être plus rapide, il doit être utilisé avec précaution car il est plus difficile à entretenir et plus facile à casser.

## Utilisation d'unsafe avec des chaînes
    var s = "Hello";      // The string referenced by variable 's' is normally immutable, but
                          // since it is memory, we could change it if we can access it in an 
                          // unsafe way.

    unsafe                // allows writing to memory; methods on System.String don't allow this
    {
      fixed (char* c = s) // get pointer to string originally stored in read only memory
        for (int i = 0; i < s.Length; i++)
          c[i] = 'a';     // change data in memory allocated for original string "Hello"
    }
    Console.WriteLine(s); // The variable 's' still refers to the same System.String
                          // value in memory, but the contents at that location were 
                          // changed by the unsafe write above.
                          // Displays: "aaaaa"

## Index de tableau non sécurisé
    void Main()
    {
        unsafe
        {
            int[] a = {1, 2, 3};
            fixed(int* b = a)
            {
                Console.WriteLine(b[4]);
            }
        }
    }

L'exécution de ce code crée un tableau de longueur 3, mais essaie ensuite d'obtenir le 5ème élément (index 4). Sur ma machine, cela a imprimé `1910457872`, mais le comportement n'est pas défini.

Sans le bloc `unsafe`, vous ne pouvez pas utiliser de pointeurs et ne pouvez donc pas accéder aux valeurs au-delà de la fin d'un tableau sans provoquer la levée d'une exception.

