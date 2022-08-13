---
title: "O(n) Algorithme de rotation circulaire d'un tableau"
slug: "on-algorithme-de-rotation-circulaire-dun-tableau"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Dans mon cheminement vers l'étude de la programmation, il y a eu des problèmes simples mais intéressants à résoudre sous forme d'exercices. L'un de ces problèmes était de faire pivoter un tableau (ou une autre collection) d'une certaine valeur. Ici, je vais partager avec vous une formule simple pour le faire.

## Exemple d'une méthode générique qui fait pivoter un tableau d'un décalage donné
Je voudrais souligner que nous tournons à gauche lorsque la valeur de décalage est négative et que nous tournons à droite lorsque la valeur est positive.

        public static void Main()
        {
            int[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            int shiftCount = 1;
            Rotate(ref array, shiftCount);
            Console.WriteLine(string.Join(", ", array));
            // Output: [10, 1, 2, 3, 4, 5, 6, 7, 8, 9]

            array = new []{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            shiftCount = 15;
            Rotate(ref array, shiftCount);
            Console.WriteLine(string.Join(", ", array));
            // Output: [6, 7, 8, 9, 10, 1, 2, 3, 4, 5]

            array = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            shiftCount = -1;
            Rotate(ref array, shiftCount);
            Console.WriteLine(string.Join(", ", array));
            // Output: [2, 3, 4, 5, 6, 7, 8, 9, 10, 1]

            array = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            shiftCount = -35;
            Rotate(ref array, shiftCount);
            Console.WriteLine(string.Join(", ", array));
            // Output: [6, 7, 8, 9, 10, 1, 2, 3, 4, 5]
        }

        private static void Rotate<T>(ref T[] array, int shiftCount)
        {
            T[] backupArray= new T[array.Length];

            for (int index = 0; index < array.Length; index++)
            {
                backupArray[(index + array.Length + shiftCount % array.Length) % array.Length] = array[index];
            }

            array = backupArray;
        }

La chose qui est importante dans ce code est la formule avec laquelle nous trouvons la nouvelle valeur d'index après la rotation.

**(index + array.Length + shiftCount % array.Length) % array.Length**

Voici un peu plus d'informations à son sujet :

**(shiftCount % array.Length)** -> nous normalisons la valeur de décalage pour qu'elle soit dans la longueur du tableau (puisque dans un tableau de longueur 10, décaler 1 ou 11 est la même chose, il en va de même pour -1 et -11).

**array.Length + (shiftCount % array.Length)** -> ceci est fait en raison des rotations à gauche pour s'assurer que nous n'entrons pas dans un index négatif, mais le faire pivoter jusqu'à la fin du tableau. Sans cela, pour un tableau de longueur 10 pour l'index 0 et une rotation -1, nous entrerions dans un nombre négatif (-1) et n'obtiendrions pas la valeur réelle de l'index de rotation, qui est 9. (10 + (-1 % 10) = 9)

**index + array.Length + (shiftCount % array.Length)** -> pas grand chose à dire ici car nous appliquons la rotation à l'index pour obtenir le nouvel index. (0 + 10 + (-1 % 10) = 9)

**index + array.Length + (shiftCount % array.Length) % array.Length** -> la deuxième normalisation s'assure que la nouvelle valeur d'index ne sort pas du tableau, mais fait tourner la valeur au début de le tableau. C'est pour les rotations à droite, car dans un tableau de longueur 10 sans lui pour l'index 9 et une rotation 1, nous irions dans l'index 10, qui est en dehors du tableau, et n'obtiendrions pas la valeur réelle de l'index de rotation est 0. ((9 + 10 + (1 % 10)) % 10 = 0)

