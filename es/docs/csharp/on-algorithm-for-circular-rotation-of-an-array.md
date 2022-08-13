---
title: "O(n) Algoritmo para la rotación circular de una matriz"
slug: "on-algoritmo-para-la-rotacion-circular-de-una-matriz"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

En mi camino para estudiar programación ha habido problemas simples, pero interesantes para resolver como ejercicios. Uno de esos problemas era rotar una matriz (u otra colección) por un cierto valor. Aquí compartiré contigo una fórmula sencilla para hacerlo.

## Ejemplo de un método genérico que rota una matriz en un cambio dado
Me gustaría señalar que giramos a la izquierda cuando el valor de desplazamiento es negativo y giramos a la derecha cuando el valor es positivo.

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

Lo que es importante en este código es la fórmula con la que encontramos el nuevo valor del índice después de la rotación.

**(índice + matriz.Longitud + shiftCount % matriz.Longitud) % matriz.Longitud**

Aquí tienes un poco más de información al respecto:

**(shiftCount % array.Length)** -> normalizamos el valor de desplazamiento para que esté en la longitud de la matriz (ya que en una matriz con una longitud de 10, cambiar 1 u 11 es lo mismo, lo mismo ocurre con -1 y -11).

**array.Length + (shiftCount % array.Length)** -> esto se hace debido a rotaciones a la izquierda para asegurarnos de que no entremos en un índice negativo, sino que lo rotamos hasta el final de la matriz. Sin él, para una matriz con una longitud de 10 para el índice 0 y una rotación -1, pasaríamos a un número negativo (-1) y no obtendríamos el valor real del índice de rotación, que es 9. (10 + (-1 % 10) = 9)

**index + array.Length + (shiftCount % array.Length)** -> No hay mucho que decir aquí, ya que aplicamos la rotación al índice para obtener el nuevo índice. (0 + 10 + (-1 % 10) = 9)

**index + array.Length + (shiftCount % array.Length) % array.Length** -> la segunda normalización se asegura de que el nuevo valor de índice no salga de la matriz, sino que rota el valor al comienzo de la matriz Es para rotaciones a la derecha, ya que en una matriz con longitud 10 sin ella para el índice 9 y una rotación 1 iríamos al índice 10, que está fuera de la matriz, y no obtendríamos el valor real del índice de rotación 0. ((9 + 10 + (1 % 10)) % 10 = 0)

