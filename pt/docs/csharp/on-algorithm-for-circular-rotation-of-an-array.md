---
title: "O(n) Algoritmo para rotação circular de uma matriz"
slug: "on-algoritmo-para-rotacao-circular-de-uma-matriz"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

No meu caminho para estudar programação houve problemas simples, mas interessantes para resolver como exercícios. Um desses problemas era girar um array (ou outra coleção) por um determinado valor. Aqui vou compartilhar com você uma fórmula simples para fazê-lo.

## Exemplo de um método genérico que gira um array por um determinado deslocamento
Gostaria de salientar que giramos para a esquerda quando o valor de deslocamento é negativo e giramos para a direita quando o valor é positivo.

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

O importante neste código é a fórmula com a qual encontramos o novo valor do índice após a rotação.

**(índice + array.Length + shiftCount % array.Length) % array.Length**

Segue um pouco mais de informações sobre ele:

**(shiftCount % array.Length)** -> normalizamos o valor de deslocamento para estar no comprimento do array (já que em um array com comprimento 10, o deslocamento de 1 ou 11 é a mesma coisa, o mesmo vale para -1 e -11).

**array.Length + (shiftCount % array.Length)** -> isso é feito devido a rotações para a esquerda para garantir que não entremos em um índice negativo, mas o gire até o final do array. Sem ele para um array com comprimento 10 para índice 0 e uma rotação -1 iríamos para um número negativo (-1) e não obteríamos o valor real do índice de rotação, que é 9. (10 + (-1 % 10) = 9)

**index + array.Length + (shiftCount % array.Length)** -> não há muito a dizer aqui, pois aplicamos a rotação ao índice para obter o novo índice. (0 + 10 + (-1% 10) = 9)

**index + array.Length + (shiftCount % array.Length) % array.Length** -> a segunda normalização é garantir que o novo valor de índice não saia do array, mas gire o valor no início de a matriz. É para rotações à direita, pois em um array com comprimento 10 sem ele para o índice 9 e uma rotação 1 iríamos para o índice 10, que está fora do array, e não obteríamos o valor real do índice de rotação é 0. ((9 + 10 + (1 % 10)) % 10 = 0)

