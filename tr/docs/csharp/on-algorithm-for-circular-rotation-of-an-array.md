---
title: "O(n) Bir dizinin dairesel dönüşü için algoritma"
slug: "on-bir-dizinin-dairesel-donusu-icin-algoritma"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

Programlama eğitimime giden yolda, alıştırma olarak çözmem gereken basit ama ilginç problemler vardı. Bu sorunlardan biri, bir diziyi (veya başka bir koleksiyonu) belirli bir değerle döndürmekti. Bunun için basit bir formülü sizlerle paylaşacağım.

## Bir diziyi belirli bir kaydırmayla döndüren genel bir yöntem örneği
Kaydırma değeri negatif olduğunda sola, değer pozitif olduğunda sağa dönüş yaptığımızı belirtmek isterim.

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

Bu kodda önemli olan rotasyondan sonra yeni indeks değerini bulduğumuz formüldür.

**(dizin + dizi.Length + shiftCount % dizi.Length) % dizi.Length**

İşte bununla ilgili biraz daha bilgi:

**(shiftCount % array.Length)** -> kaydırma değerini dizinin uzunluğunda olacak şekilde normalize ediyoruz (uzunluğu 10 olan bir dizide 1 veya 11 kaydırmak aynı şey olduğundan -1 için de aynısı geçerli ve -11).

**array.Length + (shiftCount % array.Length)** -> bu, negatif bir indekse girmememizi, dizinin sonuna döndürmemizi sağlamak için sola döndürmeler nedeniyle yapılır. İndeks 0 için uzunluk 10 ve rotasyon -1 olan bir dizi olmadan, negatif bir sayıya (-1) gider ve 9 olan gerçek rotasyon indeks değerini alamazdık (10 + (-1 % 10) = 9)

**index + array.Length + (shiftCount % array.Length)** -> yeni dizini almak için dizine rotasyon uyguladığımız için burada söylenecek fazla bir şey yok. (0 + 10 + (-1 % 10) = 9)

**index + array.Length + (shiftCount % array.Length) % array.Length** -> ikinci normalleştirme, yeni indeks değerinin dizinin dışına çıkmamasını, ancak dizinin başındaki değeri döndürmesini sağlamaktır. dizi. Doğru döndürmeler içindir, çünkü dizin 9 ve döndürme 1 için onsuz 10 uzunluğunda bir dizide dizinin dışında olan dizin 10'a giderdik ve gerçek döndürme dizin değeri 0'ı alamazdık. ((9) + 10 + (1 % 10)) % 10 = 0)

