---
title: ".NET'te Güvenli Olmayan Kod"
slug: "nette-guvenli-olmayan-kod"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

- Bir .Net projesinde `unsafe' anahtar kelimesini kullanabilmek için, Project Properties => Build kısmında "Güvenli olmayan koda izin ver" seçeneğini işaretlemelisiniz.
- Güvenli olmayan kodun kullanılması performansı artırabilir, ancak bu, kod güvenliği pahasınadır (dolayısıyla 'güvensiz' terimi).
    
Örneğin, bir for döngüsü kullandığınızda şöyle bir dizi:

    for (int i = 0; i < array.Length; i++)
    {
        array[i] = 0;
    }

.NET Framework, dizin sınırları aşarsa bir "IndexOutOfRangeException" oluşturarak dizinin sınırlarını aşmamanızı sağlar.

Ancak, güvenli olmayan kod kullanırsanız, dizinin sınırlarını şu şekilde aşabilirsiniz:


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


## Dizilerle güvensiz kullanma
İşaretçilerle dizilere erişirken, sınır denetimi yoktur ve bu nedenle hiçbir `IndexOutOfRangeException' atılmaz. Bu, kodu daha hızlı hale getirir.

İşaretçi ile bir diziye değer atama:

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

Güvenli ve normal muadili şöyle olsa da:

   
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

Güvenli olmayan kısım genellikle daha hızlı olacaktır ve performans farkı, dizideki öğelerin karmaşıklığının yanı sıra her birine uygulanan mantığa bağlı olarak değişebilir. Daha hızlı olsa da bakımı daha zor ve kırılması daha kolay olduğu için dikkatli kullanılmalıdır.

## Dizelerle güvensiz kullanma
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

## Güvenli Olmayan Dizi Dizini
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

Bu kodu çalıştırmak, 3 uzunluğunda bir dizi oluşturur, ancak daha sonra 5. öğeyi (indeks 4) almaya çalışır. Makinemde bu, '1910457872' yazdırıldı, ancak davranış tanımlı değil.

'Güvensiz' blok olmadan, işaretçileri kullanamazsınız ve bu nedenle, bir istisnanın atılmasına neden olmadan bir dizinin sonundan sonraki değerlere erişemezsiniz.

