---
title: "Código inseguro em .NET"
slug: "codigo-inseguro-em-net"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

- Para poder usar a palavra-chave `unsafe` em um projeto .Net, você deve marcar "Permitir código não seguro" em Propriedades do projeto => Construir
- Usar código inseguro pode melhorar o desempenho, no entanto, é às custas da segurança do código (daí o termo `inseguro`).
    
Por exemplo, quando você usa um loop for uma matriz assim:

    for (int i = 0; i < array.Length; i++)
    {
        array[i] = 0;
    }

O .NET Framework garante que você não exceda os limites da matriz, lançando uma `IndexOutOfRangeException` se o índice exceder os limites.

No entanto, se você usar código não seguro, poderá exceder os limites da matriz da seguinte forma:


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


## Usando inseguro com arrays
Ao acessar arrays com ponteiros, não há verificação de limites e, portanto, nenhuma `IndexOutOfRangeException` será lançada. Isso torna o código mais rápido.

Atribuindo valores a um array com um ponteiro:

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

Enquanto a contrapartida segura e normal seria:

   
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

A parte insegura geralmente será mais rápida e a diferença de desempenho pode variar dependendo da complexidade dos elementos do array, bem como da lógica aplicada a cada um. Mesmo que seja mais rápido, deve ser usado com cuidado, pois é mais difícil de manter e mais fácil de quebrar.

## Usando inseguro com strings
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

## Índice de array não seguro
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

A execução deste código cria uma matriz de comprimento 3, mas tenta obter o 5º item (índice 4). Na minha máquina, isso imprimiu `1910457872`, mas o comportamento não está definido.

Sem o bloco `unsafe`, você não pode usar ponteiros e, portanto, não pode acessar valores além do final de um array sem causar uma exceção.

