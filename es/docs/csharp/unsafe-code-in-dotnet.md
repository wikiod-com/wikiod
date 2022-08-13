---
title: "Código no seguro en .NET"
slug: "codigo-no-seguro-en-net"
draft: false
images: []
weight: 9932
type: docs
toc: true
---

- Para poder usar la palabra clave `inseguro` en un proyecto .Net, debe marcar "Permitir código no seguro" en Propiedades del proyecto => Construir
- El uso de código inseguro puede mejorar el rendimiento, sin embargo, es a expensas de la seguridad del código (de ahí el término "inseguro").
    
Por ejemplo, cuando usa un bucle for, una matriz así:

    for (int i = 0; i < array.Length; i++)
    {
        array[i] = 0;
    }

.NET Framework garantiza que no exceda los límites de la matriz, lanzando una `IndexOutOfRangeException` si el índice excede los límites.

Sin embargo, si usa un código no seguro, puede exceder los límites de la matriz de esta manera:


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


## Uso inseguro con arreglos
Al acceder a las matrices con punteros, no hay verificación de límites y, por lo tanto, no se lanzará ninguna excepción `IndexOutOfRangeException`. Esto hace que el código sea más rápido.

Asignando valores a una matriz con un puntero:

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

Mientras que la contraparte segura y normal sería:

   
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

La parte insegura generalmente será más rápida y la diferencia en el rendimiento puede variar según la complejidad de los elementos en la matriz, así como la lógica aplicada a cada uno. Aunque puede ser más rápido, debe usarse con cuidado ya que es más difícil de mantener y más fácil de romper.

## Uso inseguro con cadenas
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

## Índice de matriz insegura
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

Ejecutar este código crea una matriz de longitud 3, pero luego intenta obtener el quinto elemento (índice 4). En mi máquina, esto imprimió `1910457872`, pero el comportamiento no está definido.

Sin el bloque `inseguro`, no puede usar punteros y, por lo tanto, no puede acceder a los valores más allá del final de una matriz sin que se produzca una excepción.

