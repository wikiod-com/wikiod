---
title: "interoperabilidad"
slug: "interoperabilidad"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

**Trabajando con la API de Win32 usando C#**

Windows expone muchas funciones en forma de API Win32. Con estas API, puede realizar operaciones directas en Windows, lo que aumenta el rendimiento de su aplicación. Fuente [Haga clic aquí][1]


Windows expone una amplia gama de API. Para obtener información sobre varias API, puede consultar sitios como [pinvoke][2].


[1]: http://www.c-sharpcorner.com/article/working-with-win32-api-in-net/
[2]: http://pinvoke.net

## Función de importación desde DLL de C++ no administrado
Este es un ejemplo de cómo importar una función que está definida en una DLL de C++ no administrada. En el código fuente de C++ para "myDLL.dll", se define la función `add`:

    extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    {
        return a + b;
    }

Luego se puede incluir en un programa C# de la siguiente manera:

    class Program
    {
        // This line will import the C++ method.
        // The name specified in the DllImport attribute must be the DLL name.
        // The names of parameters are unimportant, but the types must be correct.
        [DllImport("myDLL.dll")]
        private static extern int add(int left, int right);

        static void Main(string[] args)
        {
            //The extern method can be called just as any other C# method.
            Console.WriteLine(add(1, 2));
        }
    }

Consulte [Convenciones de llamadas](https://www.wikiod.com/es/docs/c%23/3278/interoperability/16910/calling-conventions#t=201609062059032452959) y [C++ manipulación de nombres](https://www.wikiod.com/es/docs/ /c%23/3278/interoperability/16909/c-name-mangle) para obtener explicaciones sobre por qué son necesarios `extern "C"` y `__stdcall`.

# Encontrar la biblioteca dinámica

Cuando se invoca por primera vez el método externo, el programa C# buscará y cargará la DLL adecuada. Para obtener más información sobre dónde se busca para encontrar la DLL y cómo puede influir en las ubicaciones de búsqueda, consulte [esta pregunta de stackoverflow] (http://stackoverflow.com/questions/8836093/how-can-i-specify-a-dllimport -ruta-en-tiempo-de-ejecución).






## Convenciones de llamadas
Hay varias convenciones para llamar a funciones, que especifican quién (llamador o destinatario) saca los argumentos de la pila, cómo se pasan los argumentos y en qué orden. C++ usa la convención de llamada `Cdecl` de manera predeterminada, pero C# espera `StdCall`, que generalmente usa la API de Windows. Necesitas cambiar uno u otro:

* Cambiar la convención de llamadas a `StdCall` en C++:

      extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

* O cambie la convención de llamadas a `Cdecl` en C#:

      extern "C" __declspec(dllexport) int /*__cdecl*/ add(int a, int b)
    <!---->
      [DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl)]

Si desea utilizar una función con la convención de llamada `Cdecl` y un nombre alterado, su código se verá así:

    __declspec(dllexport) int add(int a, int b)
<!---->
    [DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl,
               EntryPoint = "?add@@YAHHH@Z")]

- **thiscall**(**__thiscall**) se usa principalmente en funciones que son miembros de una clase.

- Cuando una función usa **thiscall**(**__thiscall**) , se pasa un puntero a la clase como primer parámetro.

## C++ manipulación de nombres
Los compiladores de C++ codifican información adicional en los nombres de las funciones exportadas, como los tipos de argumentos, para hacer posibles las sobrecargas con diferentes argumentos. Este proceso se llama [cambio de nombre](https://en.wikipedia.org/wiki/Name_mangling). Esto causa problemas con la importación de funciones en C# (y la interoperabilidad con otros lenguajes en general), ya que el nombre de la función `int add(int a, int b)` ya no es `add`, puede ser `?add@@YAHHH @Z`, `_add@8` o cualquier otra cosa, según el compilador y la convención de llamadas.

Hay varias formas de resolver el problema de la manipulación de nombres:

* Funciones de exportación usando `extern "C"` para cambiar a un enlace externo C que usa la manipulación de nombres C:

      extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

    Function name will still be mangled (`_add@8`), but `StdCall`+`extern "C"` name mangling is recognized by C# compiler.

* Especificación de nombres de funciones exportadas en el archivo de definición del módulo `myDLL.def`:

      EXPORTS
        add
    <!---->
      int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

El nombre de la función será puro `add` en este caso.

* Importación de nombre destrozado. Necesitará algún visor de DLL para ver el nombre alterado, luego puede especificarlo explícitamente:

      __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll", EntryPoint = "?add@@YGHHH@Z")]

## Carga y descarga dinámica de archivos DLL no administrados
Al usar el atributo `DllImport`, debe conocer el dll correcto y el nombre del método en *tiempo de compilación*. Si desea ser más flexible y decidir en *tiempo de ejecución* qué dll y métodos cargar, puede usar los métodos de la API de Windows `LoadLibrary()`, [`GetProcAddress()`](https://msdn.microsoft.com /en-us/library/windows/desktop/ms683212(v=vs.85).aspx) y `FreeLibrary()`. Esto puede ser útil si la biblioteca a usar depende de las condiciones de tiempo de ejecución.

El puntero devuelto por `GetProcAddress()` se puede convertir en un delegado mediante [`Marshal.GetDelegateForFunctionPointer()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal. getdelegateforfunctionpointer(v=vs.110).aspx).

El siguiente ejemplo de código demuestra esto con `myDLL.dll` de los ejemplos anteriores:

    class Program
    {
        // import necessary API as shown in other examples
        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern IntPtr LoadLibrary(string lib);
        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern void FreeLibrary(IntPtr module);
        [DllImport("kernel32.dll", SetLastError = true)]
        public static extern IntPtr GetProcAddress(IntPtr module, string proc);

        // declare a delegate with the required signature
        private delegate int AddDelegate(int a, int b);

        private static void Main()
        {
            // load the dll
            IntPtr module = LoadLibrary("myDLL.dll");
            if (module == IntPtr.Zero) // error handling
            {
                Console.WriteLine($"Could not load library: {Marshal.GetLastWin32Error()}");
                return;
            }

            // get a "pointer" to the method
            IntPtr method = GetProcAddress(module, "add");
            if (method == IntPtr.Zero) // error handling
            {
                Console.WriteLine($"Could not load method: {Marshal.GetLastWin32Error()}");
                FreeLibrary(module);  // unload library
                return;
            }
                
            // convert "pointer" to delegate
            AddDelegate add = (AddDelegate)Marshal.GetDelegateForFunctionPointer(method, typeof(AddDelegate));
        
            // use function    
            int result = add(750, 300);
            
            // unload library   
            FreeLibrary(module);
        }
    }

## Tratar con los errores de Win32

Al usar métodos de interoperabilidad, puede usar la API **GetLastError** para obtener información adicional sobre sus llamadas a la API.

**DllImport Atributo SetLastError Atributo**

*EstablecerÚltimoError=verdadero*

Indica que el destinatario llamará a SetLastError (función API de Win32).

*EstablecerÚltimoError=falso*

Indica que el destinatario **no** llamará a SetLastError (función de la API de Win32), por lo tanto, no obtendrá información de error.

* Cuando SetLastError no está configurado, se establece en falso (valor predeterminado).

* Puede obtener el código de error utilizando el método Marshal.GetLastWin32Error:



*Ejemplo:*

   
    [DllImport("kernel32.dll", SetLastError=true)]
    public static extern IntPtr OpenMutex(uint access, bool handle, string lpName);



Si intenta abrir un mutex que no existe, GetLastError devolverá **ERROR_FILE_NOT_FOUND**.



    var lastErrorCode = Marshal.GetLastWin32Error();
    
    if (lastErrorCode == (uint)ERROR_FILE_NOT_FOUND)
    {
        //Deal with error         
    }
Los códigos de error del sistema se pueden encontrar aquí:

https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx


**API GetLastError**

Hay una API nativa **GetLastError** que también puede usar:

    [DllImport("coredll.dll", SetLastError=true)]
    static extern Int32 GetLastError();

* Al llamar a la API de Win32 desde el código administrado, siempre debe usar **Marshal.GetLastWin32Error**.

Este es el por qué:

Entre su llamada de Win32 que establece el error (llama a SetLastError), el CLR puede llamar a otras llamadas de Win32 que también podrían llamar a **SetLastError**, este comportamiento puede anular su valor de error. En este escenario, si llama a **GetLastError**, puede obtener un error no válido.

Al configurar **SetLastError = true**, se asegura de que CLR recupere el código de error antes de ejecutar otras llamadas de Win32.



## Lectura de estructuras con Marshal
La clase Marshal contiene una función llamada **PtrToStructure**, esta función nos brinda la capacidad de leer estructuras mediante un puntero no administrado.

La función **PtrToStructure** recibió muchas sobrecargas, pero todas tienen la misma intención.

Genérico **PtrToStructure**:

    public static T PtrToStructure<T>(IntPtr ptr);

*T* - tipo de estructura.

*ptr*: un puntero a un bloque de memoria no administrado.

Ejemplo:

    NATIVE_STRUCT result = Marshal.PtrToStructure<NATIVE_STRUCT>(ptr);       

* Si trata con objetos administrados mientras lee estructuras nativas, no olvide anclar su objeto :)

   

     T Read<T>(byte[] buffer)
        {
            T result = default(T);
            
            var gch = GCHandle.Alloc(buffer, GCHandleType.Pinned);
        
            try
            {
                result = Marshal.PtrToStructure<T>(gch.AddrOfPinnedObject());
            }
            finally
            {
                gch.Free();
            }
            
            return result;
        }

 





## Código simple para exponer la clase para com


## Objeto anclado
**GC** (Garbage Collector) es responsable de limpiar nuestra basura.

Mientras **GC** limpia nuestra basura, elimina los objetos no utilizados del montón administrado que causan la fragmentación del montón. Cuando **GC** termina con la eliminación, realiza una compresión de almacenamiento dinámico (desfragmentación) que implica mover objetos en el almacenamiento dinámico.

Dado que **GC** no es determinista, al pasar la referencia/el puntero del objeto administrado al código nativo, **GC** puede activarse en cualquier momento, si ocurre justo después de la llamada de Inerop, existe una gran posibilidad de que el objeto (cuya referencia pasó a nativo) se moverá al montón administrado; como resultado, obtenemos una referencia no válida en el lado administrado.

En este escenario, debe **fijar** el objeto antes de pasarlo al código nativo.

**Objeto anclado**

El objeto anclado es un objeto que GC no permite mover.

**Manija fijada Gc**

Puede crear un objeto pin usando el método **Gc.Alloc**

    GCHandle handle = GCHandle.Alloc(yourObject, GCHandleType.Pinned); 

* Obtener un **GCHandle** anclado a un objeto administrado marca un objeto específico como uno que **GC** no puede mover, hasta que se libera el controlador

Ejemplo:

    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern void EnterCriticalSection(IntPtr ptr);
    
    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern void LeaveCriticalSection(IntPtr ptr);
           
    public void EnterCriticalSection(CRITICAL_SECTION section)
    {
        try
        {
            GCHandle handle = GCHandle.Alloc(section, GCHandleType.Pinned); 
            EnterCriticalSection(handle.AddrOfPinnedObject());
            //Do Some Critical Work
            LeaveCriticalSection(handle.AddrOfPinnedObject());
        }
        finaly
        {
            handle.Free()
        }
    }

**Precauciones**

* Al anclar objetos (especialmente los grandes), intente liberar el **GcHandle** anclado lo más rápido posible, ya que interrumpe la desfragmentación del montón.
* Si olvida liberar **GcHandle**, nada lo hará. Hazlo en una sección de código segura (como finalmente)





