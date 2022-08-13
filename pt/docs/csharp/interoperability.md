---
title: "Interoperabilidade"
slug: "interoperabilidade"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

**Trabalhando com a API Win32 usando C#**

O Windows expõe muitas funcionalidades na forma de API Win32. Usando essas APIs você pode realizar operações diretas nas janelas, o que aumenta o desempenho de sua aplicação.Fonte [Clique aqui][1]


O Windows expõe uma ampla variedade de API. Para obter informações sobre várias APIs, você pode verificar sites como [pinvoke][2].


[1]: http://www.c-sharpcorner.com/article/working-with-win32-api-in-net/
[2]: http://pinvoke.net

## Importar função de DLL C++ não gerenciada
Aqui está um exemplo de como importar uma função definida em uma DLL C++ não gerenciada. No código-fonte C++ para "myDLL.dll", a função `add` é definida:

    extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    {
        return a + b;
    }

Em seguida, ele pode ser incluído em um programa C# da seguinte maneira:

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

Consulte [Convenções de chamada](https://www.wikiod.com/pt/docs/c%23/3278/interoperability/16910/calling-conventions#t=201609062059032452959) e [C++ name mangling](https://www.wikiod.com/pt/docs/ /c%23/3278/interoperability/16909/c-name-mangling) para explicações sobre por que `extern "C"` e `__stdcall` são necessários.

# Encontrando a biblioteca dinâmica

Quando o método extern é invocado pela primeira vez, o programa C# pesquisará e carregará a DLL apropriada. Para obter mais informações sobre onde é pesquisado para encontrar a DLL e como você pode influenciar os locais de pesquisa, consulte [esta pergunta do stackoverflow](http://stackoverflow.com/questions/8836093/how-can-i-specify-a-dllimport -caminho em tempo de execução).






## Convenções de chamada
Existem várias convenções para chamar funções, especificando quem (chamador ou chamado) retira argumentos da pilha, como os argumentos são passados ​​e em que ordem. C++ usa a convenção de chamada `Cdecl` por padrão, mas C# espera `StdCall`, que geralmente é usado pela API do Windows. Você precisa alterar um ou outro:

* Altere a convenção de chamada para `StdCall` em C++:

      extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

* Ou mude a convenção de chamada para `Cdecl` em C#:

      extern "C" __declspec(dllexport) int /*__cdecl*/ add(int a, int b)
    <!---->
      [DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl)]

Se você quiser usar uma função com convenção de chamada `Cdecl` e um nome desconfigurado, seu código ficará assim:

    __declspec(dllexport) int add(int a, int b)
<!---->
    [DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl,
               EntryPoint = "?add@@YAHHH@Z")]

- **thiscall**(**__thiscall**) é usado principalmente em funções que são membros de uma classe.

- Quando uma função usa **thiscall**(**__thiscall**) , um ponteiro para a classe é passado como o primeiro parâmetro.

## Desarticulação de nomes em C++
Os compiladores C++ codificam informações adicionais nos nomes das funções exportadas, como tipos de argumentos, para possibilitar sobrecargas com argumentos diferentes. Esse processo é chamado de [name mangling](https://en.wikipedia.org/wiki/Name_mangling). Isso causa problemas com a importação de funções em C# (e interoperabilidade com outras linguagens em geral), pois o nome da função `int add(int a, int b)` não é mais `add`, pode ser `?add@@YAHHH @Z`, `_add@8` ou qualquer outra coisa, dependendo do compilador e da convenção de chamada.

Existem várias maneiras de resolver o problema do desmembramento de nomes:

* Exportando funções usando `extern "C"` para alternar para C external linkage que usa C name mangling:

      extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

    Function name will still be mangled (`_add@8`), but `StdCall`+`extern "C"` name mangling is recognized by C# compiler.

* Especificando os nomes das funções exportadas no arquivo de definição do módulo `myDLL.def`:

      EXPORTS
        add
    <!---->
      int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

O nome da função será puro `add` neste caso.

* Importando nome mutilado. Você precisará de algum visualizador de DLL para ver o nome desconfigurado e, em seguida, poderá especificá-lo explicitamente:

      __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll", EntryPoint = "?add@@YGHHH@Z")]

## Carregamento e descarregamento dinâmico de DLLs não gerenciadas
Ao usar o atributo `DllImport` você precisa saber a dll correta e o nome do método em *tempo de compilação*. Se você quiser ser mais flexível e decidir em *tempo de execução* qual dll e métodos carregar, você pode usar os métodos da API do Windows `LoadLibrary()`, [`GetProcAddress()`](https://msdn.microsoft.com /en-us/library/windows/desktop/ms683212(v=vs.85).aspx) e `FreeLibrary()`. Isso pode ser útil se a biblioteca a ser usada depender das condições de tempo de execução.

O ponteiro retornado por `GetProcAddress()` pode ser convertido em um delegado usando [`Marshal.GetDelegateForFunctionPointer()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal. getdelegateforfunctionpointer(v=vs.110).aspx).

O exemplo de código a seguir demonstra isso com o `myDLL.dll` dos exemplos anteriores:

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

## Lidando com erros do Win32

Ao usar métodos de interoperabilidade, você pode usar a API **GetLastError** para obter informações adicionais sobre suas chamadas de API.

**Atributo DllImport SetLastError**

*SetLastError=true*

Indica que o receptor chamará SetLastError (função da API do Win32).

*SetLastError=false*

Indica que o chamado **não** chamará SetLastError (função da API do Win32), portanto você não receberá uma informação de erro.

* Quando SetLastError não está definido, é definido como false (valor padrão).

* Você pode obter o código de erro usando o método Marshal.GetLastWin32Error:



*Exemplo:*

   
    [DllImport("kernel32.dll", SetLastError=true)]
    public static extern IntPtr OpenMutex(uint access, bool handle, string lpName);



Se você tentar abrir um mutex que não existe, GetLastError retornará **ERROR_FILE_NOT_FOUND**.



    var lastErrorCode = Marshal.GetLastWin32Error();
    
    if (lastErrorCode == (uint)ERROR_FILE_NOT_FOUND)
    {
        //Deal with error         
    }
Os códigos de erro do sistema podem ser encontrados aqui:

https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx


**API GetLastError**

Existe uma API nativa **GetLastError** que você também pode usar:

    [DllImport("coredll.dll", SetLastError=true)]
    static extern Int32 GetLastError();

* Ao chamar a API do Win32 a partir do código gerenciado, você deve sempre usar o **Marshal.GetLastWin32Error**.

Aqui está o porquê:

Entre sua chamada Win32 que define o erro (chama SetLastError), o CLR pode chamar outras chamadas Win32 que também podem chamar **SetLastError**, esse comportamento pode substituir seu valor de erro. Nesse cenário, se você chamar **GetLastError**, poderá obter um erro inválido.

Definir **SetLastError = true** garante que o CLR recupere o código de erro antes de executar outras chamadas do Win32.



## Lendo estruturas com Marshal
A classe Marshal contém uma função chamada **PtrToStructure**, essa função nos dá a capacidade de ler estruturas por um ponteiro não gerenciado.

A função **PtrToStructure** recebeu muitas sobrecargas, mas todas têm a mesma intenção.

**PtrToStructure** Genérico:

    public static T PtrToStructure<T>(IntPtr ptr);

*T* - tipo de estrutura.

*ptr* - Um ponteiro para um bloco de memória não gerenciado.

Exemplo:

    NATIVE_STRUCT result = Marshal.PtrToStructure<NATIVE_STRUCT>(ptr);       

* Se você estiver lidando com objetos gerenciados enquanto lê estruturas nativas, não esqueça de fixar seu objeto :)

   

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

 





## Código simples para expor a classe para com


## Objeto fixado
**GC** (Garbage Collector) é responsável pela limpeza do nosso lixo.

Enquanto o **GC** limpa nosso lixo, ele remove os objetos não utilizados do heap gerenciado que causam a fragmentação do heap. Quando o **GC** termina a remoção, ele executa uma compactação de heap (desfragmentação) que envolve a movimentação de objetos no heap.

Como **GC** não é determinístico, ao passar referência/ponteiro de objeto gerenciado para código nativo, **GC** pode ser ativado a qualquer momento, se ocorrer logo após a chamada do Inerop, há uma possibilidade muito boa de que o objeto (qual referência passada para nativo) será movida no heap gerenciado - como resultado, obtemos uma referência inválida no lado gerenciado.

Nesse cenário, você deve **fixar** o objeto antes de passá-lo para o código nativo.

**Objeto Fixado**

Objeto fixado é um objeto que não pode ser movido pelo GC.

**Alça Fixada Gc**

Você pode criar um objeto pin usando o método **Gc.Alloc**

    GCHandle handle = GCHandle.Alloc(yourObject, GCHandleType.Pinned); 

* A obtenção de um **GCHandle** fixado em um objeto gerenciado marca um objeto específico como um que não pode ser movido pelo **GC**, até liberar a alça

Exemplo:

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

**Precauções**

* Ao fixar o objeto (especialmente os grandes), tente liberar o **GcHandle** fixado o mais rápido possível, pois ele interrompe a desfragmentação do heap.
* Se você esquecer de liberar o **GcHandle**, nada o fará. Faça isso em uma seção de código segura (como finalmente)





