---
title: "birlikte çalışabilirlik"
slug: "birlikte-calsabilirlik"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

**C# kullanarak Win32 API ile çalışma**

Windows, Win32 API biçiminde birçok işlevsellik sunar. Bu API'yi kullanarak, uygulamanızın performansını artıran pencerelerde doğrudan işlem gerçekleştirebilirsiniz.Kaynak [Buraya tıklayın][1]


Windows, geniş bir API yelpazesi sunar. Çeşitli API'ler hakkında bilgi almak için [pinvoke][2] gibi siteleri inceleyebilirsiniz.


[1]: http://www.c-sharpcorner.com/article/working-with-win32-api-in-net/
[2]: http://pinvoke.net

## Yönetilmeyen C++ DLL'sinden içe aktarma işlevi
Burada, yönetilmeyen bir C++ DLL'sinde tanımlanan bir işlevin nasıl içe aktarılacağına ilişkin bir örnek verilmiştir. "myDLL.dll" için C++ kaynak kodunda, "add" işlevi tanımlanmıştır:

    extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    {
        return a + b;
    }

Daha sonra aşağıdaki gibi bir C# programına dahil edilebilir:

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

Bkz. [Çağrı kuralları](https://www.wikiod.com/tr/docs/c%23/3278/interoperability/16910/calling-conventions#t=201609062059032452959) ve [C++ ad yönetimi](https://www.wikiod.com/tr/docs/ /c%23/3278/interoperability/16909/c-name-mangling) "harici "C"" ve "__stdcall" neden gerekli olduğuna ilişkin açıklamalar için.

# Dinamik kitaplığı bulma

Extern yöntemi ilk kez çağrıldığında, C# programı uygun DLL'yi arar ve yükler. DLL'yi bulmak için nerede arandığı ve arama konumlarını nasıl etkileyebileceğiniz hakkında daha fazla bilgi için [bu yığın akışı sorusuna] bakın (http://stackoverflow.com/questions/8836093/how-can-i-specify-a-dllimport -çalışma zamanında yol).






## Çağrı kuralları
Çağıran işlevlerin, yığından argümanları kimin (arayanın veya arananın) çıkaracağını, argümanların nasıl ve hangi sırayla iletileceğini belirten çeşitli kuralları vardır. C++, varsayılan olarak "Cdecl" çağrı kuralı kullanır, ancak C#, genellikle Windows API tarafından kullanılan "StdCall"ı bekler. Birini veya diğerini değiştirmeniz gerekir:

* C++'da çağrı kuralını 'StdCall' olarak değiştirin:

      extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

* Veya, C#'da arama kuralını "Cdecl" olarak değiştirin:

      extern "C" __declspec(dllexport) int /*__cdecl*/ add(int a, int b)
    <!---->
      [DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl)]

'Cdecl' çağrı kuralına ve karışık bir ada sahip bir işlev kullanmak istiyorsanız, kodunuz şöyle görünecektir:

    __declspec(dllexport) int add(int a, int b)
<!---->
    [DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl,
               EntryPoint = "?add@@YAHHH@Z")]

- **thiscall**(**__thiscall**) esas olarak bir sınıfın üyesi olan işlevlerde kullanılır.

- Bir işlev **thiscall**(**__thiscall**) işlevini kullandığında, ilk parametre olarak sınıfa bir işaretçi aktarılır.

## C++ ad yönetimi
C++ derleyicileri, farklı bağımsız değişkenlerle aşırı yüklemeleri mümkün kılmak için bağımsız değişken türleri gibi dışa aktarılan işlevlerin adlarına ek bilgileri kodlar. Bu işleme [isim yönetimi](https://en.wikipedia.org/wiki/Name_mangling) denir. Bu, `int add(int a, int b)` işlevinin adı artık `add` olmadığı için, `?add@@YAHHH olabilir. @Z`, `_add@8` veya derleyiciye ve çağrı kuralına bağlı olarak başka bir şey.

Ad değiştirme sorununu çözmenin birkaç yolu vardır:

* C ad yönetimini kullanan C harici bağlantısına geçmek için `harici "C"` kullanarak işlevleri dışa aktarma:

      extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

    Function name will still be mangled (`_add@8`), but `StdCall`+`extern "C"` name mangling is recognized by C# compiler.

* `myDLL.def` modül tanım dosyasında dışa aktarılan fonksiyon adlarının belirtilmesi:

      EXPORTS
        add
    <!---->
      int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

Bu durumda işlev adı saf 'add' olacaktır.

* Karışık adı içe aktarma. Karışık adı görmek için biraz DLL görüntüleyiciye ihtiyacınız olacak, ardından açıkça belirtebilirsiniz:

      __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll", EntryPoint = "?add@@YGHHH@Z")]

## Yönetilmeyen DLL'lerin dinamik olarak yüklenmesi ve boşaltılması
'DllImport' özniteliğini kullanırken *derleme zamanında* doğru dll ve yöntem adını bilmeniz gerekir. Daha esnek olmak ve *çalışma zamanında* hangi dll'nin ve yöntemlerin yükleneceğine karar vermek istiyorsanız, `LoadLibrary()`, [`GetProcAddress()`](https://msdn.microsoft.com) Windows API yöntemlerini kullanabilirsiniz. /en-us/library/windows/desktop/ms683212(v=vs.85).aspx) ve 'FreeLibrary()'. Bu, kullanılacak kitaplığın çalışma zamanı koşullarına bağlı olması durumunda yardımcı olabilir.

"GetProcAddress()" tarafından döndürülen işaretçi, [`Marshal.GetDelegateForFunctionPointer()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal) kullanılarak bir temsilciye dönüştürülebilir. getdelegateforfunctionpointer(v=vs.110).aspx).

Aşağıdaki kod örneği, önceki örneklerdeki "myDLL.dll" ile bunu göstermektedir:

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

## Win32 Hatalarıyla Başa Çıkma

Birlikte çalışma yöntemlerini kullanırken, API çağrılarınız hakkında ek bilgi almak için **GetLastError** API'sini kullanabilirsiniz.

**DllImport Öznitelik SetLastError Özniteliği**

*SetLastError=true*

Aranan kişinin SetLastError'ı (Win32 API işlevi) arayacağını belirtir.

*SetLastError=yanlış*

Aranan kişinin SetLastError'u (Win32 API işlevi) **çağırmayacağını**, dolayısıyla bir hata bilgisi almayacağınızı belirtir.

* SetLastError ayarlanmadığında false (Varsayılan değer) olarak ayarlanır.

* Hata kodunu Marshal.GetLastWin32Error Yöntemini kullanarak alabilirsiniz:



*Örnek:*

   
    [DllImport("kernel32.dll", SetLastError=true)]
    public static extern IntPtr OpenMutex(uint access, bool handle, string lpName);



Var olmayan mutex'i açmaya çalışıyorsanız, GetLastError **ERROR_FILE_NOT_FOUND** döndürür.



    var lastErrorCode = Marshal.GetLastWin32Error();
    
    if (lastErrorCode == (uint)ERROR_FILE_NOT_FOUND)
    {
        //Deal with error         
    }
Sistem Hata Kodları burada bulunabilir:

https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx


**GetLastError API'si**

Ayrıca kullanabileceğiniz yerel bir **GetLastError** API'si vardır:

    [DllImport("coredll.dll", SetLastError=true)]
    static extern Int32 GetLastError();

* Yönetilen koddan Win32 API'sini çağırırken, her zaman **Marshal.GetLastWin32Error**'u kullanmalısınız.

İşte nedeni:

Hatayı ayarlayan Win32 çağrınız arasında (SetLastError'u çağırır), CLR **SetLastError**'u da çağırabilen diğer Win32 çağrılarını da çağırabilir, bu davranış hata değerinizi geçersiz kılabilir. Bu senaryoda **GetLastError**'u çağırırsanız geçersiz bir hata alabilirsiniz.

**SetLastError = true** ayarı, CLR'nin diğer Win32 çağrılarını yürütmeden önce hata kodunu almasını sağlar.



## Marshal ile yapıları okuma
Marshal sınıfı **PtrToStructure** adında bir fonksiyon içerir, bu fonksiyon bize yapıları yönetilmeyen bir işaretçi ile okuma yeteneği verir.

**PtrToStructure** işlevinde birçok aşırı yüklenme var, ancak hepsinin amacı aynı.

Genel **PtrToStructure**:

    public static T PtrToStructure<T>(IntPtr ptr);

*T* - yapı tipi.

*ptr* - Yönetilmeyen bir bellek bloğuna işaretçi.

Örnek:

    NATIVE_STRUCT result = Marshal.PtrToStructure<NATIVE_STRUCT>(ptr);       

* Native yapıları okurken yönetilen nesnelerle uğraşıyorsanız, nesnenizi sabitlemeyi unutmayın :)

   

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

 





## Com için sınıfı ortaya çıkarmak için basit kod


## Sabitlenmiş Nesne
**GC** (Çöp Toplayıcı) çöpümüzün temizlenmesinden sorumludur.

**GC** çöpümüzü temizlerken, yönetilen yığından yığın parçalanmasına neden olan kullanılmayan nesneleri kaldırır. Kaldırma işlemiyle **GC** yapıldığında, öbek üzerindeki hareketli nesneleri içeren bir yığın sıkıştırması (birleştirme) gerçekleştirir.

**GC** deterministik olmadığı için, yönetilen nesne referansını/işaretçisini yerel koda geçirirken, **GC** herhangi bir zamanda devreye girebilir, eğer Inerop çağrısından hemen sonra gerçekleşirse, nesnenin (hangi referans yerele iletilir) yönetilen öbek üzerinde taşınır - sonuç olarak, yönetilen tarafta geçersiz bir referans alırız.

Bu senaryoda, nesneyi yerel koda geçirmeden önce **sabitlemeniz** gerekir.

**Sabitlenmiş Nesne**

Sabitlenmiş nesne, GC tarafından hareket etmesine izin verilmeyen bir nesnedir.

**Gc Sabitlenmiş Sap**

**Gc.Alloc** yöntemini kullanarak bir pin nesnesi oluşturabilirsiniz.

    GCHandle handle = GCHandle.Alloc(yourObject, GCHandleType.Pinned); 

* Yönetilen nesneye sabitlenmiş bir **GCHandle** elde etmek, belirli bir nesneyi tutamaç serbest bırakılıncaya kadar **GC** tarafından hareket ettirilemeyecek olarak işaretler

Örnek:

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

**Önlemler**

* Nesneyi sabitlerken (özellikle büyük olanlar), sabitlenmiş **GcHandle**'ı mümkün olduğunca hızlı serbest bırakmaya çalışın, çünkü yığın birleştirmeyi keser.
* **GcHandle**'ı serbest bırakmayı unutursanız hiçbir şey olmaz. Güvenli bir kod bölümünde yapın (final gibi)





