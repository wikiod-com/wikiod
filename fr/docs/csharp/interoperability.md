---
title: "Interopérabilité"
slug: "interoperabilite"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

**Travailler avec l'API Win32 en utilisant C#**

Windows expose de nombreuses fonctionnalités sous la forme de l'API Win32. En utilisant ces API, vous pouvez effectuer une opération directe dans Windows, ce qui augmente les performances de votre application.Source [Cliquez ici][1]


Windows expose une large gamme d'API. Pour obtenir des informations sur les différentes API, vous pouvez consulter des sites tels que [pinvoke][2].


[1] : http://www.c-sharpcorner.com/article/working-with-win32-api-in-net/
[2] : http://pinvoke.net

## Fonction d'importation à partir d'une DLL C++ non gérée
Voici un exemple d'importation d'une fonction définie dans une DLL C++ non managée. Dans le code source C++ de "myDLL.dll", la fonction "add" est définie :

    extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    {
        return a + b;
    }

Ensuite, il peut être inclus dans un programme C# comme suit :

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

Voir [Conventions d'appel](https://www.wikiod.com/fr/docs/c%23/3278/interoperability/16910/calling-conventions#t=201609062059032452959) et [Mangling de noms C++](https://www.wikiod.com/fr/docs/ /c%23/3278/interoperability/16909/c-name-mangling) pour savoir pourquoi `extern "C"` et `__stdcall` sont nécessaires.

# Trouver la bibliothèque dynamique

Lorsque la méthode extern est invoquée pour la première fois, le programme C# recherche et charge la DLL appropriée. Pour plus d'informations sur l'emplacement de la recherche pour trouver la DLL et sur la manière dont vous pouvez influencer les emplacements de recherche, consultez [cette question stackoverflow](http://stackoverflow.com/questions/8836093/how-can-i-specify-a-dllimport -chemin-à-l'exécution).






## Conventions d'appel
Il existe plusieurs conventions d'appel de fonctions, spécifiant qui (appelant ou appelé) extrait les arguments de la pile, comment les arguments sont passés et dans quel ordre. C++ utilise la convention d'appel `Cdecl` par défaut, mais C# attend `StdCall`, qui est généralement utilisé par l'API Windows. Il faut changer l'un ou l'autre :

* Modifiez la convention d'appel en `StdCall` en C++ :

      extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

* Ou changez la convention d'appel en `Cdecl` en C# :

      extern "C" __declspec(dllexport) int /*__cdecl*/ add(int a, int b)
    <!---->
      [DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl)]

Si vous souhaitez utiliser une fonction avec la convention d'appel `Cdecl` et un nom mutilé, votre code ressemblera à ceci :

    __declspec(dllexport) int add(int a, int b)
<!---->
    [DllImport("myDLL.dll", CallingConvention = CallingConvention.Cdecl,
               EntryPoint = "?add@@YAHHH@Z")]

- **thiscall**(**__thiscall**) est principalement utilisé dans les fonctions membres d'une classe.

- Lorsqu'une fonction utilise **thiscall**(**__thiscall**) , un pointeur vers la classe est transmis comme premier paramètre.

## Manipulation de noms C++
Les compilateurs C++ encodent des informations supplémentaires dans les noms des fonctions exportées, telles que les types d'arguments, pour rendre possibles les surcharges avec différents arguments. Ce processus s'appelle [name mangling](https://en.wikipedia.org/wiki/Name_mangling). Cela pose des problèmes d'importation de fonctions en C# (et d'interopérabilité avec d'autres langages en général), car le nom de la fonction `int add(int a, int b)` n'est plus `add`, il peut être `?add@@YAHHH @Z`, `_add@8` ou toute autre chose, selon le compilateur et la convention d'appel.

Il existe plusieurs façons de résoudre le problème de la manipulation des noms :

* Exportation des fonctions à l'aide de `extern "C"` pour passer à la liaison externe C qui utilise la modification des noms C :

      extern "C" __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

    Function name will still be mangled (`_add@8`), but `StdCall`+`extern "C"` name mangling is recognized by C# compiler.

* Spécification des noms de fonction exportés dans le fichier de définition de module `myDLL.def` :

      EXPORTS
        add
    <!---->
      int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll")]

Le nom de la fonction sera pur `add` dans ce cas.

* Importation de nom mutilé. Vous aurez besoin d'une visionneuse de DLL pour voir le nom mutilé, alors vous pouvez le spécifier explicitement :

      __declspec(dllexport) int __stdcall add(int a, int b)
    <!---->
      [DllImport("myDLL.dll", EntryPoint = "?add@@YGHHH@Z")]

## Chargement et déchargement dynamiques de DLL non gérées
Lorsque vous utilisez l'attribut `DllImport`, vous devez connaître la dll et le nom de méthode corrects au *moment de la compilation*. Si vous souhaitez être plus flexible et décider lors de *l'exécution* de la dll et des méthodes à charger, vous pouvez utiliser les méthodes de l'API Windows `LoadLibrary()`, [`GetProcAddress()`](https://msdn.microsoft.com /en-us/library/windows/desktop/ms683212(v=vs.85).aspx) et `FreeLibrary()`. Cela peut être utile si la bibliothèque à utiliser dépend des conditions d'exécution.

Le pointeur renvoyé par `GetProcAddress()` peut être converti en délégué à l'aide de [`Marshal.GetDelegateForFunctionPointer()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal. getdelegateforfunctionpointer(v=vs.110).aspx).

L'exemple de code suivant illustre cela avec le "myDLL.dll" des exemples précédents :

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

## Traitement des erreurs Win32

Lorsque vous utilisez des méthodes d'interopérabilité, vous pouvez utiliser l'API **GetLastError** pour obtenir des informations supplémentaires sur vos appels d'API.

** Attribut DllImport Attribut SetLastError **

*SetLastError=true*

Indique que l'appelé appellera SetLastError (fonction API Win32).

*SetLastError=false*

Indique que l'appelé **n'appellera pas** SetLastError (fonction API Win32), vous n'obtiendrez donc pas d'informations d'erreur.

* Lorsque SetLastError n'est pas défini, il est défini sur false (valeur par défaut).

* Vous pouvez obtenir le code d'erreur en utilisant la méthode Marshal.GetLastWin32Error :



*Exemple:*

   
    [DllImport("kernel32.dll", SetLastError=true)]
    public static extern IntPtr OpenMutex(uint access, bool handle, string lpName);



Si vous essayez d'ouvrir un mutex qui n'existe pas, GetLastError renverra **ERROR_FILE_NOT_FOUND**.



    var lastErrorCode = Marshal.GetLastWin32Error();
    
    if (lastErrorCode == (uint)ERROR_FILE_NOT_FOUND)
    {
        //Deal with error         
    }
Les codes d'erreur système peuvent être trouvés ici :

https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx


**API GetLastError**

Il existe une API **GetLastError** native que vous pouvez également utiliser :

    [DllImport("coredll.dll", SetLastError=true)]
    static extern Int32 GetLastError();

* Lors de l'appel de l'API Win32 à partir du code managé, vous devez toujours utiliser **Marshal.GetLastWin32Error**.

Voici pourquoi:

Entre votre appel Win32 qui définit l'erreur (appels SetLastError), le CLR peut appeler d'autres appels Win32 qui pourraient également appeler **SetLastError**, ce comportement peut remplacer votre valeur d'erreur. Dans ce scénario, si vous appelez **GetLastError**, vous pouvez obtenir une erreur non valide.

Le paramètre **SetLastError = true** garantit que le CLR récupère le code d'erreur avant d'exécuter d'autres appels Win32.



## Lecture des structures avec Marshal
La classe Marshal contient une fonction nommée **PtrToStructure**, cette fonction nous donne la possibilité de lire des structures par un pointeur non géré.

La fonction **PtrToStructure** a reçu de nombreuses surcharges, mais elles ont toutes la même intention.

**PtrVersStructure** générique :

    public static T PtrToStructure<T>(IntPtr ptr);

*T* - type de structure.

*ptr* - Un pointeur vers un bloc de mémoire non géré.

Exemple:

    NATIVE_STRUCT result = Marshal.PtrToStructure<NATIVE_STRUCT>(ptr);       

* Si vous traitez avec des objets gérés lors de la lecture de structures natives, n'oubliez pas d'épingler votre objet :)

   

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

 





## Code simple pour exposer la classe pour com


## Objet épinglé
**GC** (Garbage Collector) est responsable du nettoyage de nos déchets.

Pendant que **GC** nettoie nos ordures, il supprime les objets inutilisés du tas géré qui provoquent la fragmentation du tas. Lorsque **GC** a terminé la suppression, il effectue une compression de tas (défragmentation) qui implique le déplacement d'objets sur le tas.

Étant donné que **GC** n'est pas déterministe, lors du passage de la référence/du pointeur d'objet géré au code natif, **GC** peut démarrer à tout moment, si cela se produit juste après l'appel Inerop, il y a une très bonne possibilité que l'objet (dont la référence est passée à native) sera déplacée sur le tas géré - en conséquence, nous obtenons une référence non valide du côté géré.

Dans ce scénario, vous devez **épingler** l'objet avant de le transmettre au code natif.

**Objet épinglé**

L'objet épinglé est un objet qui n'est pas autorisé à se déplacer par GC.

**Poignée goupillée Gc**

Vous pouvez créer un objet pin en utilisant la méthode **Gc.Alloc**

    GCHandle handle = GCHandle.Alloc(yourObject, GCHandleType.Pinned); 

* L'obtention d'un **GCHandle** épinglé à l'objet géré marque un objet spécifique comme un objet qui ne peut pas être déplacé par **GC**, jusqu'à ce que le handle soit libéré

Exemple:

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

**Précautions**

* Lors de l'épinglage d'objets (en particulier les plus gros), essayez de libérer le **GcHandle** épinglé aussi vite que possible, car il interrompt la défragmentation du tas.
* Si vous oubliez de libérer **GcHandle** rien ne le fera. Faites-le dans une section de code sécurisée (comme finaly)





