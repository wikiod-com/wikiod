---
title: "Appel de plateforme"
slug: "appel-de-plateforme"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Syntaxe
- [DllImport("Exemple.dll")]
static extern void SetText(string inString);
- [DllImport("Exemple.dll")]
static extern void GetText(StringBuilder outString);
- [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)] texte de chaîne ;
- [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)] byte[] byteArr ;
- [StructLayout(LayoutKind.Sequential)] public struct PERSON {...}
- [StructLayout(LayoutKind.Explicit)] public struct MarshaledUnion { [FieldOffset(0)]... }


## Appel d'une fonction dll Win32
    using System.Runtime.InteropServices;

    class PInvokeExample
    {
        [DllImport("user32.dll", CharSet = CharSet.Auto)]
        public static extern uint MessageBox(IntPtr hWnd, String text, String caption, int options);

        public static void test()
        {
            MessageBox(IntPtr.Zero, "Hello!", "Message", 0);
        }
    }

Déclarez une fonction en tant que `static extern` stting `DllImportAttribute` avec sa propriété `Value` définie sur le nom .dll. N'oubliez pas d'utiliser l'espace de noms `System.Runtime.InteropServices`. Appelez-le ensuite comme une méthode statique régulière.

Les services d'invocation de la plate-forme se chargeront de charger le fichier .dll et de trouver la fonction souhaitée. Le P/Invoke dans la plupart des cas simples rassemblera également les paramètres et renverra la valeur vers et depuis le .dll (c'est-à-dire convertir des types de données .NET en types Win32 et vice versa).



## Utilisation de l'API Windows
Utilisez [pinvoke.net](http://pinvoke.net/).

Avant de déclarer une fonction d'API Windows "externe" dans votre code, pensez à la rechercher sur [pinvoke.net](http://pinvoke.net/). Ils ont très probablement déjà une déclaration appropriée avec tous les types de prise en charge et de bons exemples.

## Tableaux de marshalling
**Tableaux de type simple**

    [DllImport("Example.dll")]
    static extern void SetArray(
        [MarshalAs(UnmanagedType.LPArray, SizeConst = 128)]
        byte[] data);

**Tableaux de chaînes**

    [DllImport("Example.dll")]
    static extern void SetStrArray(string[] textLines);


## Structures de marshaling
**Structure simple**

Signature C++ :

    typedef struct _PERSON
    {
        int age;
        char name[32];
    } PERSON, *LP_PERSON;

    void GetSpouse(PERSON person, LP_PERSON spouse);

Définition C#

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct PERSON
    {
        public int age;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string name;
    }

    [DllImport("family.dll", CharSet = CharSet.Auto)]
    public static extern bool GetSpouse(PERSON person, ref PERSON spouse);

** Structure avec des champs de tableau de taille inconnue. De passage**

Signature C++

    typedef struct
    {
        int length;
        int *data;
    } VECTOR;

    void SetVector(VECTOR &vector);

Lorsqu'il est passé du code managé au code non managé, ce

Le tableau `data` doit être défini comme IntPtr et la mémoire doit être explicitement allouée avec [`Marshal.AllocHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal .allochglobal(v=vs.110).aspx) (et libéré avec [`Marshal.FreeHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal. postfaces freehglobal(v=vs.110).aspx) :

    [StructLayout(LayoutKind.Sequential)]
    public struct VECTOR : IDisposable
    {
        int length;
        IntPtr dataBuf;

        public int[] data
        {
            set
            {
                FreeDataBuf();
                if (value != null && value.Length > 0)
                {
                    dataBuf = Marshal.AllocHGlobal(value.Length * Marshal.SizeOf(value[0]));
                    Marshal.Copy(value, 0, dataBuf, value.Length);
                    length = value.Length;
                }
            }
        }
        void FreeDataBuf()
        {
            if (dataBuf != IntPtr.Zero)
            {
                Marshal.FreeHGlobal(dataBuf);
                dataBuf = IntPtr.Zero;
            }
        }
        public void Dispose()
        {
            FreeDataBuf();
        }
    }

    [DllImport("vectors.dll")]
    public static extern void SetVector([In]ref VECTOR vector);

** Structure avec des champs de tableau de taille inconnue. Réception**

Signature C++ :

    typedef struct
    {
        char *name;
    } USER;

    bool GetCurrentUser(USER *user);

Lorsque de telles données sont transmises hors du code non géré et que la mémoire est allouée par les fonctions non gérées, l'appelant géré doit les recevoir dans une variable "IntPrt" et convertir le tampon en un tableau géré. En cas de chaînes, il existe une méthode [`Marshal.PtrToStringAnsi()`](https://msdn.microsoft.com/en-us/library/7b620dhe(v=vs.110).aspx) pratique :

    [StructLayout(LayoutKind.Sequential)]
    public struct USER
    {
        IntPtr nameBuffer;
        public string name { get { return Marshal.PtrToStringAnsi(nameBuffer); } }
    }

    [DllImport("users.dll")]
    public static extern bool GetCurrentUser(out USER user);


## Syndicats de rassemblement
**Champs de type valeur uniquement**

Déclaration C++

    typedef union
    {
        char c;
        int i;
    } CharOrInt;


Déclaration C#

    [StructLayout(LayoutKind.Explicit)]
    public struct CharOrInt
    {
        [FieldOffset(0)]
        public byte c;
        [FieldOffset(0)]
        public int i;
    }

**Mélanger les champs de type valeur et de référence**

Le chevauchement d'une valeur de référence avec un type de valeur un n'est pas autorisé, vous ne pouvez donc pas simplement utiliser le texte <del>`FieldOffset(0) ; FieldOffset(0) i;`</del> ne compilera pas pour

    typedef union
    {
        char text[128];
        int i;
    } TextOrInt;

et généralement, vous devrez utiliser un marshaling personnalisé. Cependant, dans des cas particuliers comme celui-ci, des techniques plus simples peuvent être utilisées :

    [StructLayout(LayoutKind.Sequential)]
    public struct TextOrInt
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)]
        public byte[] text;
        public int i { get { return BitConverter.ToInt32(text, 0); } }
    }


