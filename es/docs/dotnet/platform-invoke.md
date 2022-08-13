---
title: "Invocación de plataforma"
slug: "invocacion-de-plataforma"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Sintaxis
- [ImportaciónDll("Ejemplo.dll")]
static extern void SetText(string inString);
- [ImportaciónDll("Ejemplo.dll")]
vacío externo estático GetText (StringBuilder outString);
- [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)] cadena de texto;
- [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)] byte[] byteArr;
- [StructLayout(LayoutKind.Sequential)] estructura pública PERSONA {...}
- [StructLayout(LayoutKind.Explicit)] estructura pública MarshaledUnion { [FieldOffset(0)]... }


## Llamar a una función Win32 dll
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

Declare una función como `static extern` con `DllImportAttribute` con su propiedad `Value` establecida en el nombre .dll. No olvide usar el espacio de nombres `System.Runtime.InteropServices`. Luego llámelo como un método estático regular.

Los Servicios de Invocación de Plataforma se encargarán de cargar el .dll y encontrar la función deseada. El P/Invoke en la mayoría de los casos simples también ordenará los parámetros y devolverá el valor hacia y desde el .dll (es decir, convertirá los tipos de datos de .NET a los de Win32 y viceversa).



## Uso de la API de Windows
Utilice [pinvoke.net](http://pinvoke.net/).

Antes de declarar una función API de Windows `externa` en su código, considere buscarla en [pinvoke.net](http://pinvoke.net/). Lo más probable es que ya tengan una declaración adecuada con todos los tipos de apoyo y buenos ejemplos.

## Matrices de clasificación
**Arreglos de tipo simple**

    [DllImport("Example.dll")]
    static extern void SetArray(
        [MarshalAs(UnmanagedType.LPArray, SizeConst = 128)]
        byte[] data);

**Matrices de cadenas**

    [DllImport("Example.dll")]
    static extern void SetStrArray(string[] textLines);


## Estructuras de clasificación
**Estructura simple**

Firma C++:

    typedef struct _PERSON
    {
        int age;
        char name[32];
    } PERSON, *LP_PERSON;

    void GetSpouse(PERSON person, LP_PERSON spouse);

Definición de C#

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct PERSON
    {
        public int age;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string name;
    }

    [DllImport("family.dll", CharSet = CharSet.Auto)]
    public static extern bool GetSpouse(PERSON person, ref PERSON spouse);

** Estructura con campos de matriz de tamaño desconocido. Pasando **

firma C++

    typedef struct
    {
        int length;
        int *data;
    } VECTOR;

    void SetVector(VECTOR &vector);

Cuando se pasa de código administrado a código no administrado, este

La matriz `data` debe definirse como IntPtr y la memoria debe asignarse explícitamente con [`Marshal.AllocHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal .allochglobal(v=vs.110).aspx) (y liberado con [`Marshal.FreeHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal. epílogos de freehglobal(v=vs.110).aspx):

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

** Estructura con campos de matriz de tamaño desconocido. Recepción**

Firma C++:

    typedef struct
    {
        char *name;
    } USER;

    bool GetCurrentUser(USER *user);

Cuando dichos datos se pasan fuera del código no administrado y las funciones no administradas asignan memoria, la persona que llama administrada debe recibirlos en una variable `IntPrt` y convertir el búfer en una matriz administrada. En el caso de las cadenas, existe un método conveniente [`Marshal.PtrToStringAnsi()`](https://msdn.microsoft.com/en-us/library/7b620dhe(v=vs.110).aspx):

    [StructLayout(LayoutKind.Sequential)]
    public struct USER
    {
        IntPtr nameBuffer;
        public string name { get { return Marshal.PtrToStringAnsi(nameBuffer); } }
    }

    [DllImport("users.dll")]
    public static extern bool GetCurrentUser(out USER user);


## Unión de mariscales
**Solo campos de tipo de valor**

Declaración de C++

    typedef union
    {
        char c;
        int i;
    } CharOrInt;


Declaración de C#

    [StructLayout(LayoutKind.Explicit)]
    public struct CharOrInt
    {
        [FieldOffset(0)]
        public byte c;
        [FieldOffset(0)]
        public int i;
    }

**Combinar campos de tipo de valor y de referencia**

No se permite superponer un valor de referencia con un valor de tipo uno, por lo que no puede simplemente usar el texto <del>`FieldOffset(0); FieldOffset(0) i;`</del> no compilará para

    typedef union
    {
        char text[128];
        int i;
    } TextOrInt;

y, en general, tendría que emplear la clasificación personalizada. Sin embargo, en casos particulares como este, se pueden utilizar técnicas más simples:

    [StructLayout(LayoutKind.Sequential)]
    public struct TextOrInt
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)]
        public byte[] text;
        public int i { get { return BitConverter.ToInt32(text, 0); } }
    }


