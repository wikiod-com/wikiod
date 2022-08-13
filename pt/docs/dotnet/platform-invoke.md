---
title: "Chamada de plataforma"
slug: "chamada-de-plataforma"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Sintaxe
- [DllImport("Example.dll")]
static extern void SetText(string inString);
- [DllImport("Example.dll")]
static extern void GetText(StringBuilder outString);
- [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)] texto string;
- [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)] byte[] byteArr;
- [StructLayout(LayoutKind.Sequential)] estrutura pública PESSOA {...}
- [StructLayout(LayoutKind.Explicit)] public struct MarshaledUnion { [FieldOffset(0)]... }


## Chamando uma função dll do Win32
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

Declare uma função como `static extern` stting `DllImportAttribute` com sua propriedade `Value` definida como nome .dll. Não se esqueça de usar o namespace `System.Runtime.InteropServices`. Em seguida, chame-o como um método estático regular.

Os Serviços de Invocação da Plataforma cuidarão de carregar o .dll e encontrar a função desejada. O P/Invoke na maioria dos casos simples também fará o marshaling de parâmetros e retornará o valor de e para o .dll (ou seja, converterá de tipos de dados .NET para Win32 e vice-versa).



## Usando a API do Windows
Use [pinvoke.net](http://pinvoke.net/).

Antes de declarar uma função `extern` da API do Windows em seu código, considere procurá-la em [pinvoke.net](http://pinvoke.net/). Eles provavelmente já têm uma declaração adequada com todos os tipos de suporte e bons exemplos.

## Matrizes de Marshall
**Matrizes do tipo simples**

    [DllImport("Example.dll")]
    static extern void SetArray(
        [MarshalAs(UnmanagedType.LPArray, SizeConst = 128)]
        byte[] data);

**Matrizes de string**

    [DllImport("Example.dll")]
    static extern void SetStrArray(string[] textLines);


## Estruturas de empacotamento
**Estrutura simples**

Assinatura C++:

    typedef struct _PERSON
    {
        int age;
        char name[32];
    } PERSON, *LP_PERSON;

    void GetSpouse(PERSON person, LP_PERSON spouse);

Definição de C#

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct PERSON
    {
        public int age;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string name;
    }

    [DllImport("family.dll", CharSet = CharSet.Auto)]
    public static extern bool GetSpouse(PERSON person, ref PERSON spouse);

**Struct com campos de matriz de tamanho desconhecido. Passando**

Assinatura C++

    typedef struct
    {
        int length;
        int *data;
    } VECTOR;

    void SetVector(VECTOR &vector);

Quando passado de código gerenciado para código não gerenciado, este

A matriz `data` deve ser definida como IntPtr e a memória deve ser explicitamente alocada com [`Marshal.AllocHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal .allochglobal(v=vs.110).aspx) (e liberado com [`Marshal.FreeHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal. freehglobal(v=vs.110).aspx) poswords):

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

**Struct com campos de matriz de tamanho desconhecido. Recebendo**

Assinatura C++:

    typedef struct
    {
        char *name;
    } USER;

    bool GetCurrentUser(USER *user);

Quando esses dados são passados ​​para fora do código não gerenciado e a memória é alocada pelas funções não gerenciadas, o chamador gerenciado deve recebê-los em uma variável `IntPrt` e converter o buffer em um array gerenciado. No caso de strings, existe um método conveniente [`Marshal.PtrToStringAnsi()`](https://msdn.microsoft.com/en-us/library/7b620dhe(v=vs.110).aspx):

    [StructLayout(LayoutKind.Sequential)]
    public struct USER
    {
        IntPtr nameBuffer;
        public string name { get { return Marshal.PtrToStringAnsi(nameBuffer); } }
    }

    [DllImport("users.dll")]
    public static extern bool GetCurrentUser(out USER user);


## Sindicatos de Marshaling
**Somente campos de tipo de valor**

Declaração C++

    typedef union
    {
        char c;
        int i;
    } CharOrInt;


Declaração C#

    [StructLayout(LayoutKind.Explicit)]
    public struct CharOrInt
    {
        [FieldOffset(0)]
        public byte c;
        [FieldOffset(0)]
        public int i;
    }

**Misturando tipo de valor e campos de referência**

A sobreposição de um valor de referência com um tipo de valor um não é permitida, então você não pode simplesmente usar o texto <del>`FieldOffset(0); FieldOffset(0) i;`</del> não irá compilar para

    typedef union
    {
        char text[128];
        int i;
    } TextOrInt;

e geralmente você teria que empregar o empacotamento personalizado. No entanto, em casos particulares como este, técnicas mais simples podem ser usadas:

    [StructLayout(LayoutKind.Sequential)]
    public struct TextOrInt
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)]
        public byte[] text;
        public int i { get { return BitConverter.ToInt32(text, 0); } }
    }


