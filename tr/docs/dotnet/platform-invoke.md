---
title: "Platform Çağrısı"
slug: "platform-cagrs"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Sözdizimi
- [DllImport("Örnek.dll")]
statik harici void SetText(string inString);
- [DllImport("Örnek.dll")]
statik harici void GetText(StringBuilder outString);
- [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)] dize metni;
- [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)] bayt[] byteArr;
- [StructLayout(LayoutKind.Sequential)] public struct PERSON {...}
- [StructLayout(LayoutKind.Explicit)] public struct MarshaledUnion { [FieldOffset(0)]... }


## Win32 dll işlevini çağırma
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

Bir işlevi, 'Value' özelliği .dll adına ayarlanmış 'DllImportAttribute' olarak 'statik harici' olarak bildirin. `System.Runtime.InteropServices` ad alanını kullanmayı unutmayın. Sonra onu normal bir statik yöntem olarak çağırın.

Platform Çağırma Servisleri, .dll dosyasının yüklenmesi ve istenen fonksiyonun bulunması ile ilgilenecektir. Çoğu basit durumda P/Invoke ayrıca parametreleri sıralar ve .dll'ye ve .dll'den değer döndürür (yani .NET veri türlerini Win32'lere dönüştürür veya tam tersi).



## Windows API'sini Kullanma
[pinvoke.net](http://pinvoke.net/) kullanın.

Kodunuzda bir "harici" Windows API işlevi bildirmeden önce, [pinvoke.net](http://pinvoke.net/) üzerinde aramayı düşünün. Büyük olasılıkla, zaten tüm destekleyici türleri ve iyi örnekleri içeren uygun bir beyana sahiptirler.

## Marshalling dizileri
**Basit türden diziler**

    [DllImport("Example.dll")]
    static extern void SetArray(
        [MarshalAs(UnmanagedType.LPArray, SizeConst = 128)]
        byte[] data);

**Dize dizileri**

    [DllImport("Example.dll")]
    static extern void SetStrArray(string[] textLines);


## Sıralama yapıları
**Basit yapı**

C++ imzası:

    typedef struct _PERSON
    {
        int age;
        char name[32];
    } PERSON, *LP_PERSON;

    void GetSpouse(PERSON person, LP_PERSON spouse);

C# tanımı

    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Ansi)]
    public struct PERSON
    {
        public int age;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string name;
    }

    [DllImport("family.dll", CharSet = CharSet.Auto)]
    public static extern bool GetSpouse(PERSON person, ref PERSON spouse);

**Bilinmeyen boyutta dizi alanlarıyla yapılandırılmış. Geçmek**

C++ imzası

    typedef struct
    {
        int length;
        int *data;
    } VECTOR;

    void SetVector(VECTOR &vector);

Yönetilen koddan yönetilmeyen koda geçildiğinde, bu

"data" dizisi IntPtr olarak tanımlanmalı ve bellek açıkça [`Marshal.AllocHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal) ile ayrılmalıdır. .allochglobal(v=vs.110).aspx) (ve [`Marshal.FreeHGlobal()`](https://msdn.microsoft.com/en-us/library/system.runtime.interopservices.marshal) ile serbest bırakıldı. freehglobal(v=vs.110).aspx) son sözcükleri):

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

**Bilinmeyen boyutta dizi alanlarıyla yapılandırılmış. alma**

C++ imzası:

    typedef struct
    {
        char *name;
    } USER;

    bool GetCurrentUser(USER *user);

Bu tür veriler yönetilmeyen koddan geçirildiğinde ve yönetilmeyen işlevler tarafından bellek tahsis edildiğinde, yönetilen arayan bunu bir 'IntPrt' değişkenine almalı ve arabelleği yönetilen bir diziye dönüştürmelidir. Dizeler için uygun bir [`Marshal.PtrToStringAnsi()`](https://msdn.microsoft.com/en-us/library/7b620dhe(v=vs.110).aspx) yöntemi vardır:

    [StructLayout(LayoutKind.Sequential)]
    public struct USER
    {
        IntPtr nameBuffer;
        public string name { get { return Marshal.PtrToStringAnsi(nameBuffer); } }
    }

    [DllImport("users.dll")]
    public static extern bool GetCurrentUser(out USER user);


## Sıralama birlikleri
**Yalnızca değer türü alanlar**

C++ beyanı

    typedef union
    {
        char c;
        int i;
    } CharOrInt;


C# bildirimi

    [StructLayout(LayoutKind.Explicit)]
    public struct CharOrInt
    {
        [FieldOffset(0)]
        public byte c;
        [FieldOffset(0)]
        public int i;
    }

**Değer türü ve referans alanlarını karıştırma**

Bir referans değeri ile bir değer tipinin çakışmasına izin verilmez, bu nedenle sadece <del>`FieldOffset(0) metnini kullanamazsınız; FieldOffset(0) i;`</del> için derlenmeyecek

    typedef union
    {
        char text[128];
        int i;
    } TextOrInt;

ve genellikle özel sıralamayı kullanmanız gerekir. Ancak, bu gibi özel durumlarda daha basit teknikler kullanılabilir:

    [StructLayout(LayoutKind.Sequential)]
    public struct TextOrInt
    {
        [MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)]
        public byte[] text;
        public int i { get { return BitConverter.ToInt32(text, 0); } }
    }


