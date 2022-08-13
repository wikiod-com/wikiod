---
title: "Hafıza yönetimi"
slug: "hafza-yonetimi"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

Yönetilen .NET uygulamalarındaki performans açısından kritik uygulamalar, GC'den ciddi şekilde etkilenebilir. GC çalıştığında, tamamlanana kadar diğer tüm iş parçacıkları askıya alınır. Bu nedenle GC süreçlerinin dikkatli bir şekilde değerlendirilmesi ve çalıştığında nasıl en aza indirileceğinin belirlenmesi önerilir.

## Yönetilmeyen kaynakları sararken SafeHandle kullanın
Yönetilmeyen kaynaklar için sarmalayıcılar yazarken, "IDisposable" ve bir sonlandırıcıyı kendiniz uygulamaya çalışmak yerine "SafeHandle" alt sınıfını seçmelisiniz. "SafeHandle" alt sınıfınız, tutamaç sızıntısı olasılığını en aza indirmek için mümkün olduğunca küçük ve basit olmalıdır. Bu büyük olasılıkla SafeHandle uygulamanızın, onu kullanılabilir bir API sağlamak üzere saran bir sınıfın dahili uygulama ayrıntısı olacağı anlamına gelir. Bu sınıf, bir program "SafeHandle" örneğinizi sızdırsa bile, yönetilmeyen tanıtıcınızın serbest bırakılmasını sağlar.

    using System.Runtime.InteropServices;
    
    class MyHandle : SafeHandle
    {
        public override bool IsInvalid => handle == IntPtr.Zero;
        public MyHandle() : base(IntPtr.Zero, true)
        { }
    
        public MyHandle(int length) : this()
        {
            SetHandle(Marshal.AllocHGlobal(length));
        }

        protected override bool ReleaseHandle()
        {
            Marshal.FreeHGlobal(handle);
            return true;
        }
    }

Sorumluluk Reddi: Bu örnek, sizin için "IDisposable"ı uygulayan ve sonlandırıcıları uygun şekilde yapılandıran "SafeHandle" ile yönetilen bir kaynağın nasıl korunacağını gösterme girişimidir. Bu şekilde bir bellek yığını tahsis etmek çok yapmacık ve muhtemelen anlamsızdır.

## Yönetilmeyen Kaynaklar
GC ve "yığın" hakkında konuştuğumuzda, gerçekten *yönetilen yığın* denen şeyden bahsediyoruz. *yönetilen yığındaki* nesneler, örneğin bir dosyaya yazarken veya dosyadan okurken, yönetilen yığında olmayan kaynaklara erişebilir. Beklenmeyen davranış, bir dosya okumak için açıldığında ve ardından dosya tanıtıcısının normalde olduğu gibi kapanmasını önleyen bir istisna oluştuğunda ortaya çıkabilir. Bu nedenle .NET, yönetilmeyen kaynakların "IDisposable" arabirimini uygulamasını gerektirir. Bu arabirimin parametresiz "Dispose" adlı tek bir yöntemi vardır:

    public interface IDisposable
    {
        Dispose();
    } 

Yönetilmeyen kaynakları işlerken, bunların doğru şekilde atıldığından emin olmalısınız. Bunu, bir "finally" bloğunda açıkça "Dispose()" öğesini çağırarak veya bir "using" ifadesiyle yapabilirsiniz.

    StreamReader sr; 
    string textFromFile;
    string filename = "SomeFile.txt";
    try 
    {
        sr = new StreamReader(filename);
        textFromFile = sr.ReadToEnd();
    }
    finally
    {
        if (sr != null) sr.Dispose();
    }

veya

    string textFromFile;
    string filename = "SomeFile.txt";
    
    using (StreamReader sr = new Streamreader(filename))
    {
        textFromFile = sr.ReadToEnd();
    }

İkincisi tercih edilen yöntemdir ve derleme sırasında otomatik olarak birincisine genişletilir.

