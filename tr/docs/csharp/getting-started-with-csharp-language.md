---
title: "C# Dili ile Başlarken"
slug: "c-dili-ile-baslarken"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Yeni bir konsol uygulaması oluşturma (Visual Studio)
1. Visual Studio'yu açın
2. Araç çubuğunda **Dosya** → **Yeni Proje** seçeneğine gidin
3. **Konsol Uygulaması** proje türünü seçin
4. Solution Explorer'da `Program.cs` dosyasını açın
5. `Main()`e aşağıdaki kodu ekleyin:


    public class Program
    {
        public static void Main()
        {
            // Prints a message to the console.
            System.Console.WriteLine("Hello, World!");

            System.Console.ReadKey();
        }
    }

6. Araç çubuğunda, programı çalıştırmak için **Hata Ayıklama** -> **Hata Ayıklamayı Başlat**'a tıklayın veya **F5** veya **ctrl + F5** (hata ayıklayıcı olmadan çalışıyor) tuşlarına basın.


[ideone'da Canlı Demo][1]

-------------------------------------------------- ---------------------------------

# Açıklama

- 'sınıf Programı' bir sınıf bildirimidir. "Program" sınıfı, programınızın kullandığı veri ve yöntem tanımlarını içerir. Sınıflar genellikle birden çok yöntem içerir. Yöntemler sınıfın davranışını tanımlar. Ancak, "Program" sınıfının yalnızca bir yöntemi vardır: "Main".

- 'static void Main()', tüm C# programlarının giriş noktası olan 'Main' yöntemini tanımlar. 'Main' yöntemi, yürütüldüğünde sınıfın ne yaptığını belirtir. Sınıf başına yalnızca bir "Main" yöntemine izin verilir.

- `System.Console.WriteLine("Hello, world!");` yöntemi, verilen bir veriyi (bu örnekte, `Hello, world!`) konsol penceresinde çıktı olarak yazdırır.

- `System.Console.ReadKey()`, mesajın görüntülenmesinden hemen sonra programın kapanmamasını sağlar. Bunu, kullanıcının klavyede bir tuşa basmasını bekleyerek yapar. Kullanıcının herhangi bir tuşa basması programı sonlandıracaktır. Program, `main()` yöntemindeki son kod satırını bitirdiğinde sona erer.

-------------------------------------------------- ---------------------------------

# Komut satırını kullanma

Komut satırı aracılığıyla derlemek için, her ikisi de [Microsoft Build Tools]'un (https://www.visualstudio.com/downloads/download-visual-) parçası olan 'MSBuild' veya 'csc.exe' _(C# derleyicisi)_ kullanın. studio-vs#d-build-tools) paketi.

Bu örneği derlemek için, "HelloWorld.cs" dosyasının bulunduğu dizinde aşağıdaki komutu çalıştırın:

<!-- dil: lang-none -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs

Bir uygulama içinde iki ana yönteminiz olması da mümkün olabilir. Bu durumda, derleyiciye **konsol**'a aşağıdaki komutu yazarak hangi ana yöntemi çalıştıracağını söylemelisiniz. ad alanı)

<!-- dil: lang-none -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs /main:HelloWorld.ClassA 

HelloWorld'ün ad alanı olduğu yer


***Not**: **.NET framework v4.0**'ın genel olarak bulunduğu yoldur. Yolu .NET sürümünüze göre değiştirin. Ayrıca, 32 bit .NET Framework kullanıyorsanız dizin **framework64** yerine **framework** olabilir. Windows Komut İsteminden, aşağıdaki komutları çalıştırarak tüm csc.exe Çerçeve yollarını listeleyebilirsiniz (32-bit Çerçeveler için ilk):*

    dir %WINDIR%\Microsoft.NET\Framework\csc.exe /s/b
    dir %WINDIR%\Microsoft.NET\Framework64\csc.exe /s/b

![.cs dosyasını derleme[2]

Şimdi aynı dizinde `HelloWorld.exe` adlı yürütülebilir bir dosya olmalıdır. Programı komut isteminden çalıştırmak için yürütülebilir dosyanın adını yazmanız ve aşağıdaki gibi <kbd>Enter</kbd> tuşuna basmanız yeterlidir:

<!-- dil: lang-none -->
    HelloWorld.exe

Bu üretecek:

>Merhaba dünya!

![exe dosyasını konsolda yürütme][3]

Ayrıca yürütülebilir dosyayı çift tıklatabilir ve "**Merhaba dünya!**" mesajıyla yeni bir konsol penceresi açabilirsiniz.

![Yürütülebilir dosyayı çalıştırma ve çift tıklamayı kullanma][4]

[1]: https://ideone.com/3OhmnG
[2]: http://i.stack.imgur.com/xT8kk.png
[3]: http://i.stack.imgur.com/x0Fek.png
[4]: http://i.stack.imgur.com/qstu1.png

## Visual Studio'da (konsol uygulaması) yeni bir proje oluşturma ve Hata Ayıklama modunda çalıştırma
1. **[Visual Studio][1]** dosyasını indirin ve kurun. Visual Studio, [VisualStudio.com][2] adresinden indirilebilir. Topluluk sürümü, ilk olarak ücretsiz olduğu için, ikinci olarak da tüm genel özellikleri içerdiği ve daha da genişletilebildiği için önerilmektedir.

2. **Visual Studio'yu açın.**
3. **Hoş geldiniz.** **Dosya → **Yeni** → Proje**'ye gidin.
    [![Microsoft Visual Studio - File Menu][3]][3]

4. **Şablonlar** → **Visual C#** → **Konsol Uygulaması**'na tıklayın

    [![Microsoft Visual Studio - New Project window][4]][4]

5. **Konsol Uygulaması'nı seçtikten sonra,** Projeniz için bir ad ve kaydedilecek bir konum girin ve <kbd>Tamam</kbd>'a basın. Çözüm adı hakkında endişelenmeyin.

6. **Proje oluşturuldu**. Yeni oluşturulan proje şuna benzer:

    [![Microsoft Visual Studio - c# Default Project][5]][5]

    _(Always use descriptive names for projects so that they can easily be distinguished from other projects.  It is recommended not to use spaces in project or class name.)_

7. **Kod yazın.** Artık `Program.cs` dosyanızı "Merhaba dünya!" sunacak şekilde güncelleyebilirsiniz. kullanıcıya.

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                }
            }
        }

    Add the following two lines to the `public static void Main(string[] args)` object in `Program.cs`: (make sure it's inside the braces)

        Console.WriteLine("Hello world!");
        Console.Read();

    **Why** `Console.Read()`__?__ The first line prints out the text "Hello world!" to the console, and the second line waits for a single character to be entered; in effect, this causes the program to pause execution so that you're able to see the output while debugging.  Without `Console.Read();`, when you start debugging the application it will just print "Hello world!" to the console and then immediately close.  Your code window should now look like the following:

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                    Console.WriteLine("Hello world!");
                    Console.Read();
                }
            }
        }

8. **Programınızın hatalarını ayıklayın.** Pencerenin üst kısmına yakın bir yerde bulunan araç çubuğundaki Başlat Düğmesine [![Hata Ayıklamayı Başlat Düğmesi][6]][6] veya klavyenizde <kbd>F5</kbd> tuşuna basın uygulamanızı çalıştırmak için Düğme yoksa, programı üst menüden çalıştırabilirsiniz: **Hata Ayıkla → Hata Ayıklamayı Başlat**. Program derlenecek ve ardından bir konsol penceresi açacaktır. Aşağıdaki ekran görüntüsüne benzer görünmelidir:

[![Hello World uygulamasını çalıştıran konsol][7]][7]

9. **Programı durdurun.** Programı kapatmak için klavyenizdeki herhangi bir tuşa basmanız yeterlidir. Eklediğimiz `Console.Read()` aynı amaç içindi. Programı kapatmanın başka bir yolu da <kbd>Başlat</kbd> düğmesinin bulunduğu menüye gidip <kbd>Durdur</kbd> düğmesini tıklamaktır.

     


[1]: https://www.visualstudio.com/products/vs-2015-product-editions
[2]: http://www.visualstudio.com
[3]: http://i.stack.imgur.com/fpvTX.png
[4]: http://i.stack.imgur.com/kKGls.png
[5]: http://i.stack.imgur.com/WVkeF.png
[6]: https://i.stack.imgur.com/odDu6.png
[7]: http://i.stack.imgur.com/ZD5MF.png

## Mono kullanarak yeni bir program oluşturma
İlk olarak, [kurulum bölümünde[2] açıklandığı gibi, seçtiğiniz platform için kurulum talimatlarını izleyerek [Mono][1] kurun.

Mono; Mac OS X, Windows ve Linux için kullanılabilir.

Kurulum tamamlandıktan sonra, bir metin dosyası oluşturun, ona `HelloWorld.cs` adını verin ve aşağıdaki içeriği içine kopyalayın:

    public class Program
    {
        public static void Main()
        {
            System.Console.WriteLine("Hello, world!");
            System.Console.WriteLine("Press any key to exit..");
            System.Console.Read();
        }
    }


Windows kullanıyorsanız, Mono kurulumunda bulunan ve gerekli ortam değişkenlerinin ayarlanmasını sağlayan Mono Komut İstemi'ni çalıştırın. Mac veya Linux kullanıyorsanız, yeni bir terminal açın.

Yeni oluşturulan dosyayı derlemek için, `HelloWorld.cs` dosyasını içeren dizinde aşağıdaki komutu çalıştırın:

<!-- dil: lang-none -->
    mcs -out:HelloWorld.exe HelloWorld.cs
 

Ortaya çıkan 'HelloWorld.exe' daha sonra aşağıdakilerle yürütülebilir:
 
<!-- dil: lang-none -->
    mono HelloWorld.exe
 
çıktıyı üretecek olan:
 
 
<!-- dil: lang-none -->
    Hello, world!   
    Press any key to exit..

 
[1]: http://www.mono-project.com/
[2]: http://www.mono-project.com/docs/getting-started/install/

## .NET Core kullanarak yeni bir program oluşturma
Önce [**.NET Core SDK**][1]'yi, seçtiğiniz platform için kurulum talimatlarını gözden geçirerek kurun:

- [Pencereler][2]
- [OSX][3]
- [Linux][4]
- [Docker][5]

Kurulum tamamlandıktan sonra bir komut istemi veya terminal penceresi açın.

1. `mkdir hello_world` ile yeni bir dizin oluşturun ve `cd hello_world` ile yeni oluşturulan dizine geçin.

2. `dotnet new console` ile yeni bir konsol uygulaması oluşturun.
Bu iki dosya üretecektir:

    - **hello_world.csproj**

          <Project Sdk="Microsoft.NET.Sdk">

            <PropertyGroup>
              <OutputType>Exe</OutputType>
              <TargetFramework>netcoreapp1.1</TargetFramework>
            </PropertyGroup>

          </Project>
          
    - **Program.cs**

          using System;
        
          namespace hello_world
          {
              class Program
              {
                  static void Main(string[] args)
                  {
                      Console.WriteLine("Hello World!");
                  }
              }
          }

3. Gerekli paketleri `dotnet restore` ile geri yükleyin.

4. *Opsiyonel* Uygulamayı Hata Ayıklama için "dotnet build" veya Release için "dotnet build -c Release" ile derleyin. 'dotnet run' ayrıca derleyiciyi çalıştıracak ve varsa derleme hataları verecektir.

5. Uygulamayı, Hata Ayıklama için "dotnet run" veya Sürüm için "dotnet run .\bin\Release\netcoreapp1.1\hello_world.dll" ile çalıştırın.

-------------------------------------------------- ---------------------------------

Komut İstemi çıktısı
---------------------
[![buraya resim açıklamasını girin][6]][6]


[1]: https://docs.microsoft.com/en-us/dotnet/articles/core/
[2]: https://www.microsoft.com/net/core#windows
[3]: https://www.microsoft.com/net/core#macos
[4]: https://www.microsoft.com/net/core#linuxubuntu
[5]: https://www.microsoft.com/net/core#dockercmd
[6]: https://i.stack.imgur.com/arqCl.png


## LinqPad kullanarak yeni bir sorgu oluşturma
LinqPad, .Net dillerinin (C#, F# ve VB.Net) özelliklerini öğrenmenizi ve test etmenizi sağlayan harika bir araçtır.

1. [LinqPad][1]'i yükleyin
2. Yeni bir Sorgu oluşturun (<kbd>Ctrl</kbd> + <kbd>N</kbd>)
[![buraya resim açıklamasını girin][2]][2]
3. Dil altında "C# deyimleri"ni seçin
[![buraya resim açıklamasını girin][3]][3]
4. Aşağıdaki kodu yazın ve çalıştır'a basın (<kbd>F5</kbd>)

        string hw = "Hello World";

        hw.Dump(); //or Console.WriteLine(hw);
[![buraya resim açıklamasını girin][4]][4]

5. Sonuç ekranında "Merhaba Dünya" yazısının çıktısını görmelisiniz.
[![buraya resim açıklamasını girin][5]][5]
6. Artık ilk .Net programınızı oluşturduğunuza göre, gidin ve "Samples" tarayıcısı aracılığıyla LinqPad'de bulunan örnekleri inceleyin. .Net dillerinin birçok farklı özelliğini size gösterecek birçok harika örnek var.
[![buraya resim açıklamasını girin][6]][6]

**Notlar:**
1. "IL" üzerine tıklarsanız, .net kodunuzun oluşturduğu IL kodunu inceleyebilirsiniz. Bu harika bir öğrenme aracıdır.
[![buraya resim açıklamasını girin][7]][7]
2. 'LINQ to SQL' veya 'Linq to Entities' kullanırken, oluşturulan SQL'i inceleyebilirsiniz; bu, LINQ hakkında bilgi edinmenin başka bir harika yoludur.


[1]: http://www.linqpad.net/
[2]: http://i.stack.imgur.com/D0tSi.png
[3]: http://i.stack.imgur.com/kC5Ur.jpg
[4]: http://i.stack.imgur.com/LO4kD.jpg
[5]: http://i.stack.imgur.com/GzsrS.jpg
[6]: http://i.stack.imgur.com/yucuf.jpg
[7]: http://i.stack.imgur.com/XPumO.jpg

## Xamarin Studio kullanarak yeni bir proje oluşturma
1. [Xamarin Studio Topluluğu][1]'nu indirin ve kurun.
2. Xamarin Studio'yu açın.
3. **Dosya** → **Yeni** → **Çözüm**'e tıklayın.

[![Xamarin Studio'da Yeni Proje Oluşturma][2]][2]

4. **.NET** → **Konsol Projesi**'ne tıklayın ve **C#**'yi seçin.
5. Devam etmek için <kbd>İleri</kbd>'yi tıklayın.

[![Yeni proje için Şablon Seçme][3]][3]
 
6. Kaydedilecek bir **Konum** için **Proje Adı** ve <kbd>Gözat...</kbd>'yi girin ve ardından <kbd>Oluştur</kbd>'u tıklayın.

[![Proje adı ve yeri][4]][4]

7. Yeni oluşturulan proje şuna benzer:

[![buraya resim açıklamasını girin][5]][5]

8. Metin Düzenleyicideki kod şudur:


    using System;
    
    namespace FirstCsharp
    {
        public class MainClass
        {
            public static void Main(string[] args)
            {
                Console.WriteLine("Hello World!");
                Console.ReadLine();
            }
        }
    }

9. Kodu çalıştırmak için <kbd>F5</kbd>'ye basın veya aşağıda gösterildiği gibi **Oynat Düğmesine** tıklayın:

[![Kodu çalıştırın][6]][6]

10. Çıktı aşağıdadır:

[![çıktı][7]][7]


[1]: https://store.xamarin.com/
[2]: http://i.stack.imgur.com/hHjMM.png
[3]: http://i.stack.imgur.com/s58Ju.png
[4]: http://i.stack.imgur.com/lrK8L.png
[5]: http://i.stack.imgur.com/vva82.png
[6]: http://i.stack.imgur.com/6q4ZN.png
[7]: http://i.stack.imgur.com/cqBsK.png

