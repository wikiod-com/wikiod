---
title: "zamanlayıcılar"
slug: "zamanlayclar"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Sözdizimi
- "myTimer.Interval" - "Tick" olayının ne sıklıkta çağrılacağını (milisaniye cinsinden) ayarlar
- `myTimer.Enabled` - zamanlayıcıyı etkinleştirilecek/devre dışı bırakacak şekilde ayarlayan boole değeri
- `myTimer.Start()` - Zamanlayıcıyı başlatır.
- `myTimer.Stop()` - Zamanlayıcıyı durdurur.

Visual Studio kullanıyorsanız, Zamanlayıcılar araç kutusundan doğrudan formunuza bir denetim olarak eklenebilir.

## Çok İş parçacıklı Zamanlayıcılar
`System.Threading.Timer` - En basit çok iş parçacıklı zamanlayıcı. İki yöntem ve bir kurucu içerir.

Örnek:
Bir zamanlayıcı, beşten sonra "çoklu iş parçacığı yürütüldü..." yazan DataWrite yöntemini çağırır.
saniye geçti ve bundan sonra kullanıcı Enter tuşuna basana kadar her saniye:

    using System;
    using System.Threading;
    class Program
    {
      static void Main()
      {
        // First interval = 5000ms; subsequent intervals = 1000ms
        Timer timer = new Timer (DataWrite, "multithread executed...", 5000, 1000);
        Console.ReadLine();
        timer.Dispose(); // This both stops the timer and cleans up.
      }

      static void DataWrite (object data)
      {
        // This runs on a pooled thread
        Console.WriteLine (data); // Writes "multithread executed..."
      }
    }

Not : Çok iş parçacıklı zamanlayıcıları atmak için ayrı bir bölüm yayınlayacaktır.

"Değiştir" - Bu yöntem, zamanlayıcı aralığını değiştirmek istediğinizde çağrılabilir.

`Timeout.Infinite` - Sadece bir kez ateş etmek istiyorsanız. Bunu yapıcının son argümanında belirtin.

`System.Timers` - .NET Framework tarafından sağlanan başka bir zamanlayıcı sınıfı. System.Threading.Timer'ı sarar.

Özellikler:
---------

- `IComponent` - Visual Studio'nun Tasarımcısının bileşen tepsisine yerleştirilmesine izin verilmesi
- "Değiştir" yöntemi yerine "Aralık" özelliği
- Geri arama "temsilcisi" yerine "geçen" "olay"
- Zamanlayıcıyı başlatmak ve durdurmak için "Etkin" özelliği ('varsayılan değer = yanlış')
- 'Etkin' özelliği ile kafanız karışırsa, 'Başlat' ve 'Durdur' yöntemleri (yukarıdaki nokta)
- "Otomatik Sıfırlama" - yinelenen bir olayı belirtmek için ("varsayılan değer = doğru")
- için 'Invoke' ve 'BeginInvoke' yöntemleriyle 'SynchronizingObject' özelliği
WPF öğelerinde ve Windows Forms denetimlerinde güvenli arama yöntemleri


Yukarıdaki tüm özellikleri temsil eden örnek:

    using System;
    using System.Timers; // Timers namespace rather than Threading
    class SystemTimer
    {
      static void Main()
      {
        Timer timer = new Timer(); // Doesn't require any args
        timer.Interval = 500;
        timer.Elapsed += timer_Elapsed; // Uses an event instead of a delegate
        timer.Start(); // Start the timer
        Console.ReadLine();
        timer.Stop(); // Stop the timer
        Console.ReadLine();
        timer.Start(); // Restart the timer
        Console.ReadLine();
        timer.Dispose(); // Permanently stop the timer
     }

     static void timer_Elapsed(object sender, EventArgs e)
     {
       Console.WriteLine ("Tick");
     }
    }

'Çok iş parçacıklı zamanlayıcılar' - birkaç iş parçacığının birçok iş parçacığına hizmet etmesine izin vermek için iş parçacığı havuzunu kullanın
zamanlayıcılar. Bu, geri arama yönteminin veya "Geçen" olayının farklı bir olayda tetiklenebileceği anlamına gelir.
her çağrıldığında iş parçacığı.

"Elapsed" - bu olay, önceki "Elapsed" olayının yürütülmesini bitirip bitirmediğine bakılmaksızın her zaman zamanında tetiklenir. Bu nedenle, geri aramalar veya olay işleyicileri iş parçacığı için güvenli olmalıdır.
Çok iş parçacıklı zamanlayıcıların doğruluğu, işletim sistemine bağlıdır ve tipik olarak
10–20 ms'de.

'birlikte çalışma' - daha fazla doğruluğa ihtiyacınız olduğunda bunu kullanın ve Windows multimedya zamanlayıcısını arayın. Bunun 1 ms'ye kadar doğruluğu vardır ve 'winmm.dll' içinde tanımlanmıştır.

`timeBeginPeriod` - Yüksek zamanlama doğruluğuna ihtiyacınız olduğunu OS'ye bildirmek için önce bunu arayın

`timeSetEvent` - bunu bir multimedya zamanlayıcı başlatmak için `timeBeginPeriod`dan sonra çağırın.

`timeKillEvent` - işiniz bittiğinde bunu çağırın, bu zamanlayıcıyı durdurur

`timeEndPeriod` - İşletim sistemine artık yüksek zamanlama doğruluğuna ihtiyacınız olmadığını bildirmek için bunu arayın.

`dllimport```winmm.dll```timesetevent` anahtar kelimelerini arayarak, multimedya zamanlayıcıyı kullanan İnternet'te eksiksiz örnekler bulabilirsiniz.

## Bir Zamanlayıcı Örneği Oluşturma
Zamanlayıcılar belirli zaman aralıklarında görevleri gerçekleştirmek için kullanılır (Her Y saniyede bir X yapın)
Aşağıda yeni bir Zamanlayıcı örneği oluşturma örneği verilmiştir.

**NOT**: Bu, WinForms kullanan Zamanlayıcılar için geçerlidir. WPF kullanıyorsanız, 'DispatcherTimer'a bakmak isteyebilirsiniz.


        using System.Windows.Forms; //Timers use the Windows.Forms namespace

        public partial class Form1 : Form
        {

            Timer myTimer = new Timer(); //create an instance of Timer named myTimer
   
        
            public Form1()
            {
                InitializeComponent();
            }

        }

## "Tick" olay işleyicisini bir Zamanlayıcıya atama
Bir zamanlayıcıda gerçekleştirilen tüm eylemler "Tick" olayında işlenir.

    public partial class Form1 : Form
    {

        Timer myTimer = new Timer();
   
        
        public Form1()
        {
            InitializeComponent();

            myTimer.Tick += myTimer_Tick; //assign the event handler named "myTimer_Tick"
        }

        private void myTimer_Tick(object sender, EventArgs e)
        {
            // Perform your actions here.
        }
    }

## Örnek: Basit bir geri sayım gerçekleştirmek için bir Zamanlayıcı kullanma.
        public partial class Form1 : Form
        {

        Timer myTimer = new Timer();
        int timeLeft = 10;
        
            public Form1()
            {
                InitializeComponent();
    
                //set properties for the Timer
                myTimer.Interval = 1000;
                myTimer.Enabled = true;
    
                //Set the event handler for the timer, named "myTimer_Tick"
                myTimer.Tick += myTimer_Tick;
    
                //Start the timer as soon as the form is loaded
                myTimer.Start();
    
                //Show the time set in the "timeLeft" variable
                lblCountDown.Text = timeLeft.ToString();
    
            }

            private void myTimer_Tick(object sender, EventArgs e)
            {
                //perform these actions at the interval set in the properties.
                lblCountDown.Text = timeLeft.ToString();
                timeLeft -= 1;
    
                if (timeLeft < 0)
                {
                    myTimer.Stop();
                }
            }
        }

Sonuçlar...

[![buraya resim açıklamasını girin][1]][1][![buraya resim açıklamasını girin][2]][2]

Ve benzeri...


[1]: http://i.stack.imgur.com/VZlnr.png
[2]: http://i.stack.imgur.com/30t8F.png

