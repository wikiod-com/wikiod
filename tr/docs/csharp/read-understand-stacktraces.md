---
title: "Yığın İzlerini Okuyun ve Anlayın"
slug: "ygn-izlerini-okuyun-ve-anlayn"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

Yığın izleme, bir programda hata ayıklarken harika bir yardımcıdır. Programınız bir İstisna oluşturduğunda ve bazen program anormal şekilde sona erdiğinde bir yığın izi alırsınız.

## Windows Forms'da basit bir NullReferenceException için yığın izleme
Bir istisna oluşturan küçük bir kod parçası oluşturalım:

    private void button1_Click(object sender, EventArgs e)
    {
        string msg = null;
        msg.ToCharArray();
    }

Bunu uygularsak, aşağıdaki İstisna ve yığın izini alırız:

    System.NullReferenceException: "Object reference not set to an instance of an object."
       at WindowsFormsApplication1.Form1.button1_Click(Object sender, EventArgs e) in F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:line 29
       at System.Windows.Forms.Control.OnClick(EventArgs e)
       at System.Windows.Forms.Button.OnClick(EventArgs e)
       at System.Windows.Forms.Button.OnMouseUp(MouseEventArgs mevent)

Yığın izlemesi böyle devam eder, ancak bu kısım bizim amacımız için yeterli olacaktır.

Yığın izinin en üstünde şu satırı görüyoruz:

> WindowsFormsApplication1.Form1.button1_Click(Nesne gönderen, EventArgs e) konumunda F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:satır 29

Bu en önemli kısım. Bize İstisnanın oluştuğu _exact_ satırını söyler: Form1.cs içindeki 29. satır.
Yani, aramaya başladığınız yer burasıdır.

İkinci satır

> System.Windows.Forms.Control.OnClick(EventArgs e) adresinde

Bu, `button1_Click` olarak adlandırılan yöntemdir. Artık hatanın meydana geldiği "button1_Click"in "System.Windows.Forms.Control.OnClick"den çağrıldığını biliyoruz.

Şöyle devam edebiliriz; üçüncü satır

> System.Windows.Forms.Button.OnClick(EventArgs e) adresinde

Bu, `System.windows.Forms.Control.OnClick` adlı koddur.

Yığın izleme, kodunuz İstisna ile karşılaşana kadar çağrılan işlevlerin listesidir.
Bunu izleyerek, kodunuzun sorunla karşılaşana kadar hangi yürütme yolunu izlediğini anlayabilirsiniz!

Yığın izlemenin .Net sisteminden gelen çağrıları içerdiğini unutmayın; neyin yanlış gittiğini bulmak için normalde tüm Microsoft `System.Windows.Forms` kodunu izlemeniz gerekmez, yalnızca kendi uygulamanıza ait olan kodu izlemeniz gerekir.


Peki, buna neden "yığın izleme" deniyor?
Çünkü bir program bir metodu her çağırdığında nerede olduğunu takip eder. Son konumunu attığı "yığın" adı verilen bir veri yapısına sahiptir.
Yöntemin yürütülmesi tamamlandıysa, yöntemi çağırmadan önce nerede olduğunu görmek için yığına bakar ve oradan devam eder.

Böylece yığın, yeni bir yöntem çağırmadan önce bilgisayarın nerede kaldığını bilmesini sağlar.

Ancak aynı zamanda bir hata ayıklama yardımı olarak da hizmet eder. Bir suçlunun suçunu işlerken attığı adımları izleyen bir dedektif gibi, bir programcı bir programın çökmeden önce attığı adımları izlemek için yığını kullanabilir.





