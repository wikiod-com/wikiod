---
title: "Windows Form Uygulamasında Kendi Mesaj Kutusunu Oluşturma"
slug: "windows-form-uygulamasnda-kendi-mesaj-kutusunu-olusturma"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Öncelikle MessageBox'ın ne olduğunu bilmemiz gerekiyor...

MessageBox kontrolü, belirtilen metni içeren bir mesaj görüntüler ve özel bir resim, başlık ve düğme setleri belirtilerek özelleştirilebilir (Bu düğme setleri, kullanıcının temel bir evet/hayır yanıtından fazlasını seçmesine izin verir).

Kendi MessageBox'ımızı oluşturarak, sadece oluşturulan dll'yi kullanarak veya sınıfı içeren dosyayı kopyalayarak bu MessageBox Kontrolünü herhangi bir yeni uygulamada yeniden kullanabiliriz.

## Sözdizimi
- 'statik DialogResult sonucu = DialogResult.No; //DialogResult, görevden alındıktan sonra iletişim kutuları tarafından döndürülür.'

## Kendi MessageBox Denetimi Oluşturma.
Kendi MessageBox kontrolümüzü oluşturmak için aşağıdaki kılavuzu takip etmeniz yeterlidir...

1. Visual Studio örneğinizi açın (VS 2008/2010/2012/2015/2017)

2. En üstteki araç çubuğuna gidin ve Dosya -> Yeni Proje --> Windows Forms Uygulaması --> Projeye bir ad verin ve ardından Tamam'a tıklayın.
3. Yüklendikten sonra, Araç Kutusundan (solda bulunur) bir düğme kontrolünü forma (aşağıda gösterildiği gibi) sürükleyip bırakın.

[![buraya resim açıklamasını girin][1]][1]


4. Düğmeye çift tıklayın ve Entegre Geliştirme Ortamı sizin için tıklama olay işleyicisini otomatik olarak oluşturacaktır.

5. Form kodunu aşağıdaki gibi görünecek şekilde düzenleyin (Forma sağ tıklayıp Kodu Düzenle'ye tıklayabilirsiniz):


    namespace MsgBoxExample {
        public partial class MsgBoxExampleForm : Form {
            //Constructor, called when the class is initialised.
            public MsgBoxExampleForm() {
                InitializeComponent();
            }

            //Called whenever the button is clicked.
            private void btnShowMessageBox_Click(object sender, EventArgs e) {
               CustomMsgBox.Show($"I'm a {nameof(CustomMsgBox)}!", "MSG", "OK");
            }
        }
    }

6. Çözüm Gezgini -> Projenize sağ tıklayın --> Ekle --> Windows Formu ve adı "CustomMsgBox.cs" olarak ayarlayın.

7. Araç kutusundan bir düğme ve etiket kontrolünü forma sürükleyin (Yaptıktan sonra aşağıdaki forma benzeyecektir):


[![buraya resim açıklamasını girin][2]][2]

8. Şimdi aşağıdaki kodu yeni oluşturulan formda yazın:


    private DialogResult result = DialogResult.No;
    public static DialogResult Show(string text, string caption, string btnOkText) {
        var msgBox = new CustomMsgBox();
        msgBox.lblText.Text = text; //The text for the label...
        msgBox.Text = caption; //Title of form
        msgBox.btnOk.Text = btnOkText; //Text on the button
        //This method is blocking, and will only return once the user
        //clicks ok or closes the form.
        msgBox.ShowDialog(); 
        return result;
    }

    private void btnOk_Click(object sender, EventArgs e) {
        result = DialogResult.Yes;
        MsgBox.Close();
    }

9. Şimdi sadece F5 Tuşuna basarak programı çalıştırın.
Tebrikler, yeniden kullanılabilir bir kontrol yaptınız.

[1]: https://i.stack.imgur.com/aW1q1.jpg
[2]: https://i.stack.imgur.com/73c1M.jpg









## Başka bir Windows Form uygulamasında kendi oluşturduğunuz MessageBox denetimi nasıl kullanılır?
Mevcut .cs dosyalarınızı bulmak için Visual Studio örneğinizde projeye sağ tıklayın ve Dosya Gezgini'nde Klasörü Aç'a tıklayın.

1. Visual Studio -> Mevcut Projeniz (Windows Form) -> Çözüm Gezgini -> Proje Adı -> Sağ Tık -> Ekle -> Mevcut Öğe -> Ardından mevcut .cs dosyanızı bulun.

2. Şimdi kontrolü kullanmak için yapılacak son bir şey var. Montajınızın bağımlılıklarını bilmesi için kodunuza bir using ifadesi ekleyin.

       using System;
       using System.Collections.Generic;
       using System.ComponentModel;
       using System.Data;
       using System.Drawing;
       .
       .
       .
       using CustomMsgBox; //Here's the using statement for our dependency.

3. MessageBox'ı görüntülemek için aşağıdakileri kullanın ...

    CustomMsgBox.Show("Your Message for Message Box...","MSG","OK");

