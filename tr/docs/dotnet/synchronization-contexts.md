---
title: "Senkronizasyon Bağlamları"
slug: "senkronizasyon-baglamlar"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Bir Senkronizasyon Bağlamı, işin nasıl planlanacağına dair farkındalık gerektirmeden, iş birimlerini bir zamanlayıcıya geçirmek için kod tüketmeye izin veren bir soyutlamadır.

Senkronizasyon bağlamları geleneksel olarak kodun belirli bir iş parçacığında çalıştırılmasını sağlamak için kullanılır. WPF ve Winforms uygulamalarında, sunum çerçevesi tarafından UI iş parçacığını temsil eden bir "SynchronizationContext" sağlanır. Bu şekilde `SynchronizationContext` delegeler için bir üretici-tüketici modeli olarak düşünülebilir. Çalışan iş parçacığı yürütülebilir kodu (temsilci) _üretir ve UI ileti döngüsü tarafından kuyruğa alır veya _consumption_.

Görev Paralel Kitaplığı, senkronizasyon bağlamlarını otomatik olarak yakalamak ve kullanmak için özellikler sağlar.

## Arka plan çalışması gerçekleştirdikten sonra kullanıcı arayüzü iş parçacığında kod yürütün
Bu örnek, bir 'SynchronizationContext' kullanarak bir arka plan iş parçacığından bir UI bileşeninin nasıl güncelleneceğini gösterir.


    void Button_Click(object sender, EventArgs args)
    {
        SynchronizationContext context = SynchronizationContext.Current;
        Task.Run(() =>
        {
            for(int i = 0; i < 10; i++) 
            {
                Thread.Sleep(500); //simulate work being done
                context.Post(ShowProgress, "Work complete on item " + i);
            }
        }
    }

    void UpdateCallback(object state)
    {
        // UI can be safely updated as this method is only called from the UI thread
        this.MyTextBox.Text = state as string;
    }

Bu örnekte, 'for' döngüsü içinde 'MyTextBox.Text'i doğrudan güncellemeye çalışırsanız, bir iş parçacığı hatası alırsınız. 'UpdateCallback' eylemini 'SynchronizationContext'e göndererek, metin kutusu, kullanıcı arayüzünün geri kalanıyla aynı iş parçacığında güncellenir.

Pratikte, ilerleme güncellemeleri bir `System.IPogress<T>` örneği kullanılarak gerçekleştirilmelidir. Varsayılan uygulama "System.Progress<T>", üzerinde oluşturulduğu senkronizasyon bağlamını otomatik olarak yakalar.

