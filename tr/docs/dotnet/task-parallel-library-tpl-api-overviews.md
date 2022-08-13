---
title: "Görev Paralel Kitaplığı (TPL) API'sine Genel Bakış"
slug: "gorev-paralel-kitaplg-tpl-apisine-genel-baks"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

Görev Paralel Kitaplığı, bir uygulamaya paralellik ve eşzamanlılık ekleme sürecini önemli ölçüde basitleştiren genel türler ve API'ler kümesidir. .Ağ. TPL, .Net 4'te tanıtıldı ve çok iş parçacıklı ve paralel kod yazmanın önerilen yoludur.

TPL, programcının ortak düşük seviyeli ayrıntılara zaman harcamak yerine sorunları çözmeye odaklanabilmesi için iş planlaması, iş parçacığı benzeşimi, iptal desteği, durum yönetimi ve yük dengeleme ile ilgilenir.

## Bir düğme tıklamasına yanıt olarak çalışma gerçekleştirin ve kullanıcı arayüzünü güncelleyin
Bu örnek, bir çalışan iş parçacığı üzerinde bazı çalışmalar gerçekleştirerek bir düğme tıklamasına nasıl yanıt verebileceğinizi ve ardından tamamlandığını belirtmek için kullanıcı arabirimini güncelleyebileceğinizi gösterir.

    void MyButton_OnClick(object sender, EventArgs args)
    {
        Task.Run(() => // Schedule work using the thread pool
            {
                System.Threading.Thread.Sleep(5000); // Sleep for 5 seconds to simulate work.
            })
        .ContinueWith(p => // this continuation contains the 'update' code to run on the UI thread
        {
            this.TextBlock_ResultText.Text = "The work completed at " + DateTime.Now.ToString()
        },
        TaskScheduler.FromCurrentSynchronizationContext()); // make sure the update is run on the UI thread.
    
    }


