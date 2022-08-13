---
title: "diş açma"
slug: "dis-acma"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Diğer dizilerden form kontrollerine erişme
Bir metin kutusu veya etiket gibi bir kontrolün özniteliğini, kontrolü oluşturan GUI iş parçacığından başka bir iş parçacığından değiştirmek isterseniz, onu çağırmanız gerekir, aksi takdirde şunu belirten bir hata mesajı alabilirsiniz:

> "Çapraz iş parçacığı işlemi geçerli değil: 'kontrol_adı' denetimine, oluşturulduğu iş parçacığı dışındaki bir iş parçacığından erişildi."

Bu örnek kodu bir system.windows.forms formunda kullanmak, bu mesajla bir istisna oluşturacaktır:

    private void button4_Click(object sender, EventArgs e)
    {
        Thread thread = new Thread(updatetextbox);
        thread.Start();
    }

    private void updatetextbox()
    {
        textBox1.Text = "updated"; // Throws exception
    }

Bunun yerine, bir metin kutusunun metnini, kendisine ait olmayan bir iş parçacığı içinden değiştirmek istediğinizde Control.Invoke veya Control.BeginInvoke kullanın. Kontrolü çağırmanın gerekli olup olmadığını kontrol etmek için Control.InvokeRequired'ı da kullanabilirsiniz.

    private void updatetextbox()
    {
        if (textBox1.InvokeRequired)
            textBox1.BeginInvoke((Action)(() => textBox1.Text = "updated"));
        else
            textBox1.Text = "updated";
    }

Bunu sık sık yapmanız gerekiyorsa, bu kontrolü yapmak için gereken kod miktarını azaltmak için çağrılabilir nesneler için bir uzantı yazabilirsiniz:

    public static class Extensions
    {
        public static void BeginInvokeIfRequired(this ISynchronizeInvoke obj, Action action)
        {
            if (obj.InvokeRequired)
                obj.BeginInvoke(action, new object[0]);
            else
                action();
        }
    }

Ve metin kutusunu herhangi bir diziden güncellemek biraz daha basit hale gelir:

    private void updatetextbox()
    {
        textBox1.BeginInvokeIfRequired(() => textBox1.Text = "updated");
    }

Bu örnekte kullanıldığı şekliyle Control.BeginInvoke öğesinin eşzamansız olduğunu unutmayın; bu, bir Control.BeginInvoke çağrısından sonra gelen kodun, geçirilen temsilci henüz yürütülmüş olsun ya da olmasın hemen sonra çalıştırılabileceği anlamına gelir.

Devam etmeden önce textBox1'in güncellendiğinden emin olmanız gerekiyorsa, bunun yerine Control.Invoke kullanın; bu, temsilciniz yürütülene kadar çağrı dizisini engeller. Çok sayıda çağrı çağrısı yaparsanız bu yaklaşımın kodunuzu önemli ölçüde yavaşlatabileceğini ve GUI diziniz çağrı dizisinin tutulan bir kaynağı tamamlamasını veya serbest bırakmasını bekliyorsa uygulamanızı kilitleyeceğini unutmayın.

