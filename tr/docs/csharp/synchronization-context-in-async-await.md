---
title: "Async-Await'te Senkronizasyon Bağlamı"
slug: "async-awaitte-senkronizasyon-baglam"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Zaman uyumsuz/bekleme anahtar kelimeleri için sözde kod
Basit bir eşzamansız yöntem düşünün:

    async Task Foo()
    {
        Bar();
        await Baz();
        Qux();
    }

Basitleştirmek gerekirse, bu kodun aslında şu anlama geldiğini söyleyebiliriz:

    Task Foo()
    {
        Bar();
        Task t = Baz();
        var context = SynchronizationContext.Current;
        t.ContinueWith(task) =>
        {
            if (context == null)
                Qux();
            else
                context.Post((obj) => Qux(), null);
        }, TaskScheduler.Current);

        return t;
    }

Bu, "async"/"await" anahtar sözcüklerinin, varsa, geçerli senkronizasyon bağlamını kullandığı anlamına gelir. yani UI, Web ve Console uygulamalarında düzgün çalışacak kitaplık kodu yazabilirsiniz.

[Kaynak makale](https://blogs.msdn.microsoft.com/pfxteam/2012/01/20/await-synchronizationcontext-and-console-apps/).

## Senkronizasyon bağlamını devre dışı bırakma
Senkronizasyon içeriğini devre dışı bırakmak için [`ConfigureAwait`](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx) yöntemini çağırmalısınız. :

    async Task() Foo()
    {
        await Task.Run(() => Console.WriteLine("Test"));
    }

    . . .

    Foo().ConfigureAwait(false);

> ConfigureAwait, varsayılan SynchronizationContext yakalama davranışından kaçınmak için bir yol sağlar; flowContext parametresi için false iletmek, beklemeden sonra yürütmeyi sürdürmek için SynchronizationContext'in kullanılmasını engeller.

[Her Şey Senkronizasyon Bağlamıyla İlgili](https://msdn.microsoft.com/en-us/magazine/gg598924.aspx) adlı kaynaktan alıntı.

## SynchronizationContext neden bu kadar önemli?
Bu örneği düşünün:

    private void button1_Click(object sender, EventArgs e)
    {
        label1.Text = RunTooLong();
    }

Bu yöntem, 'RunTooLong' tamamlanana kadar UI uygulamasını donduracaktır. Uygulama yanıt vermeyecektir.

İç kodu eşzamansız olarak çalıştırmayı deneyebilirsiniz:

    private void button1_Click(object sender, EventArgs e)
    {
        Task.Run(() => label1.Text = RunTooLong());
    }

Ancak bu kod yürütülmez çünkü iç gövde UI olmayan iş parçacığında çalıştırılabilir ve [UI özelliklerini doğrudan değiştirmemelidir](https://nnish.com/2010/03/14/accessing-wpf-controls- on-a-ui-thread/):

    private void button1_Click(object sender, EventArgs e)
    {
        Task.Run(() =>
        {
            var label1Text = RunTooLong();

            if (label1.InvokeRequired)
                lable1.BeginInvoke((Action) delegate() { label1.Text = label1Text; });
            else
                label1.Text = label1Text;
        });
    }

Şimdi her zaman bu kalıbı kullanmayı unutmayın. Veya [`SynchronizationContext.Post`](https://lostechies.com/gabrielschenker/2009/01/23/synchronizing-calls-to-the-ui-in-a-multi-threaded-application/) deneyin. senin için yap:

    private void button1_Click(object sender, EventArgs e)
    {
        Task.Run(() =>
        {
            var label1Text = RunTooLong();
            SynchronizationContext.Current.Post((obj) =>
            {
                label1.Text = label1    Text);
            }, null);
        });
    }




