---
title: "Düz Metin Düzenleyicisi ve C# Derleyicisi (csc.exe) kullanarak Konsol Uygulaması Oluşturma"
slug: "duz-metin-duzenleyicisi-ve-c-derleyicisi-cscexe-kullanarak-konsol-uygulamas-olusturma"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Düz Metin Düzenleyici ve C# Derleyici kullanarak Konsol uygulaması oluşturma
C# ile yazılmış bir Konsol uygulaması oluşturmak üzere düz metin düzenleyici kullanmak için C# Derleyicisine ihtiyacınız olacaktır. C# Derleyicisi (csc.exe), aşağıdaki konumda bulunabilir:
`%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe`

**N.B.** Sisteminizde yüklü olan .NET Framework sürümüne bağlı olarak, yukarıdaki yolu buna göre değiştirmeniz gerekebilir.


----------

<h1>Kodu Kaydetme</h1>
Bu konunun amacı size bir Konsol uygulamasını <i>nasıl</i> yazacağınızı öğretmek değil, hiçbir şey olmadan bir [tek bir yürütülebilir dosya üretmek için] nasıl <i>derleyeceğinizi</i> öğretmektir. C# Derleyicisi ve herhangi bir Düz Metin Düzenleyicisi (Not Defteri gibi) dışında.
<br/><br/>

1. <kbd>Windows Tuşu</kbd> + <kbd>R</kbd> klavye kısayolunu kullanarak Çalıştır iletişim kutusunu açın.
2. `notepad` yazıp <kbd>Enter</kbd> tuşuna basın
3. Aşağıdaki örnek kodu Not Defteri'ne yapıştırın
4. **Dosya** → **Farklı Kaydet...** seçeneğine gidip 'Dosya Adı' metin alanına 'ConsoleApp.cs' yazıp ardından 'seçeneğini seçerek dosyayı 'ConsoleApp.cs' olarak kaydedin. Dosya türü olarak Tüm Dosyalar'.
5. "Kaydet"i tıklayın

<h1>Kaynak Kodu Derleme</h1>
1. <kbd>Windows Tuşu</kbd> + <kbd>R</kbd><br/> kullanarak Çalıştır iletişim kutusunu açın
2. Girin:

    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe /t:exe /out:"C:\Users\yourUserName\Documents\ConsoleApp.exe" "C:\Users\yourUserName\Documents\ConsoleApp.cs"

Şimdi, 'ConsoleApp.cs' dosyanızı ilk kaydettiğiniz yere dönün. Şimdi yürütülebilir bir dosya ('ConsoleApp.exe') görmelisiniz. Açmak için `ConsoleApp.exe` üzerine çift tıklayın.

Bu kadar! Konsol uygulamanız derlendi. Yürütülebilir bir dosya oluşturuldu ve artık çalışan bir Konsol uygulamanız var.


    using System;
    
    namespace ConsoleApp
    {
        class Program
        {
            private static string input = String.Empty;
    
            static void Main(string[] args)
            {
                goto DisplayGreeting;
    
                DisplayGreeting:
                {
                    Console.WriteLine("Hello! What is your name?");
    
                    input = Console.ReadLine();
    
                    if (input.Length >= 1)
                    {
                        Console.WriteLine(
                            "Hello, " + 
                            input + 
                            ", enter 'Exit' at any time to exit this app.");
    
                        goto AwaitFurtherInstruction;
                    }
                    else
                    {
                        goto DisplayGreeting;
                    }
                }
    
                AwaitFurtherInstruction:
                {
                    input = Console.ReadLine();
    
                    if(input.ToLower() == "exit")
                    {
                        input = String.Empty;
    
                        Environment.Exit(0);
                    }
                    else
                    {
                        goto AwaitFurtherInstruction;
                    }
                }
            }
        }
    }

