---
title: "C#'da SHA1 ile çalışın"
slug: "cda-sha1-ile-calsn"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

bu projede SHA1 kriptografik hash fonksiyonu ile nasıl çalışacağınızı görüyorsunuz. örneğin dizgeden hash alma ve SHA1 hash'inin nasıl kırılacağı.

kaynak github'da tamamlandı:
https://github.com/mahdiabasi/SHA1Tool

## #Bir dosyanın SHA1 sağlama toplamını oluştur
> önce projenize System.Security.Cryptography ad alanı eklersiniz

    public string GetSha1Hash(string filePath)
        {
            using (FileStream fs = File.OpenRead(filePath))
            {
                SHA1 sha = new SHA1Managed();
                return BitConverter.ToString(sha.ComputeHash(fs));
            }
        }

## #Bir metnin karmasını oluştur
      public static string TextToHash(string text)
        {
            var sh = SHA1.Create();
            var hash = new StringBuilder();
            byte[] bytes = Encoding.UTF8.GetBytes(text);
            byte[] b = sh.ComputeHash(bytes);
            foreach (byte a in b)
            {
                var h = a.ToString("x2");
                hash.Append(h);
            }
            return hash.ToString();
        }

