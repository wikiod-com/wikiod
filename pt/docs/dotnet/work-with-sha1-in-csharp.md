---
title: "Trabalhar com SHA1 em C#"
slug: "trabalhar-com-sha1-em-c"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

neste projeto você verá como trabalhar com a função hash criptográfica SHA1. por exemplo, obtenha o hash da string e como quebrar o hash SHA1.

fonte completa no github:
https://github.com/mahdiabasi/SHA1Tool

## #Gerar soma de verificação SHA1 de um arquivo
> primeiro você adiciona o namespace System.Security.Cryptography ao seu projeto

    public string GetSha1Hash(string filePath)
        {
            using (FileStream fs = File.OpenRead(filePath))
            {
                SHA1 sha = new SHA1Managed();
                return BitConverter.ToString(sha.ComputeHash(fs));
            }
        }

## #Gerar hash de um texto
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

