---
title: "Trabajar con SHA1 en C#"
slug: "trabajar-con-sha1-en-c"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

en este proyecto verá cómo trabajar con la función hash criptográfica SHA1. por ejemplo, obtener hash de una cadena y cómo descifrar el hash SHA1.

fuente completa en github:
https://github.com/mahdiabasi/SHA1Tool

## #Generar suma de comprobación SHA1 de un archivo
> primero agrega el espacio de nombres System.Security.Cryptography a su proyecto

    public string GetSha1Hash(string filePath)
        {
            using (FileStream fs = File.OpenRead(filePath))
            {
                SHA1 sha = new SHA1Managed();
                return BitConverter.ToString(sha.ComputeHash(fs));
            }
        }

## #Generar hash de un texto
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

