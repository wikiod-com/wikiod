---
title: "Work with SHA1 in C#"
slug: "work-with-sha1-in-c"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

in this project you see how to work with SHA1 cryptographic hash function. for example get hash from string and how to crack SHA1 hash.

source compelete on github:
https://github.com/mahdiabasi/SHA1Tool

## #Generate SHA1 checksum of a file
> first you add  System.Security.Cryptography namespace to your project

    public string GetSha1Hash(string filePath)
        {
            using (FileStream fs = File.OpenRead(filePath))
            {
                SHA1 sha = new SHA1Managed();
                return BitConverter.ToString(sha.ComputeHash(fs));
            }
        }

## #Generate hash of a text
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

