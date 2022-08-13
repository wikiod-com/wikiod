---
title: "Travailler avec SHA1 en C#"
slug: "travailler-avec-sha1-en-c"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

dans ce projet, vous voyez comment travailler avec la fonction de hachage cryptographique SHA1. par exemple, obtenir le hachage de la chaîne et comment craquer le hachage SHA1.

source complète sur github :
https://github.com/mahdiabasi/SHA1Tool

## #Générer la somme de contrôle SHA1 d'un fichier
> vous ajoutez d'abord l'espace de noms System.Security.Cryptography à votre projet

    public string GetSha1Hash(string filePath)
        {
            using (FileStream fs = File.OpenRead(filePath))
            {
                SHA1 sha = new SHA1Managed();
                return BitConverter.ToString(sha.ComputeHash(fs));
            }
        }

## #Générer le hachage d'un texte
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

