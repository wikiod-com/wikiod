---
title: "Gestion de la mémoire"
slug: "gestion-de-la-memoire"
draft: false
images: []
weight: 9925
type: docs
toc: true
---

Les applications critiques pour les performances dans les applications .NET gérées peuvent être gravement affectées par le GC. Lorsque le GC s'exécute, tous les autres threads sont suspendus jusqu'à ce qu'il se termine. Pour cette raison, il est recommandé d'évaluer soigneusement les processus GC et de déterminer comment minimiser leur exécution.

## Utiliser SafeHandle lors de l'encapsulation de ressources non gérées
Lorsque vous écrivez des wrappers pour des ressources non gérées, vous devez sous-classer `SafeHandle` plutôt que d'essayer d'implémenter `IDisposable` et un finaliseur vous-même. Votre sous-classe `SafeHandle` doit être aussi petite et simple que possible pour minimiser les risques de fuite de la poignée. Cela signifie probablement que votre implémentation SafeHandle serait un détail d'implémentation interne d'une classe qui l'enveloppe pour fournir une API utilisable. Cette classe garantit que, même si un programme fuit votre instance `SafeHandle`, votre handle non géré est libéré.

    using System.Runtime.InteropServices;
    
    class MyHandle : SafeHandle
    {
        public override bool IsInvalid => handle == IntPtr.Zero;
        public MyHandle() : base(IntPtr.Zero, true)
        { }
    
        public MyHandle(int length) : this()
        {
            SetHandle(Marshal.AllocHGlobal(length));
        }

        protected override bool ReleaseHandle()
        {
            Marshal.FreeHGlobal(handle);
            return true;
        }
    }

Clause de non-responsabilité : cet exemple tente de montrer comment protéger une ressource gérée avec `SafeHandle` qui implémente `IDisposable` pour vous et configure les finaliseurs de manière appropriée. Il est très artificiel et probablement inutile d'allouer un morceau de mémoire de cette manière.

## Ressources non gérées
Lorsque nous parlons du GC et du "tas", nous parlons en fait de ce qu'on appelle le *tas géré*. Les objets sur le *tas géré* peuvent accéder à des ressources qui ne se trouvent pas sur le tas géré, par exemple, lors de l'écriture ou de la lecture d'un fichier. Un comportement inattendu peut se produire lorsqu'un fichier est ouvert en lecture, puis une exception se produit, empêchant le descripteur de fichier de se fermer normalement. Pour cette raison, .NET exige que les ressources non gérées implémentent l'interface "IDisposable". Cette interface possède une seule méthode appelée "Dispose" sans paramètres :

    public interface IDisposable
    {
        Dispose();
    } 

Lorsque vous manipulez des ressources non gérées, vous devez vous assurer qu'elles sont correctement supprimées. Vous pouvez le faire en appelant explicitement `Dispose()` dans un bloc `finally` ou avec une instruction `using`.

    StreamReader sr; 
    string textFromFile;
    string filename = "SomeFile.txt";
    try 
    {
        sr = new StreamReader(filename);
        textFromFile = sr.ReadToEnd();
    }
    finally
    {
        if (sr != null) sr.Dispose();
    }

ou

    string textFromFile;
    string filename = "SomeFile.txt";
    
    using (StreamReader sr = new Streamreader(filename))
    {
        textFromFile = sr.ReadToEnd();
    }

Cette dernière est la méthode préférée et est automatiquement étendue à la première lors de la compilation.

