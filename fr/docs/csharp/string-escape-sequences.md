---
title: "Séquences d'échappement de chaîne"
slug: "sequences-dechappement-de-chaine"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

## Syntaxe
- \\' — apostrophe (0x0027)
- \\" — guillemet double (0x0022)
- \\\ — barre oblique inverse (0x005C)
- \0 — nul (0x0000)
- \a — alerte (0x0007)
- \b — retour arrière (0x0008)
- \f — saut de page (0x000C)
- \n — nouvelle ligne (0x000A)
- \r — retour chariot (0x000D)
- \t — tabulation horizontale (0x0009)
- \v — onglet vertical (0x000B)
- \u0000 - \uFFFF — Caractère Unicode
- \x0 - \xFFFF — Caractère Unicode (code de longueur variable)
- \U00000000 - \U0010FFFF — Caractère Unicode (pour générer des substituts)

Les séquences d'échappement de chaîne sont transformées en caractère correspondant au **moment de la compilation**. Les chaînes ordinaires qui contiennent des barres obliques inversées ne sont **pas** transformées.

Par exemple, les chaînes `notEscaped` et `notEscaped2` ci-dessous ne sont pas transformées en un caractère de saut de ligne, mais resteront sous la forme de deux caractères différents (`'\'` et `'n'`).

    string escaped = "\n";
    string notEscaped = "\\" + "n";
    string notEscaped2 = "\\n";

    Console.WriteLine(escaped.Length); // 1
    Console.WriteLine(notEscaped.Length); // 2            
    Console.WriteLine(notEscaped2.Length); // 2

## Échappement des symboles spéciaux dans les littéraux de chaîne
**Barre oblique inversée**

    // The filename will be c:\myfile.txt in both cases
    string filename = "c:\\myfile.txt";
    string filename = @"c:\myfile.txt";

Le deuxième exemple utilise un [littéral de chaîne textuelle](https://www.wikiod.com/fr/docs/c%23/16/verbatim-strings#t=20151122021216101385), qui ne traite pas la barre oblique inverse comme un caractère d'échappement.

**Devis**

    string text = "\"Hello World!\", said the quick brown fox.";
    string verbatimText = @"""Hello World!"", said the quick brown fox.";

Les deux variables contiendront le même texte.

> "Hello World!", dit le renard brun rapide.

**Nouvelles lignes**

Les littéraux de chaîne textuels peuvent contenir des retours à la ligne :

    string text = "Hello\r\nWorld!";
    string verbatimText = @"Hello
    World!";

Les deux variables contiendront le même texte.

## Séquences d'échappement de caractères Unicode
    string sqrt = "\u221A";      // √
    string emoji = "\U0001F601"; // 😁
    string text = "\u0022Hello World\u0022"; // "Hello World"
    string variableWidth = "\x22Hello World\x22"; // "Hello World"

## Échapper aux symboles spéciaux dans les caractères littéraux
**Apostrophes**

    char apostrophe = '\'';
**Barre oblique inversée**

    char oneBackslash = '\\';

## Utilisation de séquences d'échappement dans les identifiants
Les séquences d'échappement ne sont pas limitées aux littéraux `string` et `char`.

Supposons que vous deviez remplacer une méthode tierce :

    protected abstract IEnumerable<Texte> ObtenirŒuvres();

et supposons que le caractère "Œ" n'est pas disponible dans l'encodage de caractères que vous utilisez pour vos fichiers source C#. Vous avez de la chance, il est permis d'utiliser des échappements du type `\u####` ou `\U########` dans ___identifiants___ dans le code. Il est donc légal d'écrire :

    protected override IEnumerable<Texte> Obtenir\u0152uvres()
    {
        // ...
    }

et le compilateur C# saura que `Œ` et `\u0152` sont le même caractère.

(Cependant, il peut être judicieux de passer à UTF-8 ou à un encodage similaire capable de gérer tous les caractères.)

## Les séquences d'échappement non reconnues produisent des erreurs de compilation
Les exemples suivants ne seront pas compilés :

    string s = "\c";
    char c = '\c';

Au lieu de cela, ils produiront l'erreur "Séquence d'échappement non reconnue" au moment de la compilation.

