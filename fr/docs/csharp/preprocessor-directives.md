---
title: "Directives du préprocesseur"
slug: "directives-du-preprocesseur"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Syntaxe
- #define *[symbol]* // Définit un symbole de compilateur.
- #undef *[symbol]* // Dédéfinit un symbole de compilateur.
- #warning *[warning message]* // Génère un avertissement du compilateur. Utile avec #if.
- #error *[message d'erreur]* // Génère une erreur de compilation. Utile avec #if.
- #line *[numéro de ligne] (nom de fichier)* // Remplace le numéro de ligne du compilateur (et éventuellement le nom du fichier source). Utilisé avec [modèles de texte T4](https://msdn.microsoft.com/en-us/library/bb126445.aspx).
- #pragma warning [disable|restore] *[warning numbers]* // Désactive/restaure les avertissements du compilateur.
- #pragma checksum "*[filename]*" "*[guid]*" "*[checksum]*" // Valide le contenu d'un fichier source.
- #region *[nom de la région]* // Définit une région de code réductible.
- #endregion // Termine un bloc de région de code.
- #if *[condition]* // Exécute le code ci-dessous si la condition est vraie.
- #else // Utilisé après un #if.
- #elif *[condition]* // Utilisé après un #if.
- #endif // Termine un bloc conditionnel commencé par #if.

Les directives de préprocesseur sont généralement utilisées pour rendre les programmes source faciles à modifier et faciles à compiler dans différents environnements d'exécution. Les directives du fichier source indiquent au préprocesseur d'effectuer des actions spécifiques. Par exemple, le préprocesseur peut remplacer des jetons dans le texte, insérer le contenu d'autres fichiers dans le fichier source ou supprimer la compilation d'une partie du fichier en supprimant des sections de texte. Les lignes de préprocesseur sont reconnues et exécutées avant l'expansion macro. Par conséquent, si une macro se développe en quelque chose qui ressemble à une commande de préprocesseur, cette commande n'est pas reconnue par le préprocesseur.
 
Les instructions du préprocesseur utilisent le même jeu de caractères que les instructions du fichier source, à l'exception que les séquences d'échappement ne sont pas prises en charge. Le jeu de caractères utilisé dans les instructions du préprocesseur est le même que le jeu de caractères d'exécution. Le préprocesseur reconnaît également les valeurs de caractère négatives.

## Expressions conditionnelles

Les expressions conditionnelles (`#if`, `#elif`, etc.) prennent en charge un sous-ensemble limité d'opérateurs booléens. Elles sont:

- `==` et `!=`. Ceux-ci ne peuvent être utilisés que pour tester si le symbole est vrai (défini) ou faux (non défini)
- `&&`, `||`, `!`
- `()`

Par exemple:

    #if !DEBUG && (SOME_SYMBOL || SOME_OTHER_SYMBOL) && RELEASE == true
    Console.WriteLine("OK!");
    #endif

compilerait le code qui affiche "OK!" à la console si `DEBUG` n'est pas défini, `SOME_SYMBOL` ou `SOME_OTHER_SYMBOL` est défini, et `RELEASE` est défini.

Remarque : Ces substitutions sont effectuées _au moment de la compilation_ et ne sont donc pas disponibles pour inspection au moment de l'exécution. Le code éliminé par l'utilisation de `#if` ne fait pas partie de la sortie du compilateur.

Voir aussi : [Directives de préprocesseur C#](https://msdn.microsoft.com/en-us/library/ed8yd1ha.aspx) sur MSDN.


## Expressions conditionnelles
Lorsque ce qui suit est compilé, il renverra une valeur différente selon les directives définies.

    // Compile with /d:A or /d:B to see the difference
    string SomeFunction() 
    {
    #if A
        return "A";
    #elif B
        return "B";
    #else
        return "C";
    #endif
    }

Les expressions conditionnelles sont généralement utilisées pour enregistrer des informations supplémentaires pour les versions de débogage.

    void SomeFunc()
    {
        try
        {
            SomeRiskyMethod();
        }
        catch (ArgumentException ex)
        {
            #if DEBUG
            log.Error("SomeFunc", ex);
            #endif

            HandleException(ex);
        }
    }



## Autres instructions du compilateur
# Ligne

`#line` contrôle le numéro de ligne et le nom de fichier signalés par le compilateur lors de la sortie des avertissements et des erreurs.

    void Test()
    {
        #line 42 "Answer"
        #line filename "SomeFile.cs"
        int life; // compiler warning CS0168 in "SomeFile.cs" at Line 42
        #line default
        // compiler warnings reset to default
    }

# Somme de contrôle Pragma

`#pragma checksum` permet la spécification d'une somme de contrôle spécifique pour une base de données de programme générée (PDB) pour le débogage.

    #pragma checksum "MyCode.cs" "{00000000-0000-0000-0000-000000000000}" "{0123456789A}"

## Définir et indéfinir les symboles
Un symbole de compilateur est un mot-clé défini au moment de la compilation qui peut être vérifié pour exécuter de manière conditionnelle des sections de code spécifiques.
 
Il existe trois façons de définir un symbole de compilateur. Ils peuvent être définis via le code :
 
    #define MYSYMBOL
 
Ils peuvent être définis dans Visual Studio, sous Project Properties > Build > Conditional Compilation Symbols :
 
![Symboles du compilateur VS](http://i.imgur.com/PHG04dI.png)
 
*(Notez que `DEBUG` et `TRACE` ont leurs propres cases à cocher et n'ont pas besoin d'être spécifiés explicitement.)*
 
Ou ils peuvent être définis au moment de la compilation à l'aide du commutateur `/define:[nom]` sur le compilateur C#, `csc.exe`.

Vous pouvez également indéfinir des symboles en utilisant la directive `#undefine`.
 
L'exemple le plus répandu est le symbole `DEBUG`, qui est défini par Visual Studio lorsqu'une application est compilée en mode Debug (par rapport au mode Release).
 
    public void DoBusinessLogic()
    {
        try
        {
            AuthenticateUser();
            LoadAccount();
            ProcessAccount();
            FinalizeTransaction();
        }
        catch (Exception ex)
        {
    #if DEBUG
            System.Diagnostics.Trace.WriteLine("Unhandled exception!");
            System.Diagnostics.Trace.WriteLine(ex);
            throw;
    #else
            LoggingFramework.LogError(ex);
            DisplayFriendlyErrorMessage();
    #endif
        }
    }
 
Dans l'exemple ci-dessus, lorsqu'une erreur se produit dans la logique métier de l'application, si l'application est compilée en mode débogage (et que le symbole `DEBUG` est défini), l'erreur sera écrite dans le journal de suivi et l'exception sera être relancé pour le débogage. Cependant, si l'application est compilée en mode Release (et qu'aucun symbole `DEBUG` n'est défini), une infrastructure de journalisation est utilisée pour consigner discrètement l'erreur et un message d'erreur convivial s'affiche pour l'utilisateur final.

## Blocs de région
Utilisez `#region` et `#endregion` pour définir une région de code réductible.
 
    #region Event Handlers
 
    public void Button_Click(object s, EventArgs e)
    {
        // ...
    }
 
    public void DropDown_SelectedIndexChanged(object s, EventArgs e)
    {
        // ...
    }
 
    #endregion
 
Ces directives ne sont utiles que lorsqu'un IDE prenant en charge les régions réductibles (comme [Visual Studio](https://www.visualstudio.com/en-us/visual-studio-homepage-vs.aspx)) est utilisé pour modifier le code.

## Désactivation et restauration des avertissements du compilateur
Vous pouvez désactiver les avertissements du compilateur en utilisant `#pragma warning disable` et les restaurer en utilisant `#pragma warning restore` :
 
    #pragma warning disable CS0168
 
    // Will not generate the "unused variable" compiler warning since it was disabled
    var x = 5;
 
    #pragma warning restore CS0168
 
    // Will generate a compiler warning since the warning was just restored
    var y = 8;
 
Les numéros d'avertissement séparés par des virgules sont autorisés :
 
    #pragma warning disable CS0168, CS0219
 
Le préfixe "CS" est facultatif et peut même être mélangé (bien que ce ne soit pas une bonne pratique) :
 
    #pragma warning disable 0168, 0219, CS0414

## Génération d'avertissements et d'erreurs du compilateur
Les avertissements du compilateur peuvent être générés à l'aide de la directive `#warning`, et les erreurs peuvent également être générées à l'aide de la directive `#error`.

<!-- langue : lang-none -->

    #if SOME_SYMBOL
    #error This is a compiler Error.
    #elif SOME_OTHER_SYMBOL
    #warning This is a compiler Warning.
    #endif

## Utilisation de l'attribut conditionnel
L'ajout d'un attribut "Conditionnel" de l'espace de noms "System.Diagnostics" à une méthode est un moyen propre de contrôler quelles méthodes sont appelées dans vos builds et lesquelles ne le sont pas.

    #define EXAMPLE_A

    using System.Diagnostics;
    class Program
    {
        static void Main()
        {
            ExampleA(); // This method will be called
            ExampleB(); // This method will not be called
        }

        [Conditional("EXAMPLE_A")]
        static void ExampleA() {...}

        [Conditional("EXAMPLE_B")]
        static void ExampleB() {...}
    }

## Préprocesseurs personnalisés au niveau du projet
Il est pratique de définir un prétraitement conditionnel personnalisé au niveau du projet lorsque certaines actions doivent être ignorées, par exemple pour les tests.

Allez dans `Solution Explorer` -> Cliquez sur <kbd>Right Mouse</kbd> sur le projet auquel vous souhaitez définir la variable -> `Properties` -> `Build` -> En général, recherchez le champ `Conditional compilation symbols` et entrez votre variable conditionnelle ici

[![entrez la description de l'image ici][1]][1]


Exemple de code qui sautera du code :

    public void Init()
    {
        #if !IGNOREREFRESHDB
        // will skip code here
         db.Initialize();
        #endif
    }

[1] : http://i.stack.imgur.com/B2pi1.png


