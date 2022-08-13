---
title: "Conventions de nommage"
slug: "conventions-de-nommage"
draft: false
images: []
weight: 9868
type: docs
toc: true
---

Cette rubrique décrit certaines conventions de dénomination de base utilisées lors de l'écriture dans le langage C#. Comme toutes les conventions, elles ne sont pas imposées par le compilateur, mais assureront la lisibilité entre les développeurs.

Pour des instructions complètes de conception de .NET Framework, voir [docs.microsoft.com/dotnet/standard/design-guidelines](https://docs.microsoft.com/dotnet/standard/design-guidelines/).

## Choisissez des noms d'identifiant facilement lisibles
Par exemple, une propriété nommée HorizontalAlignment est plus lisible en anglais qu'AlignementHorizontal.

## Privilégier la lisibilité à la brièveté
Le nom de la propriété `CanScrollHorizontally` est meilleur que `ScrollableX` (une référence obscure à l'axe X).

Évitez d'utiliser des traits de soulignement, des traits d'union ou tout autre caractère non alphanumérique.

## N'utilisez **pas** la notation hongroise
La notation hongroise est la pratique consistant à inclure un préfixe dans les identifiants pour coder certaines métadonnées sur le paramètre, telles que le type de données de l'identifiant, par ex. `chaîne strName`.

Évitez également d'utiliser des identificateurs qui entrent en conflit avec des mots-clés déjà utilisés dans C#.

## Abréviations et acronymes
En général, vous ne devez pas utiliser d'abréviations ou d'acronymes ; ceux-ci rendent vos noms moins lisibles. De même, il est difficile de savoir quand il est sûr de supposer qu'un acronyme est largement reconnu.

## Conventions d'utilisation des majuscules
Les termes suivants décrivent différentes manières d'identifier les cas.
## Boîtier Pascal
La première lettre de l'identifiant et la première lettre de chaque mot concaténé suivant sont en majuscules. Vous pouvez utiliser la casse Pascal pour les identifiants de trois caractères ou plus. Par exemple : `BackColor`

## Boîtier de chameau
La première lettre d'un identifiant est en minuscule et la première lettre de chaque mot concaténé suivant est en majuscule. Par exemple : `backColor`

## Majuscule
Toutes les lettres de l'identifiant sont en majuscules. Par exemple : "IO"

---

## Règles
Lorsqu'un identifiant se compose de plusieurs mots, n'utilisez pas de séparateurs, tels que des traits de soulignement ("_") ou des traits d'union ("-"), entre les mots. Utilisez plutôt la casse pour indiquer le début de chaque mot.

Le tableau suivant résume les règles de capitalisation des identifiants et fournit des exemples pour les différents types d'identifiants :

Identifiant | Cas | Exemple
---------------------- | ------ | -------
Variable locale | Chameau | nom de la voiture
Classe | Pascal | AppDomain
Type d'énumération | Pascal | Niveau d'erreur
Valeurs d'énumération | Pascal | Erreur fatale
Événement | Pascal | Valeur modifiée
Classe d'exception | Pascal | WebException
Champ statique en lecture seule | Pascal | Valeurrouge
Interface | Pascal | Jetable
Méthode | Pascal | ToString
Espace de noms | Pascal | System.Drawing
Paramètre | Chameau | typeName
Propriété | Pascal | Couleur de fond

Plus d'informations peuvent être trouvées sur [MSDN][1].


[1] : https://msdn.microsoft.com/library/ms229043(v=vs.110).aspx

## Énumérations
## Utilisez un nom singulier pour la plupart des énumérations

    public enum Volume
    {
       Low,
       Medium,
       High
    }

## Utilisez un nom au pluriel pour les types Enum qui sont des champs de bits

    [Flags]
    public enum MyColors
    {
        Yellow = 1,
        Green = 2,
        Red = 4,
        Blue = 8
    }
*Remarque : ajoutez toujours [`FlagsAttribute`][1] à un champ de bits de type Enum.*

## N'ajoutez **pas** 'enum' comme suffixe

    public enum VolumeEnum // Incorrect

## N'utilisez **pas** le nom de l'énumération dans chaque entrée

    public enum Color
    {
        ColorBlue, // Remove Color, unnecessary
        ColorGreen,
    }


[1] : https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx

##Interfaces
Les interfaces doivent être nommées avec des noms ou des phrases nominales, ou des adjectifs qui décrivent un comportement. Par exemple, `IComponent` utilise un nom descriptif, `ICustomAttributeProvider` utilise une expression nominale et `IPersistable` utilise un adjectif.

Les noms d'interface doivent être précédés de la lettre "I", pour indiquer que le type est une interface, et la casse Pascal doit être utilisée.

Vous trouverez ci-dessous des interfaces correctement nommées :

    public interface IServiceProvider
    public interface IFormatable

## Exceptions
## Ajouter 'exception' comme suffixe
Les noms d'exception personnalisés doivent être suffixés par "-Exception".

Vous trouverez ci-dessous des exceptions correctement nommées :

    public class MyCustomException : Exception
    public class FooException : Exception

## Champs privés
Il existe deux conventions communes pour les champs privés : `camelCase` et `_camelCaseWithLeadingUnderscore`.

## Affaire de chameau

    public class Rational
    {
        private readonly int numerator;
        private readonly int denominator;

        public Rational(int numerator, int denominator)
        {
            // "this" keyword is required to refer to the class-scope field
            this.numerator = numerator;
            this.denominator = denominator;
        }
    }

## Étui camel avec trait de soulignement

    public class Rational
    {
        private readonly int _numerator;
        private readonly int _denominator;

        public Rational(int numerator, int denominator)
        {
            // Names are unique, so "this" keyword is not required
            _numerator = numerator;
            _denominator = denominator;
        }
    }

## Espaces de noms
Le format général des espaces de noms est :

    <Company>.(<Product>|<Technology>)[.<Feature>][.<Subnamespace>].

Les exemples comprennent:

    Fabrikam.Math
    Litware.Security

Préfixer les noms d'espace de noms avec un nom de société empêche les espaces de noms de différentes sociétés d'avoir le même nom.



