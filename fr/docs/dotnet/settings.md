---
title: "Réglages"
slug: "reglages"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## AppSettings de ConfigurationSettings dans .NET 1.x
### Utilisation obsolète ###
La classe [ConfigurationSettings][1] était le moyen original de récupérer les paramètres d'un assembly dans .NET 1.0 et 1.1. Elle a été remplacée par la classe [ConfigurationManager][2] et la classe [WebConfigurationManager][3].

Si vous avez deux clés portant le même nom dans la section "appSettings" du fichier de configuration, la dernière est utilisée.

**app.config**

    <?xml version="1.0" encoding="utf-8"?>
    <configuration>
      <appSettings>
        <add key="keyName" value="anything, as a string"/>
        <add key="keyNames" value="123"/>
        <add key="keyNames" value="234"/>
      </appSettings>
    </configuration>

**Programme.cs**

    using System;
    using System.Configuration;
    using System.Diagnostics;
    
    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main()
            {
                string keyValue = ConfigurationSettings.AppSettings["keyName"];
                Debug.Assert("anything, as a string".Equals(keyValue));
    
                string twoKeys = ConfigurationSettings.AppSettings["keyNames"];
                Debug.Assert("234".Equals(twoKeys));
    
                Console.ReadKey();
            }
        }
    }


[1] : https://msdn.microsoft.com/en-us/library/system.configuration.configurationsettings.aspx
[2] : https://msdn.microsoft.com/en-us/library/system.configuration.configurationmanager.aspx
[3] : https://msdn.microsoft.com/en-us/library/system.web.configuration.webconfigurationmanager.aspx



## Lecture des AppSettings à partir de ConfigurationManager dans .NET 2.0 et versions ultérieures
La classe [ConfigurationManager][1] prend en charge la propriété `AppSettings`, qui vous permet de continuer à lire les paramètres de la section `appSettings` d'un fichier de configuration de la même manière que .NET 1.x pris en charge.

**app.config**

    <?xml version="1.0" encoding="utf-8"?>
    <configuration>
      <appSettings>
        <add key="keyName" value="anything, as a string"/>
        <add key="keyNames" value="123"/>
        <add key="keyNames" value="234"/>
      </appSettings>
    </configuration>

**Programme.cs**

    using System;
    using System.Configuration;
    using System.Diagnostics;
    
    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main()
            {
                string keyValue = ConfigurationManager.AppSettings["keyName"];
                Debug.Assert("anything, as a string".Equals(keyValue));
    
                var twoKeys = ConfigurationManager.AppSettings["keyNames"];
                Debug.Assert("234".Equals(twoKeys));
    
                Console.ReadKey();
            }
        }
    }


[1] : https://msdn.microsoft.com/en-us/library/system.configuration.configurationmanager.aspx

## Introduction à la prise en charge des applications fortement typées et des paramètres utilisateur à partir de Visual Studio
Visual Studio aide à gérer les paramètres des utilisateurs et des applications. L'utilisation de cette approche présente ces avantages par rapport à l'utilisation de la section "appSettings" du fichier de configuration.

1. Les paramètres peuvent être fortement typés. Tout type pouvant être sérialisé peut être utilisé pour une valeur de paramètres.

2. Les paramètres de l'application peuvent être facilement séparés des paramètres de l'utilisateur. Les paramètres de l'application sont stockés dans un seul fichier de configuration : `web.config` pour les sites Web et les applications Web, et app.config, renommé *assembly*.exe.config, où *assembly* est le nom de l'exécutable. Les paramètres utilisateur (non utilisés par les projets Web) sont stockés dans un fichier `user.config` dans le dossier Application Data de l'utilisateur (qui varie selon la version du système d'exploitation).

3. Les paramètres d'application des bibliothèques de classes peuvent être combinés dans un seul fichier de configuration sans risque de collisions de noms, puisque chaque bibliothèque de classes peut avoir sa propre section de paramètres personnalisés.

Dans la plupart des types de projets, [Project Properties Designer][1] possède un onglet [Settings][2] qui est le point de départ pour créer des paramètres d'application et utilisateur personnalisés. Initialement, l'onglet Paramètres sera vide, avec un seul lien pour créer un fichier de paramètres par défaut. Cliquer sur le lien entraîne ces modifications :

1. Si un fichier de configuration (`app.config` ou `web.config`) n'existe pas pour le projet, un sera créé.

2. L'onglet Paramètres sera remplacé par un contrôle de grille qui vous permet de créer, de modifier et de supprimer des entrées de paramètres individuelles.

3. Dans l'Explorateur de solutions, un élément "Settings.settings" est ajouté sous le dossier spécial Propriétés. L'ouverture de cet élément ouvrira l'onglet Paramètres.

4. Un nouveau fichier avec une nouvelle classe partielle est ajouté sous le dossier "Propriétés" dans le dossier du projet. Ce nouveau fichier est nommé `Settings.Designer.__` (.cs, .vb, etc.) et la classe est nommée `Settings`. La classe est générée par le code, elle ne doit donc pas être modifiée, mais la classe est une classe partielle, vous pouvez donc étendre la classe en plaçant des membres supplémentaires dans un fichier séparé. De plus, la classe est implémentée à l'aide du modèle Singleton, exposant l'instance singleton avec la propriété nommée `Default`.

Lorsque vous ajoutez chaque nouvelle entrée à l'onglet Paramètres, Visual Studio effectue ces deux opérations :

1. Enregistre le paramètre dans le fichier de configuration, dans une section de configuration personnalisée conçue pour être gérée par la classe Settings.

2. Crée un nouveau membre dans la classe Paramètres pour lire, écrire et présenter le paramètre dans le type spécifique sélectionné dans l'onglet Paramètres.



[1] : https://msdn.microsoft.com/en-us/library/z2f953x9.aspx
[2] : https://msdn.microsoft.com/en-us/library/a65txexh.aspx

## Lecture des paramètres fortement typés de la section personnalisée du fichier de configuration
À partir d'une nouvelle classe de paramètres et d'une section de configuration personnalisée :

[![Onglet Paramètres du Concepteur de propriétés de projet][1]][1]

Ajoutez un paramètre d'application nommé ExampleTimeout, en utilisant l'heure System.Timespan, et définissez la valeur sur 1 minute :

[![Onglet Paramètres lors de l'ajout du paramètre d'application ExampleTimeout][2]][2]

Enregistrez les propriétés du projet, ce qui enregistre les entrées de l'onglet Paramètres, ainsi que régénère la classe de paramètres personnalisée et met à jour le fichier de configuration du projet.

Utilisez le paramètre du code (C #):

**Programme.cs**

    using System;
    using System.Diagnostics;
    using ConsoleApplication1.Properties;
    
    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main()
            {
                TimeSpan exampleTimeout = Settings.Default.ExampleTimeout;
                Debug.Assert(TimeSpan.FromMinutes(1).Equals(exampleTimeout));
    
                Console.ReadKey();
            }
        }
    }

## Sous les couvertures

Regardez dans le fichier de configuration du projet pour voir comment l'entrée de paramètre d'application a été créée :

**app.config** (Visual Studio le met à jour automatiquement)

    <?xml version="1.0" encoding="utf-8"?>
    <configuration>
      <configSections>
        <sectionGroup name="applicationSettings" type="System.Configuration.ApplicationSettingsGroup, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" >
          <section name="ConsoleApplication1.Properties.Settings" type="System.Configuration.ClientSettingsSection, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" requirePermission="false" />
        </sectionGroup>
      </configSections>
      <appSettings />
      <applicationSettings>
        <ConsoleApplication1.Properties.Settings>
          <setting name="ExampleTimeout" serializeAs="String">
            <value>00:01:00</value>
          </setting>
        </ConsoleApplication1.Properties.Settings>
      </applicationSettings>
    </configuration>

Notez que la section "appSettings" n'est pas utilisée. La section `applicationSettings` contient une section qualifiée d'espace de noms personnalisé qui a un élément `setting` pour chaque entrée. Le type de la valeur n'est pas stocké dans le fichier de configuration ; il n'est connu que par la classe `Settings`.

Regardez dans la classe `Settings` pour voir comment elle utilise la classe `ConfigurationManager` pour lire cette section personnalisée.

**Settings.designer.cs** (pour les projets C#)

    ...
        [global::System.Configuration.ApplicationScopedSettingAttribute()]
        [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
        [global::System.Configuration.DefaultSettingValueAttribute("00:01:00")]
        public global::System.TimeSpan ExampleTimeout {
            get {
                return ((global::System.TimeSpan)(this["ExampleTimeout"]));
            }
        }
    ...

Notez qu'un `DefaultSettingValueAttribute` a été créé pour stocker la valeur entrée dans l'onglet Paramètres du concepteur de propriétés du projet. Si l'entrée est absente du fichier de configuration, cette valeur par défaut est utilisée à la place.

[1] : http://i.stack.imgur.com/ccuKH.png
[2] : http://i.stack.imgur.com/bVMK4.png



