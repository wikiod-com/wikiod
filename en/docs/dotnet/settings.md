---
title: "Settings"
slug: "settings"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## AppSettings from ConfigurationSettings in .NET 1.x
### Deprecated usage ###
The [ConfigurationSettings][1] class was the original way to retrieve settings for an assembly in .NET 1.0 and 1.1. It has been superseded by the [ConfigurationManager][2] class and the [WebConfigurationManager][3] class.

If you have two keys with the same name in the `appSettings` section of the configuration file, the last one is used.

**app.config**

    <?xml version="1.0" encoding="utf-8"?>
    <configuration>
      <appSettings>
        <add key="keyName" value="anything, as a string"/>
        <add key="keyNames" value="123"/>
        <add key="keyNames" value="234"/>
      </appSettings>
    </configuration>

**Program.cs**

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


  [1]: https://msdn.microsoft.com/en-us/library/system.configuration.configurationsettings.aspx
  [2]: https://msdn.microsoft.com/en-us/library/system.configuration.configurationmanager.aspx
  [3]: https://msdn.microsoft.com/en-us/library/system.web.configuration.webconfigurationmanager.aspx



## Reading AppSettings from ConfigurationManager in .NET 2.0 and later
The [ConfigurationManager][1] class supports the `AppSettings` property, which allows you to continue reading settings from the `appSettings` section of a configuration file the same way as .NET 1.x supported.

**app.config**

    <?xml version="1.0" encoding="utf-8"?>
    <configuration>
      <appSettings>
        <add key="keyName" value="anything, as a string"/>
        <add key="keyNames" value="123"/>
        <add key="keyNames" value="234"/>
      </appSettings>
    </configuration>

**Program.cs**

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


  [1]: https://msdn.microsoft.com/en-us/library/system.configuration.configurationmanager.aspx

## Introduction to strongly-typed application and user settings support from Visual Studio
Visual Studio helps manage user and application settings. Using this approach has these benefits over using the `appSettings` section of the configuration file.

 1. Settings can be made strongly typed. Any type which can be serialized can be used for a settings value.

 2. Application settings can be easily separated from user settings. Application settings are stored in a single configuration file: `web.config` for Web sites and Web applications, and app.config, renamed as *assembly*.exe.config, where *assembly* is the name of the executable. User settings (not used by Web projects) are stored in a `user.config` file in the user's Application Data folder (which varies with the operating system version).

 3. Application settings from class libraries can be combined into a single configuration file without risk of name collisions, since each class library can have its own custom settings section.

In most project types, the [Project Properties Designer][1] has a [Settings][2] tab which is the starting point for creating custom application and user settings. Initially, the Settings tab will be blank, with a single link to create a default settings file. Clicking the link results in these changes:

 1. If a configuration file (`app.config` or `web.config`) does not exist for the project, one will be created.

 2. The Settings tab will be replaced with a grid control which enables you to create, edit, and delete individual settings entries.

 3. In Solution Explorer, a `Settings.settings` item is added under the Properties special folder. Opening this item will open the Settings tab.

 4. A new file with a new partial class is added under the `Properties` folder in the project folder. This new file is named `Settings.Designer.__` (.cs, .vb, etc.), and the class is named `Settings`. The class is code-generated, so it should not be edited, but the class is a partial class, so you can extend the class by putting additional members in a separate file. Furthermore, the class is implemented using the Singleton Pattern, exposing the singleton instance with the property named `Default`.

As you add each new entry to the Settings tab, Visual Studio does these two things:

 1. Saves the setting in the configuration file, in a custom configuration section designed to be managed by the Settings class.

 2. Creates a new member in the Settings class to read, write, and present the setting in the specific type selected from the Settings tab.



  [1]: https://msdn.microsoft.com/en-us/library/z2f953x9.aspx
  [2]: https://msdn.microsoft.com/en-us/library/a65txexh.aspx

## Reading strongly-typed settings from custom section of configuration file
Starting from a new Settings class and custom configuration section:

[![Settings tab of the Project Properties Designer][1]][1]

Add an application setting named ExampleTimeout, using the time System.Timespan, and set the value to 1 minute:

[![Settings tab while adding ExampleTimeout application setting][2]][2]

Save the Project Properties, which saves the Settings tab entries, as well as re-generates the custom Settings class and updates the project configuration file.

Use the setting from code (C#):

**Program.cs**

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

## Under the covers

Look in the project configuration file to see how the application setting entry has been created:

**app.config** (Visual Studio updates this automatically)

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

Notice that the `appSettings` section is not used. The `applicationSettings` section contains a custom namespace-qualified section that has a `setting` element for each entry. The type of the value is not stored in the configuration file; it is only known by the `Settings` class.

Look in the `Settings` class to see how it uses the `ConfigurationManager` class to read this custom section.

**Settings.designer.cs** (for C# projects)

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

Notice that a `DefaultSettingValueAttribute` was created to stored the value entered in the Settings tab of the Project Properties Designer. If the entry is missing from the configuration file, this default value is used instead.

  [1]: http://i.stack.imgur.com/ccuKH.png
  [2]: http://i.stack.imgur.com/bVMK4.png



