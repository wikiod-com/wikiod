---
title: "Ajustes"
slug: "ajustes"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## AppSettings de ConfigurationSettings en .NET 1.x
### Uso obsoleto ###
La clase [ConfigurationSettings][1] era la forma original de recuperar la configuración de un ensamblado en .NET 1.0 y 1.1. Ha sido reemplazado por la clase [ConfigurationManager][2] y la clase [WebConfigurationManager][3].

Si tiene dos claves con el mismo nombre en la sección `appSettings` del archivo de configuración, se usa la última.

**aplicación.config**

    <?xml version="1.0" encoding="utf-8"?>
    <configuration>
      <appSettings>
        <add key="keyName" value="anything, as a string"/>
        <add key="keyNames" value="123"/>
        <add key="keyNames" value="234"/>
      </appSettings>
    </configuration>

**Programa.cs**

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



## Leer AppSettings desde ConfigurationManager en .NET 2.0 y versiones posteriores
La clase [ConfigurationManager][1] es compatible con la propiedad `AppSettings`, que le permite continuar leyendo la configuración de la sección `appSettings` de un archivo de configuración de la misma manera que se admite .NET 1.x.

**aplicación.config**

    <?xml version="1.0" encoding="utf-8"?>
    <configuration>
      <appSettings>
        <add key="keyName" value="anything, as a string"/>
        <add key="keyNames" value="123"/>
        <add key="keyNames" value="234"/>
      </appSettings>
    </configuration>

**Programa.cs**

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

## Introducción a la compatibilidad con configuraciones de usuario y aplicaciones fuertemente tipadas de Visual Studio
Visual Studio ayuda a administrar la configuración de usuarios y aplicaciones. El uso de este enfoque tiene estos beneficios sobre el uso de la sección `appSettings` del archivo de configuración.

1. Los ajustes se pueden realizar fuertemente tipeados. Cualquier tipo que pueda serializarse puede usarse para un valor de configuración.

2. La configuración de la aplicación se puede separar fácilmente de la configuración del usuario. Los ajustes de la aplicación se almacenan en un solo archivo de configuración: `web.config` para sitios web y aplicaciones web, y app.config, renombrado como *assembly*.exe.config, donde *assembly* es el nombre del ejecutable. La configuración del usuario (no utilizada por los proyectos web) se almacena en un archivo `user.config` en la carpeta de datos de la aplicación del usuario (que varía según la versión del sistema operativo).

3. La configuración de la aplicación de las bibliotecas de clases se puede combinar en un solo archivo de configuración sin riesgo de colisión de nombres, ya que cada biblioteca de clases puede tener su propia sección de configuración personalizada.

En la mayoría de los tipos de proyectos, el [Diseñador de propiedades del proyecto][1] tiene una pestaña [Configuración][2] que es el punto de partida para crear configuraciones personalizadas de aplicaciones y usuarios. Inicialmente, la pestaña Configuración estará en blanco, con un solo enlace para crear un archivo de configuración predeterminado. Al hacer clic en el enlace, se producen estos cambios:

1. Si no existe un archivo de configuración (`app.config` o `web.config`) para el proyecto, se creará uno.

2. La pestaña Configuración se reemplazará con un control de cuadrícula que le permite crear, editar y eliminar entradas de configuración individuales.

3. En el Explorador de soluciones, se agrega un elemento `Settings.settings` en la carpeta especial Propiedades. Al abrir este elemento, se abrirá la pestaña Configuración.

4. Se agrega un nuevo archivo con una nueva clase parcial en la carpeta `Propiedades` en la carpeta del proyecto. Este nuevo archivo se llama `Settings.Designer.__` (.cs, .vb, etc.), y la clase se llama `Settings`. La clase se genera mediante código, por lo que no debe editarse, pero la clase es una clase parcial, por lo que puede extender la clase colocando miembros adicionales en un archivo separado. Además, la clase se implementa utilizando el patrón Singleton, exponiendo la instancia de singleton con la propiedad denominada `Default`.

A medida que agrega cada nueva entrada a la pestaña Configuración, Visual Studio hace estas dos cosas:

1. Guarda la configuración en el archivo de configuración, en una sección de configuración personalizada diseñada para ser administrada por la clase Configuración.

2. Crea un nuevo miembro en la clase Configuración para leer, escribir y presentar la configuración en el tipo específico seleccionado en la pestaña Configuración.



[1]: https://msdn.microsoft.com/en-us/library/z2f953x9.aspx
[2]: https://msdn.microsoft.com/en-us/library/a65txexh.aspx

## Lectura de configuraciones fuertemente tipadas de la sección personalizada del archivo de configuración
A partir de una nueva clase de configuración y una sección de configuración personalizada:

[![Pestaña Configuración del Diseñador de propiedades del proyecto][1]][1]

Agregue una configuración de aplicación llamada ExampleTimeout, usando el tiempo System.Timespan, y establezca el valor en 1 minuto:

[![Pestaña Configuración al agregar la configuración de la aplicación ExampleTimeout][2]][2]

Guarde las Propiedades del proyecto, que guarda las entradas de la pestaña Configuración, además de volver a generar la clase Configuración personalizada y actualizar el archivo de configuración del proyecto.

Utilice la configuración del código (C#):

**Programa.cs**

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

## Debajo de las sábanas

Busque en el archivo de configuración del proyecto para ver cómo se ha creado la entrada de configuración de la aplicación:

**app.config** (Visual Studio actualiza esto automáticamente)

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

Tenga en cuenta que la sección `appSettings` no se utiliza. La sección `applicationSettings` contiene una sección calificada de espacio de nombres personalizado que tiene un elemento `setting` para cada entrada. El tipo del valor no se almacena en el archivo de configuración; solo lo conoce la clase `Settings`.

Mire en la clase `Configuración` para ver cómo usa la clase `ConfigurationManager` para leer esta sección personalizada.

**Configuración.designer.cs** (para proyectos de C#)

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

Observe que se creó un `DefaultSettingValueAttribute` para almacenar el valor ingresado en la pestaña Configuración del Diseñador de propiedades del proyecto. Si falta la entrada en el archivo de configuración, se utiliza este valor predeterminado en su lugar.

[1]: http://i.stack.imgur.com/ccuKH.png
[2]: http://i.stack.imgur.com/bVMK4.png



