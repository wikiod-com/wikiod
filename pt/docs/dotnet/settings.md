---
title: "Definições"
slug: "definicoes"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## AppSettings de ConfigurationSettings em .NET 1.x
### Uso obsoleto ###
A classe [ConfigurationSettings][1] era a maneira original de recuperar as configurações de um assembly no .NET 1.0 e 1.1. Ele foi substituído pela classe [ConfigurationManager][2] e pela classe [WebConfigurationManager][3].

Se você tiver duas chaves com o mesmo nome na seção `appSettings` do arquivo de configuração, a última será usada.

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



## Lendo AppSettings do ConfigurationManager no .NET 2.0 e posterior
A classe [ConfigurationManager][1] suporta a propriedade `AppSettings`, que permite que você continue lendo as configurações da seção `appSettings` de um arquivo de configuração da mesma forma que suporta .NET 1.x.

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

## Introdução ao suporte de configurações de usuário e aplicativos fortemente tipados do Visual Studio
O Visual Studio ajuda a gerenciar as configurações do usuário e do aplicativo. Usar essa abordagem tem esses benefícios em relação ao uso da seção `appSettings` do arquivo de configuração.

1. As configurações podem ser feitas fortemente digitadas. Qualquer tipo que possa ser serializado pode ser usado para um valor de configuração.

2. As configurações do aplicativo podem ser facilmente separadas das configurações do usuário. As configurações do aplicativo são armazenadas em um único arquivo de configuração: `web.config` para sites e aplicativos da Web e app.config, renomeado como *assembly*.exe.config, onde *assembly* é o nome do executável. As configurações do usuário (não usadas por projetos da Web) são armazenadas em um arquivo `user.config` na pasta Application Data do usuário (que varia com a versão do sistema operacional).

3. As configurações de aplicativos das bibliotecas de classes podem ser combinadas em um único arquivo de configuração sem risco de colisões de nomes, pois cada biblioteca de classes pode ter sua própria seção de configurações personalizadas.

Na maioria dos tipos de projeto, o [Project Properties Designer][1] tem uma guia [Settings][2] que é o ponto de partida para criar aplicativos personalizados e configurações de usuário. Inicialmente, a guia Configurações estará em branco, com um único link para criar um arquivo de configurações padrão. Clicar no link resulta nestas alterações:

1. Se um arquivo de configuração (`app.config` ou `web.config`) não existir para o projeto, um será criado.

2. A guia Configurações será substituída por um controle de grade que permite criar, editar e excluir entradas de configurações individuais.

3. No Solution Explorer, um item `Settings.settings` é adicionado na pasta especial Propriedades. Abrir este item abrirá a guia Configurações.

4. Um novo arquivo com uma nova classe parcial é adicionado na pasta `Properties` na pasta do projeto. Este novo arquivo é denominado `Settings.Designer.__` (.cs, .vb, etc.), e a classe é denominada `Settings`. A classe é gerada por código, portanto, não deve ser editada, mas a classe é uma classe parcial, portanto, você pode estender a classe colocando membros adicionais em um arquivo separado. Além disso, a classe é implementada usando o Padrão Singleton, expondo a instância singleton com a propriedade chamada `Default`.

À medida que você adiciona cada nova entrada à guia Configurações, o Visual Studio faz estas duas coisas:

1. Salva a configuração no arquivo de configuração, em uma seção de configuração personalizada projetada para ser gerenciada pela classe Settings.

2. Cria um novo membro na classe Configurações para ler, gravar e apresentar a configuração no tipo específico selecionado na guia Configurações.



[1]: https://msdn.microsoft.com/en-us/library/z2f953x9.aspx
[2]: https://msdn.microsoft.com/en-us/library/a65txexh.aspx

## Lendo configurações fortemente tipadas da seção personalizada do arquivo de configuração
A partir de uma nova classe de configurações e seção de configuração personalizada:

[![Guia Configurações do Designer de propriedades do projeto][1]][1]

Adicione uma configuração de aplicativo chamada ExampleTimeout, usando a hora System.Timespan e defina o valor como 1 minuto:

[![Guia Configurações ao adicionar a configuração do aplicativo ExampleTimeout][2]][2]

Salve as Propriedades do Projeto, que salva as entradas da guia Configurações, além de gerar novamente a classe Configurações personalizada e atualizar o arquivo de configuração do projeto.

Use a configuração do código (C#):

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

## Debaixo das cobertas

Procure no arquivo de configuração do projeto para ver como a entrada de configuração do aplicativo foi criada:

**app.config** (o Visual Studio atualiza isso automaticamente)

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

Observe que a seção `appSettings` não é usada. A seção `applicationSettings` contém uma seção personalizada qualificada por namespace que possui um elemento `setting` para cada entrada. O tipo do valor não é armazenado no arquivo de configuração; é conhecido apenas pela classe `Settings`.

Olhe na classe `Settings` para ver como ela usa a classe `ConfigurationManager` para ler esta seção personalizada.

**Settings.designer.cs** (para projetos C#)

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

Observe que um `DefaultSettingValueAttribute` foi criado para armazenar o valor inserido na guia Configurações do Designer de propriedades do projeto. Se a entrada estiver faltando no arquivo de configuração, esse valor padrão será usado.

[1]: http://i.stack.imgur.com/ccuKH.png
[2]: http://i.stack.imgur.com/bVMK4.png



