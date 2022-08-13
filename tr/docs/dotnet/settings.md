---
title: "Ayarlar"
slug: "ayarlar"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## .NET 1.x'te ConfigurationSettings'den AppSettings
### Kullanımdan kaldırılmış kullanım ###
[ConfigurationSettings][1] sınıfı, .NET 1.0 ve 1.1'deki bir derleme için ayarları almanın orijinal yoluydu. [ConfigurationManager][2] sınıfı ve [WebConfigurationManager][3] sınıfı tarafından yerini almıştır.

Yapılandırma dosyasının 'appSettings' bölümünde aynı ada sahip iki anahtarınız varsa, sonuncusu kullanılır.

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



## .NET 2.0 ve sonraki sürümlerde ConfigurationManager'dan AppSettings Okuma
[ConfigurationManager][1] sınıfı, bir yapılandırma dosyasının 'appSettings' bölümünden ayarları .NET 1.x'in desteklediği şekilde okumaya devam etmenize izin veren 'AppSettings' özelliğini destekler.

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

## Visual Studio'dan kesin olarak girilmiş uygulama ve kullanıcı ayarları desteğine giriş
Visual Studio, kullanıcı ve uygulama ayarlarının yönetilmesine yardımcı olur. Bu yaklaşımı kullanmanın, yapılandırma dosyasının "appSettings" bölümünü kullanmaya göre şu avantajları vardır.

1. Ayarlar kesinlikle yazılarak yapılabilir. Serileştirilebilen herhangi bir tür, bir ayar değeri için kullanılabilir.

2. Uygulama ayarları, kullanıcı ayarlarından kolaylıkla ayrılabilir. Uygulama ayarları tek bir yapılandırma dosyasında saklanır: Web siteleri ve Web uygulamaları için "web.config" ve *assembly*.exe.config olarak yeniden adlandırılan app.config, burada *assembly* yürütülebilir dosyanın adıdır. Kullanıcı ayarları (Web projeleri tarafından kullanılmaz), kullanıcının Application Data klasöründeki (işletim sistemi sürümüne göre değişen) bir "user.config" dosyasında saklanır.

3. Sınıf kitaplıklarından uygulama ayarları, her sınıf kitaplığının kendi özel ayarlar bölümüne sahip olabileceğinden, ad çakışması riski olmadan tek bir yapılandırma dosyasında birleştirilebilir.

Çoğu proje türünde, [Proje Özellikleri Tasarımcısı][1], özel uygulama ve kullanıcı ayarları oluşturmak için başlangıç ​​noktası olan bir [Ayarlar][2] sekmesine sahiptir. Başlangıçta, Ayarlar sekmesi, varsayılan ayarlar dosyası oluşturmak için tek bir bağlantıyla boş olacaktır. Bağlantıya tıklamak şu değişikliklerle sonuçlanır:

1. Proje için bir yapılandırma dosyası (`app.config` veya `web.config`) yoksa, bir tane oluşturulur.

2. Ayarlar sekmesi, bireysel ayar girişlerini oluşturmanıza, düzenlemenize ve silmenize olanak tanıyan bir ızgara kontrolü ile değiştirilecektir.

3. Solution Explorer'da, Özellikler özel klasörünün altına bir `Settings.settings` öğesi eklenir. Bu öğeyi açmak, Ayarlar sekmesini açacaktır.

4. Proje klasöründeki `Özellikler` klasörünün altına yeni bir kısmi sınıfa sahip yeni bir dosya eklenir. Bu yeni dosyanın adı `Settings.Designer.__` (.cs, .vb, vb.) ve sınıfın adı da `Settings`. Sınıf kod tarafından oluşturulur, bu nedenle düzenlenmemelidir, ancak sınıf kısmi bir sınıftır, bu nedenle ayrı bir dosyaya ek üyeler koyarak sınıfı genişletebilirsiniz. Ayrıca, sınıf, Singleton Modeli kullanılarak uygulanır ve singleton örneğini 'Varsayılan' adlı özellik ile ortaya çıkarır.

Her yeni girişi Ayarlar sekmesine eklediğinizde, Visual Studio şu iki şeyi yapar:

1. Ayarları, Ayarlar sınıfı tarafından yönetilmek üzere tasarlanmış özel bir yapılandırma bölümünde, yapılandırma dosyasına kaydeder.

2. Ayarlar sekmesinden seçilen belirli türdeki ayarı okumak, yazmak ve sunmak için Ayarlar sınıfında yeni bir üye oluşturur.



[1]: https://msdn.microsoft.com/en-us/library/z2f953x9.aspx
[2]: https://msdn.microsoft.com/en-us/library/a65txexh.aspx

## Yapılandırma dosyasının özel bölümünden kesinlikle yazılan ayarları okuma
Yeni bir Ayarlar sınıfından ve özel yapılandırma bölümünden başlayarak:

[![Proje Özellikleri Tasarımcısının Ayarlar sekmesi][1]][1]

System.Timespan zamanını kullanarak ExampleTimeout adlı bir uygulama ayarı ekleyin ve değeri 1 dakikaya ayarlayın:

[![ÖrnekTimeout uygulama ayarı eklenirken Ayarlar sekmesi][2]][2]

Ayarlar sekmesi girişlerini kaydeden ve ayrıca özel Ayarlar sınıfını yeniden oluşturan ve proje yapılandırma dosyasını güncelleyen Proje Özelliklerini kaydedin.

(C#) kodundaki ayarı kullanın:

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

## Kapakların altında

Uygulama ayarı girişinin nasıl oluşturulduğunu görmek için proje yapılandırma dosyasına bakın:

**app.config** (Visual Studio bunu otomatik olarak günceller)

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

'appSettings' bölümünün kullanılmadığına dikkat edin. "applicationSettings" bölümü, her giriş için bir "ayar" öğesi içeren ad alanı nitelikli özel bir bölüm içerir. Değerin türü yapılandırma dosyasında saklanmaz; yalnızca 'Ayarlar' sınıfı tarafından bilinir.

Bu özel bölümü okumak için "ConfigurationManager" sınıfını nasıl kullandığını görmek için "Settings" sınıfına bakın.

**Settings.designer.cs** (C# projeleri için)

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

Proje Özellikleri Tasarımcısının Ayarlar sekmesinde girilen değeri depolamak için bir "DefaultSettingValueAttribute" oluşturulduğuna dikkat edin. Yapılandırma dosyasında girdi yoksa, bunun yerine bu varsayılan değer kullanılır.

[1]: http://i.stack.imgur.com/ccuKH.png
[2]: http://i.stack.imgur.com/bVMK4.png



