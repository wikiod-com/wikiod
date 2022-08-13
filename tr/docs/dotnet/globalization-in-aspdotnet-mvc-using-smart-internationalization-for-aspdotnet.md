---
title: "ASP.NET için Akıllı uluslararasılaştırmayı kullanarak ASP.NET MVC'de küreselleştirme"
slug: "aspnet-icin-akll-uluslararaslastrmay-kullanarak-aspnet-mvcde-kuresellestirme"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

[ASP.NET sayfası için akıllı uluslararasılaştırma](https://github.com/turquoiseowl/i18n)

Bu yaklaşımın yararı, .resx dosyalarından değerleri aramak için denetleyicileri ve diğer sınıfları kodla karıştırmanız gerekmemesidir. Metni [[[üçlü parantez]]] içine almanız yeterlidir (Sınırlayıcı yapılandırılabilir.) Bir "HttpModule", sınırlandırılmış metni değiştirmek için .po dosyanızda bir çeviri arar. Bir çeviri bulunursa, "HttpModule" çevirinin yerine geçer. Çeviri bulunamazsa, üç köşeli parantezleri kaldırır ve sayfayı orijinal çevrilmemiş metinle işler.

.po dosyaları, uygulamalar için çeviri sağlamak için standart bir biçimdir, bu nedenle bunları düzenlemek için kullanılabilecek çok sayıda uygulama vardır. Çevirileri ekleyebilmeleri için teknik bilgisi olmayan bir kullanıcıya bir .po dosyası göndermek kolaydır.

## Temel yapılandırma ve kurulum
1. [I18N nuget paketini][1] MVC projenize ekleyin.
2. web.config'de, "i18n.LocalizingModule" öğesini "<httpModules>" veya "<modules>" bölümünüze ekleyin.


    <!-- IIS 6 -->
    <httpModules>
      <add name="i18n.LocalizingModule" type="i18n.LocalizingModule, i18n" />
    </httpModules>

    <!-- IIS 7 -->
    <system.webServer> 
      <modules>
        <add name="i18n.LocalizingModule" type="i18n.LocalizingModule, i18n" />
      </modules>
    </system.webServer>

3. Sitenizin kök dizinine "locale" adlı bir klasör ekleyin. Desteklemek istediğiniz her kültür için bir alt klasör oluşturun. Örneğin, `/locale/fr/`.
4. Her kültüre özgü klasörde, `messages.po` adında bir metin dosyası oluşturun.
5. Test amacıyla, "messages.po" dosyanıza aşağıdaki metin satırlarını girin:


    #: Translation test
    msgid "Hello, world!"
    msgstr "Bonjour le monde!"

6. Projenize çevrilecek metin döndüren bir denetleyici ekleyin.


    using System.Web.Mvc;

    namespace I18nDemo.Controllers
    {
        public class DefaultController : Controller
        {
            public ActionResult Index()
            {
                // Text inside [[[triple brackets]]] must precisely match
                // the msgid in your .po file.
                return Content("[[[Hello, world!]]]");
            }
        }
    }

7. MVC uygulamanızı çalıştırın ve http://localhost:[portnumaranız]/varsayılan gibi denetleyici işleminize karşılık gelen rotaya göz atın.
URL'nin varsayılan kültürünüzü yansıtacak şekilde değiştirildiğini gözlemleyin, örneğin:
http://localhost:[portnumaranız]/en/default.
8. URL'deki `/en/` ibaresini `/fr/` ile değiştirin (veya hangi kültürü seçtiyseniz). Sayfa şimdi metninizin çevrilmiş sürümünü göstermelidir.
9. Alternatif kültürünüzü tercih etmek için tarayıcınızın dil ayarını değiştirin ve tekrar `/default` seçeneğine gidin. URL'nin alternatif kültürünüzü yansıtacak şekilde değiştirildiğini ve çevrilen metnin göründüğünü gözlemleyin.
10. Web.config'de, kullanıcıların 'locale' klasörünüze göz atamaması için işleyiciler ekleyin.


    <!-- IIS 6 -->
    <system.web>
      <httpHandlers>
        <add path="*" verb="*" type="System.Web.HttpNotFoundHandler"/>
      </httpHandlers>
    </system.web>
   
    <!-- IIS 7 -->
    <system.webServer>
      <handlers>
        <remove name="BlockViewHandler"/>
       <add name="BlockViewHandler" path="*" verb="*" preCondition="integratedMode" type="System.Web.HttpNotFoundHandler"/>
      </handlers>
    </system.webServer>

[1]: https://www.nuget.org/packages/I18N/

