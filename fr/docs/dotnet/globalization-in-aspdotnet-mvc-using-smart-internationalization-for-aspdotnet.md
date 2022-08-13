---
title: "Globalisation dans ASP.NET MVC à l'aide de l'internationalisation intelligente pour ASP.NET"
slug: "globalisation-dans-aspnet-mvc-a-laide-de-linternationalisation-intelligente-pour-aspnet"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

[Internationalisation intelligente pour la page ASP.NET](https://github.com/turquoiseowl/i18n)

L'avantage de cette approche est que vous n'avez pas à encombrer les contrôleurs et autres classes avec du code pour rechercher des valeurs à partir de fichiers .resx. Vous entourez simplement le texte de [[[crochets triples.]]] (Le délimiteur est configurable.) Un `HttpModule` recherche une traduction dans votre fichier .po pour remplacer le texte délimité. Si une traduction est trouvée, le `HttpModule` remplace la traduction. Si aucune traduction n'est trouvée, il supprime les triples crochets et affiche la page avec le texte original non traduit.

Les fichiers .po sont un format standard pour fournir des traductions pour les applications, il existe donc un certain nombre d'applications disponibles pour les modifier. Il est facile d'envoyer un fichier .po à un utilisateur non technique afin qu'il puisse ajouter des traductions.

## Configuration et configuration de base
1. Ajoutez le [paquet nuget I18N][1] à votre projet MVC.
2. Dans web.config, ajoutez le `i18n.LocalizingModule` à votre section `<httpModules>` ou `<modules>`.


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

3. Ajoutez un dossier nommé "locale" à la racine de votre site. Créez un sous-dossier pour chaque culture que vous souhaitez prendre en charge. Par exemple, `/locale/fr/`.
4. Dans chaque dossier spécifique à la culture, créez un fichier texte nommé « messages.po ».
5. À des fins de test, saisissez les lignes de texte suivantes dans votre fichier `messages.po` :


    #: Translation test
    msgid "Hello, world!"
    msgstr "Bonjour le monde!"

6. Ajoutez un contrôleur à votre projet qui renvoie du texte à traduire.


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

7. Exécutez votre application MVC et accédez à la route correspondant à l'action de votre contrôleur, telle que http://localhost:[yourportnumber]/default.
Observez que l'URL est modifiée pour refléter votre culture par défaut, telle que
http://localhost :[votrenumérodeport]/en/default.
8. Remplacez `/en/` dans l'URL par `/fr/` (ou la culture que vous avez sélectionnée). La page devrait maintenant afficher la version traduite de votre texte.
9. Modifiez le paramètre de langue de votre navigateur pour préférer votre autre culture et accédez à nouveau à `/default`. Observez que l'URL est modifiée pour refléter votre autre culture et que le texte traduit apparaît.
10. Dans web.config, ajoutez des gestionnaires afin que les utilisateurs ne puissent pas accéder à votre dossier `locale`.


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

[1] : https://www.nuget.org/packages/I18N/

