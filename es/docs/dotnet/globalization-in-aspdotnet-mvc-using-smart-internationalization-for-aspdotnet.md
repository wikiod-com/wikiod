---
title: "Globalización en ASP.NET MVC usando Internacionalización inteligente para ASP.NET"
slug: "globalizacion-en-aspnet-mvc-usando-internacionalizacion-inteligente-para-aspnet"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

[Internacionalización inteligente para la página ASP.NET](https://github.com/turquoiseowl/i18n)

El beneficio de este enfoque es que no tiene que saturar los controladores y otras clases con código para buscar valores de archivos .resx. Simplemente rodee el texto entre [[[corchetes triples.]]] (El delimitador es configurable.) Un `HttpModule` busca una traducción en su archivo .po para reemplazar el texto delimitado. Si se encuentra una traducción, `HttpModule` sustituye la traducción. Si no se encuentra ninguna traducción, elimina los corchetes triples y presenta la página con el texto original sin traducir.

Los archivos .po son un formato estándar para proporcionar traducciones para aplicaciones, por lo que hay varias aplicaciones disponibles para editarlos. Es fácil enviar un archivo .po a un usuario no técnico para que pueda agregar traducciones.

## Configuración y configuración básica
1. Agregue el [paquete nuget I18N][1] a su proyecto MVC.
2. En web.config, agregue `i18n.LocalizingModule` a su sección `<httpModules>` o `<modules>`.


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

3. Agregue una carpeta llamada "locale" a la raíz de su sitio. Cree una subcarpeta para cada cultura que desee apoyar. Por ejemplo, `/locale/fr/`.
4. En cada carpeta específica de cultura, cree un archivo de texto llamado `messages.po`.
5. Para fines de prueba, ingrese las siguientes líneas de texto en su archivo `messages.po`:


    #: Translation test
    msgid "Hello, world!"
    msgstr "Bonjour le monde!"

6. Agregue un controlador a su proyecto que devuelva un texto para traducir.


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

7. Ejecute su aplicación MVC y busque la ruta correspondiente a la acción de su controlador, como http://localhost:[yourportnumber]/default.
Observe que la URL se cambia para reflejar su cultura predeterminada, como
http://localhost:[su número de puerto]/en/default.
8. Reemplace `/en/` en la URL con `/fr/` (o la cultura que haya seleccionado). La página ahora debería mostrar la versión traducida de su texto.
9. Cambie la configuración de idioma de su navegador para preferir su cultura alternativa y busque `/default` nuevamente. Observe que la URL se cambia para reflejar su cultura alternativa y aparece el texto traducido.
10. En web.config, agregue controladores para que los usuarios no puedan navegar a su carpeta `locale`.


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

