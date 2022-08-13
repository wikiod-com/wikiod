---
title: "Globalização em ASP.NET MVC usando internacionalização inteligente para ASP.NET"
slug: "globalizacao-em-aspnet-mvc-usando-internacionalizacao-inteligente-para-aspnet"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

[Internacionalização inteligente para página ASP.NET](https://github.com/turquoiseowl/i18n)

O benefício dessa abordagem é que você não precisa sobrecarregar os controladores e outras classes com código para pesquisar valores de arquivos .resx. Você simplesmente coloca o texto entre [[[colchetes triplos.]]] (O delimitador é configurável.) Um `HttpModule` procura uma tradução em seu arquivo .po para substituir o texto delimitado. Se uma tradução for encontrada, o `HttpModule` substitui a tradução. Se nenhuma tradução for encontrada, ele remove os colchetes triplos e renderiza a página com o texto original não traduzido.

Os arquivos .po são um formato padrão para fornecer traduções para aplicativos, portanto, há vários aplicativos disponíveis para editá-los. É fácil enviar um arquivo .po para um usuário não técnico para que ele possa adicionar traduções.

## Configuração básica e configuração
1. Adicione o [pacote nuget I18N][1] ao seu projeto MVC.
2. Em web.config, adicione o `i18n.LocalizingModule` à sua seção `<httpModules>` ou `<modules>`.


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

3. Adicione uma pasta chamada "locale" à raiz do seu site. Crie uma subpasta para cada cultura que você deseja oferecer suporte. Por exemplo, `/locale/fr/`.
4. Em cada pasta específica da cultura, crie um arquivo de texto chamado `messages.po`.
5. Para fins de teste, insira as seguintes linhas de texto em seu arquivo `messages.po`:


    #: Translation test
    msgid "Hello, world!"
    msgstr "Bonjour le monde!"

6. Adicione um controlador ao seu projeto que retorne algum texto para tradução.


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

7. Execute seu aplicativo MVC e navegue até a rota correspondente à sua ação do controlador, como http://localhost:[yourportnumber]/default.
Observe que a URL é alterada para refletir sua cultura padrão, como
http://localhost:[yourportnumber]/en/default.
8. Substitua `/en/` na URL por `/fr/` (ou qualquer cultura que você selecionou.) A página agora deve exibir a versão traduzida do seu texto.
9. Altere a configuração de idioma do seu navegador para preferir sua cultura alternativa e navegue até `/default` novamente. Observe que a URL é alterada para refletir sua cultura alternativa e o texto traduzido é exibido.
10. Em web.config, adicione manipuladores para que os usuários não possam navegar para sua pasta `locale`.


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

