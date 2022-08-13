---
title: "Sistema de empacotamento NuGet"
slug: "sistema-de-empacotamento-nuget"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

[NuGet.org](https://www.nuget.org/):

> NuGet é o gerenciador de pacotes para a plataforma de desenvolvimento da Microsoft, incluindo .NET. As ferramentas do cliente NuGet fornecem a capacidade de produzir e consumir pacotes. A Galeria NuGet é o repositório central de pacotes usado por todos os autores e consumidores de pacotes.

Imagens em exemplos cortesia de [NuGet.org](https://www.nuget.org/).

## Desinstalando um pacote de um projeto em uma solução
    PM> Uninstall-Package -ProjectName MyProjectB EntityFramework

## Instalando uma versão específica de um pacote
    PM> Install-Package EntityFramework -Version 6.1.2  


## Instalando o Gerenciador de Pacotes NuGet
Para poder gerenciar os pacotes de seus projetos, você precisa do NuGet Package Manager. Esta é uma extensão do Visual Studio, explicada nos documentos oficiais: [Instalando e atualizando o cliente NuGet](https://docs.nuget.org/consume/installing-nuget).

A partir do Visual Studio 2012, o NuGet está incluído em todas as edições e pode ser usado em: Ferramentas -> Gerenciador de Pacotes NuGet -> Console do Gerenciador de Pacotes.

Você faz isso por meio do menu Ferramentas do Visual Studio, clicando em Extensões e Atualizações:

[![digite a descrição da imagem aqui][1]][1]

[1]: http://i.stack.imgur.com/zTzgp.png

Isso instala a GUI:

* Disponível clicando em "Gerenciar Pacotes NuGet..." em um projeto ou em sua pasta Referências

E o console do gerenciador de pacotes:

* Ferramentas -> Gerenciador de Pacotes NuGet -> Console do Gerenciador de Pacotes.

## Adicionando um feed de origem do pacote (MyGet, Klondike, ect)
    nuget sources add -name feedname -source http://sourcefeedurl

## Gerenciando pacotes por meio da interface do usuário
Ao clicar com o botão direito do mouse em um projeto (ou em sua pasta Referências), você pode clicar na opção "Gerenciar pacotes NuGet...". Isso mostra o [Diálogo do Gerenciador de Pacotes](https://docs.nuget.org/consume/package-manager-dialog).

[![digite a descrição da imagem aqui][1]][1]

[1]: http://i.stack.imgur.com/Fi0Uq.png

## Gerenciando Pacotes através do console
Clique nos menus Ferramentas -> Gerenciador de Pacotes NuGet -> Console do Gerenciador de Pacotes para mostrar o console em seu IDE. [Documentação oficial aqui](https://docs.nuget.org/consume/package-manager-console-powershell-reference).

Aqui você pode emitir, entre outros, comandos `install-package` que instalam o pacote inserido no "projeto padrão" atualmente selecionado:

    Install-Package Elmah

Você também pode fornecer o projeto para o qual instalar o pacote, substituindo o projeto selecionado na lista suspensa "Projeto padrão":

    Install-Package Elmah -ProjectName MyFirstWebsite

## Atualizando um pacote
Para atualizar um pacote, use o seguinte comando:

    PM> Update-Package EntityFramework
onde EntityFramework é o nome do pacote a ser atualizado. Observe que a atualização será executada para todos os projetos e, portanto, é diferente de `Install-Package EntityFramework`, que seria instalado apenas no "projeto padrão".

Você também pode especificar um único projeto explicitamente:

    PM> Update-Package EntityFramework -ProjectName MyFirstWebsite



## Desinstalando um pacote
    PM> Uninstall-Package EntityFramework  

## desinstale uma versão específica do pacote
    
    PM> uninstall-Package EntityFramework -Version 6.1.2

## Usando diferentes fontes de pacotes Nuget (locais) usando a interface do usuário
É comum que a empresa configure seu próprio servidor nuget para distribuição de pacotes entre diferentes equipes.

1. Vá para Solution Explorer e clique no botão <kbd>Right Mouse</kbd> e escolha `Manage NuGet Packages for Solution`

[![digite a descrição da imagem aqui][1]][1]

2. Na janela que se abre, clique em `Configurações`

[![digite a descrição da imagem aqui][2]][2]

3. Clique em `+` no canto superior direito e adicione o nome e a url que apontam para o seu servidor nuget local.

[![digite a descrição da imagem aqui][3]][3]


[1]: http://i.stack.imgur.com/PhB3d.png
[2]: http://i.stack.imgur.com/8vKM6.png
[3]: http://i.stack.imgur.com/h85QG.png

