---
title: "Introdução ao Dapper.NET"
slug: "introducao-ao-dappernet"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Instale o Dapper do Nuget
Ou pesquise na GUI do Visual Studio:

Ferramentas > Gerenciador de Pacotes NuGet > Gerenciar Pacotes para Solução... (Visual Studio 2015)

[![captura de tela da interface do gerenciador de pacotes do Visual Studio com Dapper sendo selecionado][1]][1]

Ou execute este comando em uma instância do Nuget Power Shell para instalar a versão estável mais recente

    Install-Package Dapper

Ou para uma versão específica

    Install-Package Dapper -Version 1.42.0

[1]: http://i.stack.imgur.com/sWn6V.png

## Usando Dapper em C#
    using System.Data;
    using System.Linq;
    using Dapper;
    
    class Program
    {
        static void Main()
        {
            using (IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true"))
            {
                db.Open();
                var result = db.Query<string>("SELECT 'Hello World'").Single();
                Console.WriteLine(result);
            }
        }
    }

Envolver a conexão em um [bloco `Using`](https://www.wikiod.com/pt/docs/c%23/38/using-statement/157/cleaner-dispose-syntax) fechará a conexão

## Usando Dapper no LINQPad
[LINQPad](http://www.linqpad.net/) é ótimo para testar consultas de banco de dados e inclui [integração NuGet](http://www.linqpad.net/Purchase.aspx#NuGet). Para usar o Dapper no LINQPad, pressione **F4** para abrir as Propriedades da Consulta e selecione **Adicionar NuGet**. Pesquise **dapper dot net** e selecione **Add To Query**. Você também vai querer clicar em **Add namespaces** e destacar Dapper para incluir os métodos de extensão em sua consulta LINQPad.

Assim que o Dapper estiver ativado, você poderá alterar o menu suspenso Idioma para **Programa C#**, mapear os resultados da consulta para classes C# e usar o método .Dump() para inspecionar os resultados:

void Principal()
	{
using (IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true")){
db.Abrir();
var escalar = db.Query<string>("SELECT GETDATE()").SingleOrDefault();
scalar.Dump("Este é um resultado escalar de string:");
			
var resultados = db.Query<meuobjeto>(@"
SELECIONE DE (
VALORES (1,'um'),
(2, 'dois'),
(3, 'três')
) AS minhatabela(id,nome)");
results.Dump("Esta é uma tabela mapeada para uma classe:");
		}
	}
	
// Defina outros métodos e classes aqui
class meuobjeto {
public int id { get; definir; }
public string name { get; definir; }
	}

Os resultados ao executar o programa ficariam assim:

[![Captura de tela do LINQPad][1]][1]


[1]: http://i.stack.imgur.com/swXB1.png

