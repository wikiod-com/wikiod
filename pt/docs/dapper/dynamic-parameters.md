---
title: "Parâmetros dinâmicos"
slug: "parametros-dinamicos"
draft: false
images: []
weight: 9497
type: docs
toc: true
---

## Uso básico
Nem sempre é possível empacotar ordenadamente todos os parâmetros em um único objeto/chamada. Para ajudar em cenários mais complicados, o dapper permite que o parâmetro `param` seja uma instância `IDynamicParameters`. Se você fizer isso, seu método `AddParameters` personalizado será chamado no momento apropriado e receberá o comando para anexar. Na maioria dos casos, no entanto, é suficiente usar o tipo `DynamicParameters` pré-existente:

    var p = new DynamicParameters(new { a = 1, b = 2 });
    p.Add("c", dbType: DbType.Int32, direction: ParameterDirection.Output);
    connection.Execute(@"set @c = @a + @b", p);
    int updatedValue = p.Get<int>("@c");

Isso mostra:

- (opcional) população de um objeto existente
- (opcional) adicionar parâmetros adicionais em tempo real
- passando os parâmetros para o comando
- recuperando qualquer valor atualizado após o término do comando

Observe que, devido à forma como os protocolos RDBMS funcionam, geralmente só é confiável obter valores de parâmetro atualizados **depois** de qualquer dado (de uma operação `Query` ou QueryMultiple`) ter sido **totalmente** consumido (por exemplo, em SQL Server, os valores de parâmetro atualizados estão no *final* do fluxo TDS).

## Parâmetros dinâmicos no Dapper
    
    connection.Execute(@"some Query with @a,@b,@c", new {a=somevalueOfa,b=somevalueOfb,c=somevalueOfc});

## Usando um objeto de modelo
Você pode usar uma instância de um objeto para formar seus parâmetros

    public class SearchParameters {
      public string SearchString { get; set; }
      public int Page { get; set; }
    }

    var template= new SearchParameters {
      SearchString = "Dapper",
      Page = 1
    };

    var p = new DynamicParameters(template);

Você também pode usar um objeto anônimo ou um `Dicionário`

