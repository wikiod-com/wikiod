---
title: "Vários resultados"
slug: "varios-resultados"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

## Sintaxe
- public static SqlMapper.GridReader QueryMultiple(este IDbConnection cnn, string sql, object param = null, IDbTransaction transaction = null, int? commandTimeout = null, CommandType? commandType = null)
- public static SqlMapper.GridReader QueryMultiple(este IDbConnection cnn, comando CommandDefinition)

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| cnn | Sua conexão com o banco de dados já deve estar aberta |
| SQL | A string sql a ser processada contém várias consultas |
| parâmetro | Objeto para extrair parâmetros |
| SqlMapper.GridReader | Fornece interfaces para leitura de vários conjuntos de resultados de uma consulta Dapper |


## Exemplo de vários resultados básicos
Para buscar várias grades em uma única consulta, o método `QueryMultiple` é usado. Isso permite que você recupere cada grade *sequencialmente* por meio de chamadas sucessivas contra o `GridReader` retornado.

    var sql = @"select * from Customers where CustomerId = @id
                select * from Orders where CustomerId = @id
                select * from Returns where CustomerId = @id";
    
    using (var multi = connection.QueryMultiple(sql, new {id=selectedId}))
    {
       var customer = multi.Read<Customer>().Single();
       var orders = multi.Read<Order>().ToList();
       var returns = multi.Read<Return>().ToList();
    } 

