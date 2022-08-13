---
title: "Resultados múltiples"
slug: "resultados-multiples"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

## Sintaxis
- SqlMapper.GridReader QueryMultiple estático público (este IDbConnection cnn, cadena sql, parámetro de objeto = nulo, transacción IDbTransaction = nulo, int? commandTimeout = nulo, CommandType? commandType = null)
- público estático SqlMapper.GridReader QueryMultiple (este IDbConnection cnn, comando CommandDefinition)

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| CNN | Su conexión a la base de datos, ya debe estar abierta |
| sql | La cadena sql a procesar contiene múltiples consultas |
| parámetro | Objeto del que extraer parámetros |
| SqlMapper.GridReader | Proporciona interfaces para leer múltiples conjuntos de resultados de una consulta Dapper |


## Ejemplo base de resultados múltiples
Para obtener varias cuadrículas en una sola consulta, se utiliza el método `QueryMultiple`. Esto le permite recuperar cada cuadrícula *secuencialmente* a través de llamadas sucesivas contra el 'GridReader' devuelto.

    var sql = @"select * from Customers where CustomerId = @id
                select * from Orders where CustomerId = @id
                select * from Returns where CustomerId = @id";
    
    using (var multi = connection.QueryMultiple(sql, new {id=selectedId}))
    {
       var customer = multi.Read<Customer>().Single();
       var orders = multi.Read<Order>().ToList();
       var returns = multi.Read<Return>().ToList();
    } 

