---
title: "Consulta básica"
slug: "consulta-basica"
draft: false
images: []
weight: 9748
type: docs
toc: true
---

## Sintaxis
- público estático IEnumerable&lt;T&gt; Consulta&lt;T&gt;(esta IDbConnection cnn, cadena sql, parámetro de objeto = nulo, transacción SqlTransaction = nulo, bool almacenado en búfer = verdadero)
- public static IEnumerable&lt;dynamic&gt; Consulta (este IDbConnection cnn, cadena sql, parámetro de objeto = nulo, transacción SqlTransaction = nulo, bool almacenado en búfer = verdadero)

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| CNN | Su conexión a la base de datos, que ya debe estar abierta. |
| sql | Comando a ejecutar. |
| parámetro | Objeto del que extraer parámetros. |
| transacción | Transacción de la que forma parte esta consulta, si corresponde. |
| amortiguado | Si almacenar o no en el búfer la lectura de los resultados de la consulta. Este es un parámetro opcional y el valor predeterminado es verdadero. Cuando buffered es verdadero, los resultados se almacenan en un `List<T>` y luego se devuelven como un `IEnumerable<T>` que es seguro para la enumeración múltiple. Cuando buffered es falso, la conexión sql se mantiene abierta hasta que termine de leer, lo que le permite procesar una sola fila a la vez en la memoria. Múltiples enumeraciones generarán conexiones adicionales a la base de datos. Mientras que el falso almacenado en búfer es altamente eficiente para reducir el uso de la memoria si solo mantiene fragmentos muy pequeños de los registros devueltos, tiene una [sobrecarga de rendimiento considerable] (http://stackoverflow.com/a/30493725/37055) en comparación con la materialización ansiosa del resultado. establecer. Por último, si tiene numerosas conexiones sql simultáneas sin búfer, debe considerar la inanición del grupo de conexiones, lo que hace que las solicitudes se bloqueen hasta que las conexiones estén disponibles. |


## Consultando por un tipo estático
Para tipos conocidos en tiempo de compilación, use un parámetro genérico con `Query<T>`.

    public class Dog
    {
        public int? Age { get; set; }
        public Guid Id { get; set; }
        public string Name { get; set; }
        public float? Weight { get; set; }
    
        public int IgnoredProperty { get { return 1; } }
    }    
    
    //
    IDBConnection db = /* ... */;

    var @params = new { age = 3 };
    var sql = "SELECT * FROM dbo.Dogs WHERE Age = @age";

    IEnumerable<Dog> dogs = db.Query<Dog>(sql, @params);

## Consulta de tipos dinámicos
También puede consultar dinámicamente si deja fuera el tipo genérico.
    
    IDBConnection db = /* ... */;
    IEnumerable<dynamic> result = db.Query("SELECT 1 as A, 2 as B");

    var first = result.First();
    int a = (int)first.A; // 1
    int b = (int)first.B; // 2

## Consulta con parámetros dinámicos
    var color = "Black";
    var age = 4;

    var query = "Select * from Cats where Color = :Color and Age > :Age";
    var dynamicParameters = new DynamicParameters();
    dynamicParameters.Add("Color", color);
    dynamicParameters.Add("Age", age);

    using (var connection = new SqlConnection(/* Your Connection String Here */))
    {
        IEnumerable<dynamic> results = connection.Query(query, dynamicParameters);
    }

