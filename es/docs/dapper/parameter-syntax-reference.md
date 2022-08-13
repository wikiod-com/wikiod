---
title: "Referencia de sintaxis de parámetros"
slug: "referencia-de-sintaxis-de-parametros"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| `esta cnn` | La conexión de la base de datos subyacente: `this` denota un método de extensión; no es necesario que la conexión esté abierta; si no está abierta, se abre y se cierra automáticamente.
| `<T>` / `Tipo` | (opcional) El tipo de objeto a devolver; si se usa la API no genérica / no `Type`, se devuelve un objeto `dynamic` por fila, simulando una propiedad nombrada por nombre de columna devuelta por la consulta (este objeto `dynamic` también implementa `IDicionary<string,object >`).
| `sql` | El SQL a ejecutar
| `parámetro` | (opcional) Los parámetros a incluir.
| `transacción` | (opcional) La transacción de la base de datos para asociar con el comando
| `amortiguado` | (opcional) Ya sea para consumir previamente los datos en una lista (el valor predeterminado), en lugar de exponer un `IEnumerable` abierto sobre el lector en vivo
| `tiempo de espera del comando` | (opcional) El tiempo de espera para usar en el comando; si no se especifica, se asume `SqlMapper.Settings.CommandTimeout` (si se especifica)
| `tipo de comando` | El tipo de comando que se está realizando; por defecto es `CommandText`

La sintaxis para expresar parámetros varía entre RDBMS. Todos los ejemplos anteriores utilizan la sintaxis de SQL Server, es decir, `@foo`; sin embargo, `?foo` y `:foo` también deberían funcionar bien.

## Valor en línea
A veces, la conveniencia de un parámetro (en términos de mantenimiento y expresividad) puede verse superada por su costo de rendimiento para tratarlo como un parámetro. Por ejemplo, cuando el tamaño de la página está fijado por un parámetro de configuración. O un valor de estado se compara con un valor `enum`. Considerar:

    var orders = connection.Query<Order>(@"
    select top (@count) * -- these brackets are an oddity of SQL Server
    from Orders
    where CustomerId = @customerId
    and Status = @open", new { customerId, count = PageSize, open = OrderStatus.Open });

El único parámetro *real* aquí es `customerId`; los otros dos son pseudoparámetros que en realidad no cambiarán. A menudo, el RDBMS puede hacer un mejor trabajo si los detecta como constantes. Dapper tiene una sintaxis especial para esto: `{=nombre}` en lugar de `@nombre`, que *solo* se aplica a tipos numéricos. (Esto minimiza cualquier superficie de ataque de la inyección de SQL). Un ejemplo es el siguiente:

    var orders = connection.Query<Order>(@"
    select top {=count} *
    from Orders
    where CustomerId = @customerId
    and Status = {=open}", new { customerId, count = PageSize, open = OrderStatus.Open });

Dapper reemplaza los valores con literales antes de emitir el SQL, por lo que el RDBMS realmente ve algo como:

    select top 10 *
    from Orders
    where CustomerId = @customerId
    and Status = 3

Esto es particularmente útil cuando permite que los sistemas RDBMS no solo tomen mejores decisiones, sino que abran planes de consulta que los parámetros reales impiden. Por ejemplo, si un predicado de columna está en contra de un parámetro, entonces no se puede usar un índice filtrado con valores específicos en esas columnas. Esto se debe a que la consulta *siguiente* puede tener un parámetro aparte de uno de esos valores especificados.

Con valores literales, el optimizador de consultas puede hacer uso de los índices filtrados ya que sabe que el valor no puede cambiar en futuras consultas.

## SQL parametrizado básico
Dapper facilita el seguimiento de las mejores prácticas mediante SQL completamente parametrizado.

![Mesas de Bobby](https://imgs.xkcd.com/comics/exploits_of_a_mom.png)

Los parámetros son importantes, por lo que dapper facilita hacerlo bien. Simplemente expresas tus parámetros de la manera normal para tu RDBMS (usualmente `@foo`, `?foo` o `:foo`) y le das a dapper un objeto que *tiene un miembro llamado `foo`*. La forma más común de hacer esto es con un tipo anónimo:

    int id = 123;
    string name = "abc";
    connection.Execute("insert [KeyLookup](Id, Name) values(@id, @name)",
        new { id, name });

Y eso es. Dapper agregará los parámetros requeridos y todo debería funcionar.

Usando su modelo de objeto
---

También puede usar su modelo de objeto existente como parámetro:

    KeyLookup lookup = ... // some existing instance
    connection.Execute("insert [KeyLookup](Id, Name) values(@Id, @Name)", lookup);

Dapper usa el texto del comando para determinar qué miembros del objeto agregar; por lo general, no agregará cosas innecesarias como `Descripción`, `IsActive`, `CreationDate` porque el comando que emitimos claramente no los involucra: aunque hay casos en los que podría hacer eso, por ejemplo, si su comando contiene:

    // TODO - removed for now; include the @Description in the insert

No intenta darse cuenta de que lo anterior es solo un comentario.

Procedimientos almacenados
---

Los parámetros de los procedimientos almacenados funcionan exactamente igual, excepto que Dapper no puede intentar determinar qué debe/no debe incluirse: todo lo disponible se trata como un parámetro. Por esa razón, generalmente se prefieren los tipos anónimos:

    connection.Execute("KeyLookupInsert", new { id, name },
        commandType: CommandType.StoredProcedure);



## Expansiones de lista
Un escenario común en consultas de base de datos es `IN (...)` donde la lista aquí se genera en tiempo de ejecución. La mayoría de los RDBMS carecen de una buena metáfora para esto, y no existe una solución universal *cross-RDBMS* para esto. En cambio, dapper proporciona una suave expansión automática de comandos. Todo lo que se requiere es un valor de parámetro proporcionado que sea `IEnumerable`. Un comando que implica `@foo` se expande a `(@foo0,@foo1,@foo2,@foo3)` (para una secuencia de 4 elementos). El uso más común de esto sería `IN`:

    int[] orderIds = ...
    var orders = connection.Query<Order>(@"
    select *
    from Orders
    where Id in @orderIds", new { orderIds });

Esto luego se expande automáticamente para emitir el SQL apropiado para la obtención de varias filas:

    select *
    from Orders
    where Id in (@orderIds0, @orderIds1, @orderIds2, @orderIds3)

con los parámetros `@orderIds0`, etc., que se agregan como valores tomados de la matriz.
Tenga en cuenta que el hecho de que originalmente no sea un SQL válido es intencional, para garantizar que esta característica no se use por error. Esta función también funciona correctamente con la sugerencia de consulta `OPTIMIZE FOR` / `UNKNOWN` en SQL Server; si utiliza:

    option (optimize for
        (@orderIds unknown))

expandirá esto correctamente a:

    option (optimize for
        (@orderIds0 unknown, @orderIds1 unknown, @orderIds2 unknown, @orderIds3 unknown))

## Realización de operaciones contra múltiples conjuntos de entrada
A veces, quieres hacer lo mismo varias veces. Dapper admite esto en el método `Execute` si el parámetro *outermost* (que suele ser un único tipo anónimo o una instancia de modelo de dominio) en realidad se proporciona como una secuencia `IEnumerable`. Por ejemplo:

    Order[] orders = ...
    // update the totals
    connection.Execute("update Orders set Total=@Total where Id=@Id", orders);

Aquí, dapper simplemente está haciendo un bucle simple en nuestros datos, esencialmente lo mismo que si hubiéramos hecho:

    Order[] orders = ...
    // update the totals
    foreach(Order order in orders) {
        connection.Execute("update Orders set Total=@Total where Id=@Id", order);
    }

Este uso se vuelve *particularmente* interesante cuando se combina con la API `async` en una conexión que está configurada explícitamente para todos los "Conjuntos de resultados activos múltiples". el costo de latencia por fila. Esto requiere un uso un poco más complicado,

    await connection.ExecuteAsync(
        new CommandDefinition(
            "update Orders set Total=@Total where Id=@Id", 
             orders, flags: CommandFlags.Pipelined))

Tenga en cuenta, sin embargo, que es posible que también desee investigar los parámetros con valores de tabla.

## Parámetros pseudoposicionales (para proveedores que no admiten parámetros con nombre)
Algunos proveedores de ADO.NET (sobre todo: OleDB) no admiten parámetros *con nombre*; en cambio, los parámetros se especifican solo por *posición*, con el marcador de posición `?`. Dapper no sabría qué miembro usar para estos, por lo que dapper permite una sintaxis alternativa, `?foo?`; esto sería lo mismo que `@foo` o `:foo` en otras variantes de SQL, excepto que dapper **reemplazará** el token de parámetro por completo con `?` antes de ejecutar la consulta.

Esto funciona en combinación con otras funciones, como la expansión de la lista, por lo que lo siguiente es válido:

    string region = "North";
    int[] users = ...
    var docs = conn.Query<Document>(@"
         select * from Documents
         where Region = ?region?
         and OwnerId in ?users?", new { region, users }).AsList();

Los miembros `.region` y `.users` se utilizan en consecuencia, y el SQL emitido es (por ejemplo, con 3 usuarios):

         select * from Documents
         where Region = ?
         and OwnerId in (?,?,?)

Tenga en cuenta, sin embargo, que dapper **no** permite que el mismo parámetro se use varias veces cuando se usa esta función; esto es para evitar tener que agregar el mismo valor de parámetro (que podría ser grande) varias veces. Si necesita hacer referencia al mismo valor varias veces, considere declarar una variable, por ejemplo:

    declare @id int = ?id?; // now we can use @id multiple times in the SQL

Si las variables no están disponibles, puede usar nombres de miembros duplicados en los parámetros; esto también hará que sea obvio que el valor se envía varias veces:

    int id = 42;
    connection.Execute("... where ParentId = $id0$ ... SomethingElse = $id1$ ...",
          new { id0 = id, id1 = id });

