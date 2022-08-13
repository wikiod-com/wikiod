---
title: "Referência de sintaxe de parâmetro"
slug: "referencia-de-sintaxe-de-parametro"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| `este cnn` | A conexão de banco de dados subjacente - o `this` denota um método de extensão; a conexão não precisa estar aberta - se não estiver aberta, ela é aberta e fechada automaticamente.
| `<T>` / `Tipo` | (opcional) O tipo de objeto a ser retornado; se a API não genérica / não `Type` for usada, um objeto `dynamic` será retornado por linha, simulando uma propriedade nomeada por nome de coluna retornado da consulta (este objeto `dynamic` também implementa `IDicionary<string,object >`).
| `sql` | O SQL para executar
| `param` | (opcional) Os parâmetros a serem incluídos.
| `transação` | (opcional) A transação do banco de dados a ser associada ao comando
| `buffered` | (opcional) Se deve pré-consumir os dados em uma lista (o padrão), versus expor um 'IEnumerable' aberto no leitor ao vivo
| `comandoTimeout` | (opcional) O tempo limite a ser usado no comando; se não especificado, `SqlMapper.Settings.CommandTimeout` é assumido (se especificado)
| `commandType` | O tipo de comando que está sendo executado; padrão para `CommandText`

A sintaxe para expressar parâmetros varia entre RDBMS. Todos os exemplos acima usam a sintaxe do SQL Server, ou seja, `@foo`; entretanto, `?foo` e `:foo` também devem funcionar bem.

## Valor embutido
Às vezes, a conveniência de um parâmetro (em termos de manutenção e expressividade), pode ser superada pelo seu custo em desempenho para tratá-lo como parâmetro. Por exemplo, quando o tamanho da página é fixado por uma definição de configuração. Ou um valor de status corresponde a um valor `enum`. Considerar:

    var orders = connection.Query<Order>(@"
    select top (@count) * -- these brackets are an oddity of SQL Server
    from Orders
    where CustomerId = @customerId
    and Status = @open", new { customerId, count = PageSize, open = OrderStatus.Open });

O único parâmetro *real* aqui é `customerId` - os outros dois são pseudo-parâmetros que não serão alterados. Muitas vezes, o RDBMS pode fazer um trabalho melhor se detectá-los como constantes. O Dapper tem uma sintaxe especial para isso - `{=name}` em vez de `@name` - que *somente* se aplica a tipos numéricos. (Isso minimiza qualquer superfície de ataque da injeção de SQL). Um exemplo é o seguinte:

    var orders = connection.Query<Order>(@"
    select top {=count} *
    from Orders
    where CustomerId = @customerId
    and Status = {=open}", new { customerId, count = PageSize, open = OrderStatus.Open });

Dapper substitui valores por literais antes de emitir o SQL, então o RDBMS realmente vê algo como:

    select top 10 *
    from Orders
    where CustomerId = @customerId
    and Status = 3

Isso é particularmente útil ao permitir que os sistemas RDBMS não apenas tomem decisões melhores, mas também abram planos de consulta que os parâmetros reais impedem. Por exemplo, se um predicado de coluna for contra um parâmetro, um índice filtrado com valores específicos nessas colunas não poderá ser usado. Isso ocorre porque a consulta *próxima* pode ter um parâmetro diferente de um desses valores especificados.

Com valores literais, o otimizador de consulta pode fazer uso dos índices filtrados, pois sabe que o valor não pode ser alterado em consultas futuras.

## SQL parametrizado básico
O Dapper facilita seguir as melhores práticas por meio de SQL totalmente parametrizado.

![Bobby Tables](https://imgs.xkcd.com/comics/exploits_of_a_mom.png)

Os parâmetros são importantes, por isso o elegante torna mais fácil acertar. Você apenas expressa seus parâmetros da maneira normal para seu RDBMS (geralmente `@foo`, `?foo` ou `:foo`) e dá ao dapper um objeto que *tem um membro chamado `foo`*. A maneira mais comum de fazer isso é com um tipo anônimo:

    int id = 123;
    string name = "abc";
    connection.Execute("insert [KeyLookup](Id, Name) values(@id, @name)",
        new { id, name });

E é isso. Dapper irá adicionar os parâmetros necessários e tudo deve funcionar.

Usando seu modelo de objeto
---

Você também pode usar seu modelo de objeto existente como parâmetro:

    KeyLookup lookup = ... // some existing instance
    connection.Execute("insert [KeyLookup](Id, Name) values(@Id, @Name)", lookup);

O Dapper usa o texto do comando para determinar quais membros do objeto adicionar - geralmente não adiciona coisas desnecessárias como `Description`, `IsActive`, `CreationDate` porque o comando que emitimos claramente não os envolve - embora haja casos em que isso possa acontecer, por exemplo, se seu comando contiver:

    // TODO - removed for now; include the @Description in the insert

Ele não tenta descobrir que o acima é apenas um comentário.

Procedimentos armazenados
---

Os parâmetros para procedimentos armazenados funcionam exatamente da mesma forma, exceto que o dapper não pode tentar determinar o que deve/não deve ser incluído - tudo o que está disponível é tratado como um parâmetro. Por esse motivo, os tipos anônimos geralmente são preferidos:

    connection.Execute("KeyLookupInsert", new { id, name },
        commandType: CommandType.StoredProcedure);



## Expansões da lista
Um cenário comum em consultas de banco de dados é `IN (...)` onde a lista aqui é gerada em tempo de execução. A maioria dos RDBMS não tem uma boa metáfora para isso - e não existe uma solução universal *cross-RDBMS* para isso. Em vez disso, o dapper fornece uma expansão de comando automática suave. Tudo o que é necessário é um valor de parâmetro fornecido que seja `IEnumerable`. Um comando envolvendo `@foo` é expandido para `(@foo0,@foo1,@foo2,@foo3)` (para uma sequência de 4 itens). O uso mais comum disso seria `IN`:

    int[] orderIds = ...
    var orders = connection.Query<Order>(@"
    select *
    from Orders
    where Id in @orderIds", new { orderIds });

Isso se expande automaticamente para emitir o SQL apropriado para a busca de várias linhas:

    select *
    from Orders
    where Id in (@orderIds0, @orderIds1, @orderIds2, @orderIds3)

com os parâmetros `@orderIds0` etc sendo adicionados como valores retirados do array.
Observe que o fato de não ser SQL originalmente válido é intencional, para garantir que esse recurso não seja usado erroneamente. Este recurso também funciona corretamente com a dica de consulta `OPTIMIZE FOR` / `UNKNOWN` no SQL Server; se você usar:

    option (optimize for
        (@orderIds unknown))

ele irá expandir isso corretamente para:

    option (optimize for
        (@orderIds0 unknown, @orderIds1 unknown, @orderIds2 unknown, @orderIds3 unknown))

## Executando operações contra vários conjuntos de entrada
Às vezes, você quer fazer a mesma coisa várias vezes. Dapper suporta isso no método `Execute` se o parâmetro *outermost* (que geralmente é um único tipo anônimo, ou uma instância de modelo de domínio) é realmente fornecido como uma sequência `IEnumerable`. Por exemplo:

    Order[] orders = ...
    // update the totals
    connection.Execute("update Orders set Total=@Total where Id=@Id", orders);

Aqui, dapper está apenas fazendo um loop simples em nossos dados, essencialmente o mesmo que se tivéssemos feito:

    Order[] orders = ...
    // update the totals
    foreach(Order order in orders) {
        connection.Execute("update Orders set Total=@Total where Id=@Id", order);
    }

Esse uso se torna *particularmente* interessante quando combinado com a API `async` em uma conexão explicitamente configurada para todos os "Vários conjuntos de resultados ativos" - nesse uso, o dapper automaticamente *pipeline* as operações, então você não está pagando o custo de latência por linha. Isso requer um uso um pouco mais complicado,

    await connection.ExecuteAsync(
        new CommandDefinition(
            "update Orders set Total=@Total where Id=@Id", 
             orders, flags: CommandFlags.Pipelined))

Observe, no entanto, que você também pode querer investigar parâmetros com valor de tabela.

## Parâmetros pseudo-posicionais (para provedores que não suportam parâmetros nomeados)
Alguns provedores ADO.NET (mais notavelmente: OleDB) não suportam parâmetros *named*; os parâmetros são especificados apenas por *position*, com o espaço reservado `?`. Dapper não saberia qual membro usar para isso, então dapper permite uma sintaxe alternativa, `?foo?`; isso seria o mesmo que `@foo` ou `:foo` em outras variantes do SQL, exceto que o dapper **substituirá** o token do parâmetro completamente por `?` antes de executar a consulta.

Isso funciona em combinação com outros recursos, como expansão de lista, portanto, o seguinte é válido:

    string region = "North";
    int[] users = ...
    var docs = conn.Query<Document>(@"
         select * from Documents
         where Region = ?region?
         and OwnerId in ?users?", new { region, users }).AsList();

Os membros `.region` e `.users` são usados ​​de acordo, e o SQL emitido é (por exemplo, com 3 usuários):

         select * from Documents
         where Region = ?
         and OwnerId in (?,?,?)

Observe, no entanto, que o elegante **não** permite que o mesmo parâmetro seja usado várias vezes ao usar esse recurso; isso é para evitar ter que adicionar o mesmo valor de parâmetro (que pode ser grande) várias vezes. Se você precisar fazer referência ao mesmo valor várias vezes, considere declarar uma variável, por exemplo:

    declare @id int = ?id?; // now we can use @id multiple times in the SQL

Se as variáveis ​​não estiverem disponíveis, você pode usar nomes de membros duplicados nos parâmetros - isso também tornará óbvio que o valor está sendo enviado várias vezes:

    int id = 42;
    connection.Execute("... where ParentId = $id0$ ... SomethingElse = $id1$ ...",
          new { id0 = id, id1 = id });

