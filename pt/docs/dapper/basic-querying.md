---
title: "Consulta básica"
slug: "consulta-basica"
draft: false
images: []
weight: 9748
type: docs
toc: true
---

## Sintaxe
- IEnumerable estático público&lt;T&gt; Query&lt;T&gt;(este IDbConnection cnn, string sql, object param = null, SqlTransaction transaction = null, bool buffered = true)
- IEnumerable estático público&lt;dinâmico&gt; Query (este IDbConnection cnn, string sql, object param = null, SqlTransaction transaction = null, bool buffered = true)

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| cnn | Sua conexão com o banco de dados, que já deve estar aberta. |
| SQL | Comando para executar. |
| parâmetro | Objeto do qual extrair parâmetros. |
| transação | Transação da qual esta consulta faz parte, se houver. |
| tamponado | Se deve ou não armazenar em buffer a leitura dos resultados da consulta. Este é um parâmetro opcional com o padrão sendo true. Quando buffer é true, os resultados são armazenados em buffer em um `List<T>` e, em seguida, retornados como um `IEnumerable<T>` que é seguro para enumeração múltipla. Quando o buffer é false, a conexão sql é mantida aberta até que você termine de ler, permitindo que você processe uma única linha de cada vez na memória. Várias enumerações gerarão conexões adicionais com o banco de dados. Embora o buffer false seja altamente eficiente para reduzir o uso de memória, se você mantiver apenas fragmentos muito pequenos dos registros retornados, ele terá uma [sobrecarga de desempenho considerável](http://stackoverflow.com/a/30493725/37055) em comparação com a materialização ansiosa do resultado definir. Por fim, se você tiver várias conexões sql sem buffer simultâneas, precisará considerar a inanição do pool de conexões, fazendo com que as solicitações bloqueiem até que as conexões fiquem disponíveis. |


## Consultando um tipo estático
Para tipos conhecidos em tempo de compilação, use um parâmetro genérico com `Query<T>`.

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

## Consultando tipos dinâmicos
Você também pode consultar dinamicamente se deixar de fora o tipo genérico.
    
    IDBConnection db = /* ... */;
    IEnumerable<dynamic> result = db.Query("SELECT 1 as A, 2 as B");

    var first = result.First();
    int a = (int)first.A; // 1
    int b = (int)first.B; // 2

## Consulta com parâmetros dinâmicos
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

