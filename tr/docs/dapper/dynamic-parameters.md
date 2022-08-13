---
title: "Dinamik Parametreler"
slug: "dinamik-parametreler"
draft: false
images: []
weight: 9497
type: docs
toc: true
---

## Temel Kullanım
Tüm parametreleri tek bir nesnede/çağrıda düzgün bir şekilde paketlemek her zaman mümkün değildir. Daha karmaşık senaryolara yardımcı olmak için zarif, "param" parametresinin bir "IDynamicParameters" örneği olmasına izin verir. Bunu yaparsanız, özel `AddParameters` yönteminiz uygun zamanda çağrılır ve eklenecek komutu verir. Ancak çoğu durumda önceden var olan 'DynamicParameters' türünü kullanmak yeterlidir:

    var p = new DynamicParameters(new { a = 1, b = 2 });
    p.Add("c", dbType: DbType.Int32, direction: ParameterDirection.Output);
    connection.Execute(@"set @c = @a + @b", p);
    int updatedValue = p.Get<int>("@c");

Bu gösterir ki:

- (isteğe bağlı) mevcut bir nesneden nüfus
- (isteğe bağlı) anında ek parametreler ekleme
- parametreleri komuta iletmek
- komut tamamlandıktan sonra herhangi bir güncellenmiş değeri alma

RDBMS protokollerinin çalışma şeklinden dolayı, genellikle yalnızca herhangi bir veri (bir "Sorgu" veya QueryMultiple" işleminden gelen) **tamamen** tüketildikten **sonra** güncellenmiş parametre değerleri elde etmenin güvenilir olduğunu unutmayın. SQL Server, güncellenmiş parametre değerleri TDS akışının *sonunda* bulunur).

## Dapper'da Dinamik Parametreler
    
    connection.Execute(@"some Query with @a,@b,@c", new {a=somevalueOfa,b=somevalueOfb,c=somevalueOfc});

## Bir şablon nesnesi kullanma
Parametrelerinizi oluşturmak için bir nesnenin örneğini kullanabilirsiniz.

    public class SearchParameters {
      public string SearchString { get; set; }
      public int Page { get; set; }
    }

    var template= new SearchParameters {
      SearchString = "Dapper",
      Page = 1
    };

    var p = new DynamicParameters(template);

Ayrıca anonim bir nesne veya bir "Sözlük" de kullanabilirsiniz.

