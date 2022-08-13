---
title: "Sorgulanabilir arayüz"
slug: "sorgulanabilir-arayuz"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## LINQ sorgusunu SQL sorgusuna çevirme
"IQueryable" ve "IQueryable<T>" arabirimleri, geliştiricilerin bir LINQ sorgusunu ("dille tümleşik" bir sorgu) belirli bir veri kaynağına, örneğin ilişkisel bir veritabanına çevirmesine olanak tanır. C# ile yazılmış bu LINQ sorgusunu alın:

    var query = from book in books
                where book.Author == "Stephen King" 
                select book;

"books" değişkeni "IQueryable<Book>" uygulayan bir türdeyse, yukarıdaki sorgu sağlayıcıya ("IQueryable.Provider" özelliğinde ayarlanır) bir ifade ağacı biçiminde iletilir. kodun yapısını yansıtır.

Sağlayıcı, aşağıdakileri belirlemek için çalışma zamanında ifade ağacını inceleyebilir:

- 'Kitap' sınıfının 'Yazar' özelliği için bir yüklem olduğu;
- kullanılan karşılaştırma yönteminin 'eşittir' (`==`);
- eşit olması gereken değerin `"Stephen King"` olması.

Sağlayıcı, bu bilgilerle, C# sorgusunu çalışma zamanında bir SQL sorgusuna çevirebilir ve yalnızca yüklemle eşleşen kitapları almak için bu sorguyu ilişkisel bir veritabanına iletebilir:

    select *
    from Books
    where Author = 'Stephen King'

Sağlayıcı, "sorgu" değişkeni tekrarlandığında çağrılır ("IQueryable", "IEnumerable"ı uygular).

(Bu örnekte kullanılan sağlayıcı, hangi tablonun sorgulanacağını ve C# sınıfının özelliklerinin tablonun sütunlarıyla nasıl eşleştirileceğini bilmek için fazladan bazı meta verilere ihtiyaç duyacaktır, ancak bu tür meta veriler, "IQueryable" arabiriminin kapsamı dışındadır. )

