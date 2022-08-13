---
title: "Tip İşleyiciler"
slug: "tip-isleyiciler"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

Tür İşleyicileri, veritabanı türlerinin .Net özel türlerine dönüştürülmesine izin verir.

## TypeHandler Yükleme
Yukarıdaki tip işleyici, 'AddTypeHandler' yöntemi kullanılarak 'SqlMapper' içine kurulabilir.

    SqlMapper.AddTypeHandler<IHtmlString>(new IHtmlStringTypeHandler());

Tür çıkarımı, genel tür parametresini atlamanıza olanak tanır:

    SqlMapper.AddTypeHandler(new IHtmlStringTypeHandler());

Ayrıca, açık bir "Type" argümanı alan iki argümanlı bir aşırı yükleme var:

    SqlMapper.AddTypeHandler(typeof(IHtmlString), new IHtmlStringTypeHandler());

## varchar'ı IHtmlString'e dönüştürme
    public class IHtmlStringTypeHandler : SqlMapper.TypeHandler<IHtmlString>
    {
        public override void SetValue(
            IDbDataParameter parameter, 
            IHtmlString value)
        {
            parameter.DbType = DbType.String;
            parameter.Value = value?.ToHtmlString();
        }

        public override IHtmlString Parse(object value)
        {
            return MvcHtmlString.Create(value?.ToString());
        }
    }

