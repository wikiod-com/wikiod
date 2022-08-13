---
title: "Manipuladores de tipo"
slug: "manipuladores-de-tipo"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

Os manipuladores de tipo permitem que os tipos de banco de dados sejam convertidos em tipos personalizados .Net.

## Instalando um TypeHandler
O manipulador de tipo acima pode ser instalado em `SqlMapper` usando o método `AddTypeHandler`.

    SqlMapper.AddTypeHandler<IHtmlString>(new IHtmlStringTypeHandler());

A inferência de tipo permite omitir o parâmetro de tipo genérico:

    SqlMapper.AddTypeHandler(new IHtmlStringTypeHandler());

Há também uma sobrecarga de dois argumentos que recebe um argumento `Type` explícito:

    SqlMapper.AddTypeHandler(typeof(IHtmlString), new IHtmlStringTypeHandler());

## Convertendo varchar para IHtmlString
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

