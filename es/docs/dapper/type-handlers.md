---
title: "Controladores de tipo"
slug: "controladores-de-tipo"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

Los controladores de tipos permiten que los tipos de bases de datos se conviertan en tipos personalizados de .Net.

## Instalar un controlador de tipos
El controlador de tipo anterior se puede instalar en `SqlMapper` utilizando el método `AddTypeHandler`.

    SqlMapper.AddTypeHandler<IHtmlString>(new IHtmlStringTypeHandler());

La inferencia de tipo le permite omitir el parámetro de tipo genérico:

    SqlMapper.AddTypeHandler(new IHtmlStringTypeHandler());

También hay una sobrecarga de dos argumentos que toma un argumento `Tipo` explícito:

    SqlMapper.AddTypeHandler(typeof(IHtmlString), new IHtmlStringTypeHandler());

## Convirtiendo varchar a IHtmlString
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

