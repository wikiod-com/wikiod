---
title: "Gestionnaires de types"
slug: "gestionnaires-de-types"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

Les gestionnaires de type permettent de convertir les types de base de données en types personnalisés .Net.

## Installer un TypeHandler
Le gestionnaire de type ci-dessus peut être installé dans `SqlMapper` en utilisant la méthode `AddTypeHandler`.

    SqlMapper.AddTypeHandler<IHtmlString>(new IHtmlStringTypeHandler());

L'inférence de type vous permet d'omettre le paramètre de type générique :

    SqlMapper.AddTypeHandler(new IHtmlStringTypeHandler());

Il existe également une surcharge à deux arguments qui prend un argument `Type` explicite :

    SqlMapper.AddTypeHandler(typeof(IHtmlString), new IHtmlStringTypeHandler());

## Conversion de varchar en IHtmlString
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

