---
title: "Type Handlers"
slug: "type-handlers"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

Type Handlers allow database types to be converted to .Net custom types.

## Installing a TypeHandler
The above type handler can be installed into `SqlMapper` using the `AddTypeHandler` method.

    SqlMapper.AddTypeHandler<IHtmlString>(new IHtmlStringTypeHandler());

Type inference allows you to omit the generic type parameter:

    SqlMapper.AddTypeHandler(new IHtmlStringTypeHandler());

There's also a two-argument overload which takes an explicit `Type` argument:

    SqlMapper.AddTypeHandler(typeof(IHtmlString), new IHtmlStringTypeHandler());

## Converting varchar to IHtmlString
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

