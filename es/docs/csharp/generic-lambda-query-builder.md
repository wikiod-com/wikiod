---
title: "Generador de consultas de Lambda genérico"
slug: "generador-de-consultas-de-lambda-generico"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

La clase se llama `ExpressionBuilder`. Tiene tres propiedades:

     private static readonly MethodInfo ContainsMethod = typeof(string).GetMethod("Contains", new[] { typeof(string) });
     private static readonly MethodInfo StartsWithMethod = typeof(string).GetMethod("StartsWith", new[] { typeof(string) });
     private static readonly MethodInfo EndsWithMethod = typeof(string).GetMethod("EndsWith", new[] { typeof(string) });

Un método público `GetExpression` que devuelve la expresión lambda y tres métodos privados:
 

- `Expresión GetExpression<T>`
- `Expresión binaria GetExpression<T>`
- `ConstanteExpresión ObtenerConstante`

Todos los métodos se explican en detalle en los ejemplos.

## Clase de filtro de consulta
Esta clase contiene valores de filtros de predicado.

    public class QueryFilter
    {
        public string PropertyName { get; set; }
        public string Value { get; set; }
        public Operator Operator { get; set; }

        // In the query {a => a.Name.Equals("Pedro")} 
        // Property name to filter - propertyName = "Name"
        // Filter value - value = "Pedro"
        // Operation to perform - operation = enum Operator.Equals
        public QueryFilter(string propertyName, string value, Operator operatorValue)
        {
            PropertyName = propertyName;
            Value = value;
            Operator = operatorValue;
        }       
    }

Enum para contener los valores de las operaciones:

     public enum Operator
    {
        Contains,
        GreaterThan,
        GreaterThanOrEqual,
        LessThan,
        LessThanOrEqualTo,
        StartsWith,
        EndsWith,
        Equals,
        NotEqual
    }
    


## Método ObtenerExpresión
    public static Expression<Func<T, bool>> GetExpression<T>(IList<QueryFilter> filters)
    {
        Expression exp = null;
        
        // Represents a named parameter expression. {parm => parm.Name.Equals()}, it is the param part
        // To create a ParameterExpression need the type of the entity that the query is against an a name
        // The type is possible to find with the generic T and the name is fixed parm
        ParameterExpression param = Expression.Parameter(typeof(T), "parm");
    
        // It is good parctice never trust in the client, so it is wise to validate.
        if (filters.Count == 0)
            return null;
    
        // The expression creation differ if there is one, two or more filters.    
        if (filters.Count != 1)
        {
            if (filters.Count == 2)
                // It is result from direct call.
                // For simplicity sake the private overloads will be explained in another example.
                exp = GetExpression<T>(param, filters[0], filters[1]);
            else
            {
                // As there is no method for more than two filters, 
                // I iterate through all the filters and put I in the query two at a time
                while (filters.Count > 0)
                {
                    // Retreive the first two filters
                    var f1 = filters[0];
                    var f2 = filters[1];
                    
                    // To build a expression with a conditional AND operation that evaluates 
                    // the second operand only if the first operand evaluates to true.
                    // It needed to use the BinaryExpression a Expression derived class 
                    // That has the AndAlso method that join two expression together
                    exp = exp == null ? GetExpression<T>(param, filters[0], filters[1]) : Expression.AndAlso(exp, GetExpression<T>(param, filters[0], filters[1]));
                    
                    // Remove the two just used filters, for the method in the next iteration finds the next filters
                    filters.Remove(f1);
                    filters.Remove(f2);
                    
                    // If it is that last filter, add the last one and remove it
                    if (filters.Count == 1)
                    {
                        exp = Expression.AndAlso(exp, GetExpression<T>(param, filters[0]));
                        filters.RemoveAt(0);
                    }
                }
            }
        }
        else
            // It is result from direct call.
            exp = GetExpression<T>(param, filters[0]);
    
               // converts the Expression into Lambda and retuns the query
        return Expression.Lambda<Func<T, bool>>(exp, param);
    }

## GetExpression Sobrecarga privada
# Para un filtro:
Aquí es donde se crea la consulta, recibe un parámetro de expresión y un filtro.


    private static Expression GetExpression<T>(ParameterExpression param, QueryFilter queryFilter)
    {
        // Represents accessing a field or property, so here we are accessing for example:
        // the property "Name" of the entity
        MemberExpression member = Expression.Property(param, queryFilter.PropertyName);

        //Represents an expression that has a constant value, so here we are accessing for example:
        // the values of the Property "Name".
        // Also for clarity sake the GetConstant will be explained in another example.
        ConstantExpression constant = GetConstant(member.Type, queryFilter.Value);
    
        // With these two, now I can build the expression
        // every operator has it one way to call, so the switch will do.
        switch (queryFilter.Operator)
        {
            case Operator.Equals:
                return Expression.Equal(member, constant);
    
            case Operator.Contains:
                return Expression.Call(member, ContainsMethod, constant);
    
            case Operator.GreaterThan:
                return Expression.GreaterThan(member, constant);
    
            case Operator.GreaterThanOrEqual:
                return Expression.GreaterThanOrEqual(member, constant);
    
            case Operator.LessThan:
                return Expression.LessThan(member, constant);
    
            case Operator.LessThanOrEqualTo:
                return Expression.LessThanOrEqual(member, constant);
    
            case Operator.StartsWith:
                return Expression.Call(member, StartsWithMethod, constant);
    
            case Operator.EndsWith:
                return Expression.Call(member, EndsWithMethod, constant);
        }
    
        return null;
    }

# Para dos filtros:
Devuelve la instancia de BinaryExpresion en lugar de la expresión simple.

    private static BinaryExpression GetExpression<T>(ParameterExpression param, QueryFilter filter1, QueryFilter filter2)
    {
        // Built two separated expression and join them after.
        Expression result1 = GetExpression<T>(param, filter1);
        Expression result2 = GetExpression<T>(param, filter2);
        return Expression.AndAlso(result1, result2);
    }

## Método de expresión constante
`ConstantExpression` debe ser del mismo tipo que `MemberExpression`. El valor en este ejemplo es una cadena, que se convierte antes de crear la instancia `ConstantExpression`.

    private static ConstantExpression GetConstant(Type type, string value)
    {
        // Discover the type, convert it, and create ConstantExpression 
        ConstantExpression constant = null;
        if (type == typeof(int))
        {
            int num;
            int.TryParse(value, out num);
            constant = Expression.Constant(num);
        }
        else if(type == typeof(string))
        {
            constant = Expression.Constant(value);
        }
        else if (type == typeof(DateTime))
        {
            DateTime date;
            DateTime.TryParse(value, out date);
            constant = Expression.Constant(date);
        }
        else if (type == typeof(bool))
        {                
            bool flag;
            if (bool.TryParse(value, out flag))
            {
                flag = true;
            }
            constant = Expression.Constant(flag);
        }
        else if (type == typeof(decimal))
        {
            decimal number;
            decimal.TryParse(value, out number);
            constant = Expression.Constant(number);
        }
        return constant;
    }

## Uso
Collection<QueryFilter> filtros = new List<QueryFilter>();
     QueryFilter filter = new QueryFilter("Name", "Burger", Operator.StartsWith);
     filters.Add(filter);

     Expression<Func<Food, bool>> query = ExpressionBuilder.GetExpression<Food>(filters);
    
En este caso, es una consulta contra la entidad Alimentos, que desea encontrar todos los alimentos que comienzan con "Hamburguesa" en el nombre.

# Producción:

    query = {parm => a.parm.StartsWith("Burger")}


    Expression<Func<T, bool>> GetExpression<T>(IList<QueryFilter> filters)

