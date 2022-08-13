---
title: "Reflexión"
slug: "reflexion"
draft: false
images: []
weight: 9898
type: docs
toc: true
---

## ¿Qué es una asamblea?
Los ensamblados son la base de cualquier aplicación [Common Language Runtime (CLR)][CLR].
Cada tipo que defina, junto con sus métodos, propiedades y su código de bytes, se compila y empaqueta dentro de un ensamblado.

    using System.Reflection;

<b></b>
    
    Assembly assembly = this.GetType().Assembly;   
    
Los ensamblajes se autodocumentan: no solo contienen tipos, métodos y su código IL, sino también los metadatos necesarios para inspeccionarlos y consumirlos, tanto en tiempo de compilación como de ejecución:

    Assembly assembly = Assembly.GetExecutingAssembly();

    foreach (var type in assembly.GetTypes())
    {
        Console.WriteLine(type.FullName);
    }
 
Los ensamblajes tienen nombres que describen su identidad única y completa:

    Console.WriteLine(typeof(int).Assembly.FullName);
    // Will print: "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"

Si este nombre incluye un `PublicKeyToken`, se denomina *nombre seguro*. La denominación segura de un ensamblado es el proceso de crear una firma mediante el uso de la clave privada que corresponde a la clave pública distribuida con el ensamblado. Esta firma se agrega al manifiesto del ensamblado, que contiene los nombres y hash de todos los archivos que componen el ensamblado, y su `PublicKeyToken` pasa a formar parte del nombre. Los ensamblados que tienen el mismo nombre seguro deben ser idénticos; los nombres seguros se utilizan en el control de versiones y para evitar conflictos de ensamblado.

[CLR]: https://en.wikipedia.org/wiki/Common_Language_Runtime

## Compara dos objetos con reflejo
    public class Equatable
    {
        public string field1;

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;

            var type = obj.GetType();
            if (GetType() != type)
                return false;

            var fields = type.GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
            foreach (var field in fields)
                if (field.GetValue(this) != field.GetValue(obj))
                    return false;

            return true;
        }

        public override int GetHashCode()
        {
            var accumulator = 0;
            var fields = GetType().GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
            foreach (var field in fields)
                accumulator = unchecked ((accumulator * 937) ^ field.GetValue(this).GetHashCode());

            return accumulator;
        }
    }

**Nota:** este ejemplo hace una comparación basada en campos (ignora los campos estáticos y las propiedades) para simplificar


## Cómo crear un objeto de T usando Reflection
Usando el constructor por defecto

    T variable = Activator.CreateInstance(typeof(T));


Usando constructor parametrizado

    T variable = Activator.CreateInstance(typeof(T), arg1, arg2);

## Creación de objetos y configuración de propiedades mediante la reflexión
Digamos que tenemos una clase `Classy` que tiene propiedad Propertua

    public class Classy
    {
        public string Propertua {get; set;}
    }

para establecer `Propertua` usando la reflexión:

    var typeOfClassy = typeof (Classy);
    var classy = new Classy();
    var prop = typeOfClassy.GetProperty("Propertua");
    prop.SetValue(classy, "Value");

## Obtener un atributo de una enumeración con reflejo (y almacenarlo en caché)
Los atributos pueden ser útiles para indicar metadatos en las enumeraciones. Obtener el valor de esto puede ser lento, por lo que es importante almacenar en caché los resultados.

        private static Dictionary<object, object> attributeCache = new Dictionary<object, object>();

        public static T GetAttribute<T, V>(this V value)
            where T : Attribute
            where V : struct
        {
            object temp;

            // Try to get the value from the static cache.
            if (attributeCache.TryGetValue(value, out temp))
            {
                return (T) temp;
            }
            else
            {
                // Get the type of the struct passed in.
                Type type = value.GetType();   
                FieldInfo fieldInfo = type.GetField(value.ToString());

                // Get the custom attributes of the type desired found on the struct.
                T[] attribs = (T[])fieldInfo.GetCustomAttributes(typeof(T), false);

                // Return the first if there was a match.
                var result = attribs.Length > 0 ? attribs[0] : null;

                // Cache the result so future checks won't need reflection.
                attributeCache.Add(value, result);

                return result;
            }
        }

