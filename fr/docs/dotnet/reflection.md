---
title: "Réflexion"
slug: "reflexion"
draft: false
images: []
weight: 9898
type: docs
toc: true
---

## Qu'est-ce qu'un assemblage ?
Les assemblages sont le bloc de construction de toute application [Common Language Runtime (CLR)][CLR].
Chaque type que vous définissez, ainsi que ses méthodes, ses propriétés et son bytecode, est compilé et empaqueté dans un Assembly.

    using System.Reflection;

<b></b>
    
    Assembly assembly = this.GetType().Assembly;   
    
Les assemblages sont auto-documentés : ils ne contiennent pas seulement des types, des méthodes et leur code IL, mais également les métadonnées nécessaires pour les inspecter et les consommer, à la fois à la compilation et à l'exécution :

    Assembly assembly = Assembly.GetExecutingAssembly();

    foreach (var type in assembly.GetTypes())
    {
        Console.WriteLine(type.FullName);
    }
 
Les assemblages ont des noms qui décrivent leur identité complète et unique :

    Console.WriteLine(typeof(int).Assembly.FullName);
    // Will print: "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"

Si ce nom inclut un `PublicKeyToken`, il s'agit d'un *nom fort*. L'attribution d'un nom fort à un assembly est le processus de création d'une signature à l'aide de la clé privée qui correspond à la clé publique distribuée avec l'assembly. Cette signature est ajoutée au manifeste de l'assembly, qui contient les noms et les hachages de tous les fichiers qui composent l'assembly, et son "PublicKeyToken" devient une partie du nom. Les assemblys qui ont le même nom fort doivent être identiques ; les noms forts sont utilisés dans la gestion des versions et pour éviter les conflits d'assemblage.

[CLR] : https://en.wikipedia.org/wiki/Common_Language_Runtime

## Comparer deux objets avec réflexion
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

**Remarque :** cet exemple effectue une comparaison basée sur un champ (ignorez les champs et les propriétés statiques) pour plus de simplicité


## Comment créer un objet de T en utilisant Reflection
Utilisation du constructeur par défaut

    T variable = Activator.CreateInstance(typeof(T));


Utilisation d'un constructeur paramétré

    T variable = Activator.CreateInstance(typeof(T), arg1, arg2);

## Création d'un objet et définition des propriétés à l'aide de la réflexion
Disons que nous avons une classe "Classy" qui a la propriété Propertua

    public class Classy
    {
        public string Propertua {get; set;}
    }

pour définir `Propertua` en utilisant la réflexion :

    var typeOfClassy = typeof (Classy);
    var classy = new Classy();
    var prop = typeOfClassy.GetProperty("Propertua");
    prop.SetValue(classy, "Value");

## Obtenir un attribut d'une énumération avec réflexion (et le mettre en cache)
Les attributs peuvent être utiles pour indiquer les métadonnées sur les énumérations. L'obtention de la valeur de ceci peut être lente, il est donc important de mettre les résultats en cache.

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

