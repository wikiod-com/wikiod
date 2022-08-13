---
title: "Les attributs"
slug: "les-attributs"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Création d'un attribut personnalisé
    //1) All attributes should be inherited from System.Attribute
    //2) You can customize your attribute usage (e.g. place restrictions) by using System.AttributeUsage Attribute
    //3) You can use this attribute only via reflection in the way it is supposed to be used
    //4) MethodMetadataAttribute is just a name. You can use it without "Attribute" postfix - e.g. [MethodMetadata("This text could be retrieved via reflection")].
    //5) You can overload an attribute constructors
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Class)]
    public class MethodMetadataAttribute : System.Attribute
    {
        //this is custom field given just for an example
        //you can create attribute without any fields
        //even an empty attribute can be used - as marker
        public string Text { get; set; }
    
        //this constructor could be used as [MethodMetadata]
        public MethodMetadataAttribute ()
        {
        }
    
        //This constructor could be used as [MethodMetadata("String")]
        public MethodMetadataAttribute (string text)
        {
            Text = text;
        }
    }

## Lecture d'un attribut
La méthode `GetCustomAttributes` renvoie un tableau d'attributs personnalisés appliqués au membre. Après avoir récupéré ce tableau, vous pouvez rechercher un ou plusieurs attributs spécifiques.

    var attribute = typeof(MyClass).GetCustomAttributes().OfType<MyCustomAttribute>().Single();

Ou parcourez-les

    foreach(var attribute in typeof(MyClass).GetCustomAttributes()) {
        Console.WriteLine(attribute.GetType());
    }

La méthode d'extension `GetCustomAttribute` de `System.Reflection.CustomAttributeExtensions` récupère un attribut personnalisé d'un type spécifié, il peut être appliqué à n'importe quel `MemberInfo`.

    var attribute = (MyCustomAttribute) typeof(MyClass).GetCustomAttribute(typeof(MyCustomAttribute));

`GetCustomAttribute` a également une signature générique pour spécifier le type d'attribut à rechercher.

    var attribute = typeof(MyClass).GetCustomAttribute<MyCustomAttribute>();

L'argument booléen `inherit` peut être passé à ces deux méthodes. Si cette valeur est définie sur "true", les ancêtres de l'élément doivent également être inspectés.

## Utiliser un attribut
    [StackDemo(Text = "Hello, World!")]
    public class MyClass
    {
        [StackDemo("Hello, World!")]
        static void MyMethod()
        {
        }
    }

## Attribut d'affichage du débogueur
L'ajout de l'attribut `DebuggerDisplay` changera la façon dont le débogueur affiche la classe lorsqu'elle est survolée.

Les expressions enveloppées dans `{}` seront évaluées par le débogueur. Il peut s'agir d'une simple propriété comme dans l'exemple suivant ou d'une logique plus complexe.

    
    [DebuggerDisplay("{StringProperty} - {IntProperty}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    }
    

[![Exemple d'affichage du débogueur][1]][1]

L'ajout de `,nq` avant le crochet fermant supprime les guillemets lors de la sortie d'une chaîne.

    [DebuggerDisplay("{StringProperty,nq} - {IntProperty}")]
Même si les expressions générales sont autorisées dans le `{}`, elles ne sont pas recommandées. L'attribut `DebuggerDisplay` sera écrit dans les métadonnées de l'assembly sous forme de chaîne. La validité des expressions dans `{}` n'est pas vérifiée. Ainsi, un attribut `DebuggerDisplay` contenant une logique plus complexe que c'est-à-dire une arithmétique simple peut fonctionner correctement en C #, mais la même expression évaluée dans VB.NET ne sera probablement pas syntaxiquement valide et produira une erreur lors du débogage.

Une façon de rendre `DebuggerDisplay` plus indépendant du langage consiste à écrire l'expression dans une méthode ou une propriété et à l'appeler à la place.

    [DebuggerDisplay("{DebuggerDisplay(),nq}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    
       private string DebuggerDisplay()
        {
            return $"{StringProperty} - {IntProperty}"";
        }
    }

On peut souhaiter que `DebuggerDisplay` affiche toutes ou seulement certaines des propriétés et lors du débogage et de l'inspection également le type de l'objet.
L'exemple ci-dessous entoure également la méthode d'assistance avec `#if DEBUG` car `DebuggerDisplay` est utilisé dans les environnements de débogage.

    [DebuggerDisplay("{DebuggerDisplay(),nq}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    
    #if DEBUG
       private string DebuggerDisplay()
        {
            return
                $"ObjectId:{this.ObjectId}, StringProperty:{this.StringProperty}, Type:{this.GetType()}";
        }
        #endif
    }

[1] : http://i.stack.imgur.com/6JjJs.png

## Attributs des informations sur l'appelant
Les attributs d'informations sur l'appelant peuvent être utilisés pour transmettre des informations sur l'invocateur à la méthode invoquée. La déclaration ressemble à ceci :
    
    using System.Runtime.CompilerServices;

    public void LogException(Exception ex,
                             [CallerMemberName]string callerMemberName = "",
                             [CallerLineNumber]int callerLineNumber = 0,
                             [CallerFilePath]string callerFilePath = "")
    {
        //perform logging
    }

Et l'invocation ressemble à ceci :

    public void Save(DBContext context)
    {
        try
        {
            context.SaveChanges();
        }
        catch (Exception ex)
        {
            LogException(ex);
        }
    }
    

Notez que seul le premier paramètre est passé explicitement à la méthode `LogException` tandis que les autres seront fournis au moment de la compilation avec les valeurs pertinentes.

Le paramètre `callerMemberName` recevra la valeur `"Save"` - le nom de la méthode appelante.

Le paramètre `callerLineNumber` recevra le numéro de la ligne sur laquelle l'appel de la méthode `LogException` est écrit.

Et le paramètre 'callerFilePath' recevra le chemin complet du fichier dans lequel la méthode `Save` est déclarée.


## Lecture d'un attribut depuis l'interface
Il n'y a pas de moyen simple d'obtenir des attributs d'une interface, puisque les classes n'héritent pas des attributs d'une interface. Chaque fois que vous implémentez une interface ou remplacez des membres dans une classe dérivée, vous devez re-déclarer les attributs.
Ainsi, dans l'exemple ci-dessous, la sortie serait "True" dans les trois cas.

    using System;
    using System.Linq;
    using System.Reflection;

    namespace InterfaceAttributesDemo {
        
        [AttributeUsage(AttributeTargets.Interface, Inherited = true)]
        class MyCustomAttribute : Attribute {
            public string Text { get; set; }
        }
        
        [MyCustomAttribute(Text = "Hello from interface attribute")]
        interface IMyClass {
            void MyMethod();
        }
        
        class MyClass : IMyClass {
            public void MyMethod() { }
        }
        
        public class Program {
            public static void Main(string[] args) {
                GetInterfaceAttributeDemo();
            }
            
            private static void GetInterfaceAttributeDemo() {
                var attribute1 = (MyCustomAttribute) typeof(MyClass).GetCustomAttribute(typeof(MyCustomAttribute), true);
                Console.WriteLine(attribute1 == null); // True
                
                var attribute2 = typeof(MyClass).GetCustomAttributes(true).OfType<MyCustomAttribute>().SingleOrDefault();
                Console.WriteLine(attribute2 == null); // True
                
                var attribute3 = typeof(MyClass).GetCustomAttribute<MyCustomAttribute>(true);
                Console.WriteLine(attribute3 == null); // True
            }
        }
    }

Une façon de récupérer les attributs d'interface consiste à les rechercher dans toutes les interfaces implémentées par une classe.

    var attribute = typeof(MyClass).GetInterfaces().SelectMany(x => x.GetCustomAttributes().OfType<MyCustomAttribute>()).SingleOrDefault();
    Console.WriteLine(attribute == null); // False
    Console.WriteLine(attribute.Text); // Hello from interface attribute


## Attribut obsolète
System.Obsolete est un attribut utilisé pour marquer un type ou un membre qui a une meilleure version et ne doit donc pas être utilisé.

    [Obsolete("This class is obsolete. Use SomeOtherClass instead.")]
    class SomeClass
    {
        //
    }

Dans le cas où la classe ci-dessus est utilisée, le compilateur donnera l'avertissement "Cette classe est obsolète. Utilisez SomeOtherClass à la place."



