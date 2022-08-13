---
title: "Atributos"
slug: "atributos"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Criando um atributo personalizado
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

## Lendo um atributo
O método `GetCustomAttributes` retorna uma matriz de atributos personalizados aplicados ao membro. Após recuperar esse array, você pode pesquisar um ou mais atributos específicos.

    var attribute = typeof(MyClass).GetCustomAttributes().OfType<MyCustomAttribute>().Single();

Ou iterar por eles

    foreach(var attribute in typeof(MyClass).GetCustomAttributes()) {
        Console.WriteLine(attribute.GetType());
    }

O método de extensão `GetCustomAttribute` de `System.Reflection.CustomAttributeExtensions` recupera um atributo personalizado de um tipo especificado, pode ser aplicado a qualquer `MemberInfo`.

    var attribute = (MyCustomAttribute) typeof(MyClass).GetCustomAttribute(typeof(MyCustomAttribute));

`GetCustomAttribute` também possui assinatura genérica para especificar o tipo de atributo a ser pesquisado.

    var attribute = typeof(MyClass).GetCustomAttribute<MyCustomAttribute>();

O argumento booleano `inherit` pode ser passado para ambos os métodos. Se esse valor for definido como `true`, os ancestrais do elemento também serão inspecionados.

## Usando um atributo
    [StackDemo(Text = "Hello, World!")]
    public class MyClass
    {
        [StackDemo("Hello, World!")]
        static void MyMethod()
        {
        }
    }

## Atributo de exibição do depurador
Adicionar o atributo `DebuggerDisplay` mudará a maneira como o depurador exibe a classe quando passa o mouse sobre ela.

As expressões envolvidas em `{}` serão avaliadas pelo depurador. Isso pode ser uma propriedade simples como no exemplo a seguir ou uma lógica mais complexa.

    
    [DebuggerDisplay("{StringProperty} - {IntProperty}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    }
    

[![Exemplo de exibição do depurador][1]][1]

Adicionar `,nq` antes do colchete de fechamento remove as aspas ao gerar uma string.

    [DebuggerDisplay("{StringProperty,nq} - {IntProperty}")]
Embora expressões gerais sejam permitidas no `{}`, elas não são recomendadas. O atributo `DebuggerDisplay` será gravado nos metadados do assembly como uma string. As expressões em `{}` não são verificadas quanto à validade. Portanto, um atributo `DebuggerDisplay` contendo uma lógica mais complexa do que alguma aritmética simples pode funcionar bem em C#, mas a mesma expressão avaliada em VB.NET provavelmente não será sintaticamente válida e produzirá um erro durante a depuração.

Uma maneira de tornar o `DebuggerDisplay` mais independente de linguagem é escrever a expressão em um método ou propriedade e chamá-la.

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

Pode-se querer que o `DebuggerDisplay` produza todas ou apenas algumas das propriedades e ao depurar e inspecionar também o tipo do objeto.
O exemplo abaixo também envolve o método auxiliar com `#if DEBUG` já que `DebuggerDisplay` é usado em ambientes de depuração.

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

[1]: http://i.stack.imgur.com/6JjJs.png

## Atributos de informações do chamador
Os atributos de informações do chamador podem ser usados ​​para passar informações sobre o chamador para o método invocado. A declaração fica assim:
    
    using System.Runtime.CompilerServices;

    public void LogException(Exception ex,
                             [CallerMemberName]string callerMemberName = "",
                             [CallerLineNumber]int callerLineNumber = 0,
                             [CallerFilePath]string callerFilePath = "")
    {
        //perform logging
    }

E a invocação fica assim:

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
    

Observe que apenas o primeiro parâmetro é passado explicitamente para o método `LogException`, enquanto o restante deles será fornecido em tempo de compilação com os valores relevantes.

O parâmetro `callerMemberName` receberá o valor `"Save"` - o nome do método de chamada.

O parâmetro `callerLineNumber` receberá o número de qualquer linha em que a chamada do método `LogException` for escrita.

E o parâmetro 'callerFilePath' receberá o caminho completo do arquivo em que o método 'Save' está declarado.


## Lendo um atributo da interface
Não existe uma maneira simples de obter atributos de uma interface, pois as classes não herdam atributos de uma interface. Sempre que implementar uma interface ou substituir membros em uma classe derivada, você precisará declarar novamente os atributos.
Portanto, no exemplo abaixo, a saída seria `True` em todos os três casos.

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

Uma maneira de recuperar atributos de interface é procurá-los em todas as interfaces implementadas por uma classe.

    var attribute = typeof(MyClass).GetInterfaces().SelectMany(x => x.GetCustomAttributes().OfType<MyCustomAttribute>()).SingleOrDefault();
    Console.WriteLine(attribute == null); // False
    Console.WriteLine(attribute.Text); // Hello from interface attribute


## Atributo Obsoleto
System.Obsolete é um atributo que é usado para marcar um tipo ou um membro que possui uma versão melhor e, portanto, não deve ser usado.

    [Obsolete("This class is obsolete. Use SomeOtherClass instead.")]
    class SomeClass
    {
        //
    }

Caso a classe acima seja usada, o compilador dará o aviso "Esta classe é obsoleta. Use SomeOtherClass em vez disso."



