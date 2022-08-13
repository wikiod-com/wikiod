---
title: "Reflexão"
slug: "reflexao"
draft: false
images: []
weight: 9898
type: docs
toc: true
---

## O que é uma Assembleia?
Os assemblies são o bloco de construção de qualquer aplicativo [Common Language Runtime (CLR)][CLR].
Cada tipo que você define, junto com seus métodos, propriedades e seu bytecode, é compilado e empacotado dentro de um Assembly.

    using System.Reflection;

<b></b>
    
    Assembly assembly = this.GetType().Assembly;   
    
Os assemblies são autodocumentados: eles não contêm apenas tipos, métodos e seu código IL, mas também os metadados necessários para inspecioná-los e consumi-los, tanto em compilação quanto em tempo de execução:

    Assembly assembly = Assembly.GetExecutingAssembly();

    foreach (var type in assembly.GetTypes())
    {
        Console.WriteLine(type.FullName);
    }
 
Os assemblies têm nomes que descrevem sua identidade completa e exclusiva:

    Console.WriteLine(typeof(int).Assembly.FullName);
    // Will print: "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"

Se esse nome incluir um `PublicKeyToken`, ele será chamado de *nome forte*. A nomeação forte de um assembly é o processo de criação de uma assinatura usando a chave privada que corresponde à chave pública distribuída com o assembly. Essa assinatura é adicionada ao manifesto do Assembly, que contém os nomes e hashes de todos os arquivos que compõem o assembly, e seu `PublicKeyToken` passa a fazer parte do nome. Os assemblies que têm o mesmo nome forte devem ser idênticos; nomes fortes são usados ​​no controle de versão e para evitar conflitos de assembly.

[CLR]: https://en.wikipedia.org/wiki/Common_Language_Runtime

## Comparar dois objetos com reflexão
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

**Observação:** este exemplo faz uma comparação baseada em campo (ignore campos e propriedades estáticos) para simplificar


## Como criar um objeto de T usando Reflexão
Usando o construtor padrão

    T variable = Activator.CreateInstance(typeof(T));


Usando construtor parametrizado

    T variable = Activator.CreateInstance(typeof(T), arg1, arg2);

## Criando objetos e definindo propriedades usando reflexão
Digamos que temos uma classe `Classy` que tem a propriedade Propertua

    public class Classy
    {
        public string Propertua {get; set;}
    }

para definir `Propertua` usando reflexão:

    var typeOfClassy = typeof (Classy);
    var classy = new Classy();
    var prop = typeOfClassy.GetProperty("Propertua");
    prop.SetValue(classy, "Value");

## Obtendo um atributo de um enum com reflexão (e armazenando-o em cache)
Os atributos podem ser úteis para denotar metadados em enums. Obter o valor disso pode ser lento, por isso é importante armazenar os resultados em cache.

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

