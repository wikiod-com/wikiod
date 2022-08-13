---
title: "Operadores condicionais nulos"
slug: "operadores-condicionais-nulos"
draft: false
images: []
weight: 9614
type: docs
toc: true
---

## Sintaxe
- X?.Y; //nulo se X for nulo senão X.Y
- X?.Y?.Z; //null se X for null ou Y for null senão X.Y.Z
- X?[índice]; //nulo se X for nulo senão X[índice]
- X?.ValueMethod(); //nulo se X for nulo, senão o resultado de X.ValueMethod();
- X?.VoidMethod(); //não faça nada se X for nulo mais chame X.VoidMethod();


Observe que, ao usar o operador de coalescência nulo em um tipo de valor `T`, você receberá um `Nullable<T>` de volta.

## Operador Nulo-Condicional
O operador `?.` é um açúcar sintático para evitar verificações de nulo detalhadas. Também é conhecido como [Operador de Navegação Segura] (https://en.wikipedia.org/wiki/Safe_Navigation_Operator).

------

Classe usada no exemplo a seguir:

    public class Person
    {
        public int Age { get; set; }
        public string Name { get; set; }
        public Person Spouse { get; set; }
    }

Se um objeto for potencialmente nulo (como uma função que retorna um tipo de referência), o objeto deve primeiro ser verificado quanto a nulo para evitar uma possível `NullReferenceException`. Sem o operador condicional nulo, isso ficaria assim:

    Person person = GetPerson();

    int? age = null;
    if (person != null)
        age = person.Age;

O mesmo exemplo usando o operador condicional nulo:

    Person person = GetPerson();

    var age = person?.Age;    // 'age' will be of type 'int?', even if 'person' is not null


----------


Encadeando o Operador
----------
O operador condicional nulo pode ser combinado nos membros e submembros de um objeto.

    // Will be null if either `person` or `person.Spouse` are null
    int? spouseAge = person?.Spouse?.Age;


----------


Combinando com o operador de coalescência nula
----------
O operador condicional nulo pode ser combinado com o [operador de coalescência nulo][1] para fornecer um valor padrão:

    // spouseDisplayName will be "N/A" if person, Spouse, or Name is null
    var spouseDisplayName = person?.Spouse?.Name ?? "N/A";



[1]: https://www.wikiod.com/pt/docs/c%23/37/null-coalescing-operator#t=201610192135170167414

## O Índice Nulo-Condicional
Da mesma forma que o operador `?.`, o operador de índice condicional nulo verifica valores nulos ao indexar em uma coleção que pode ser nula.

    string item = collection?[index];

é açúcar sintático para

    string item = null;
    if(collection != null)
    {
        item = collection[index];
    }


## Evitando NullReferenceExceptions
    var person = new Person
    {
        Address = null;
    };
    
    var city = person.Address.City; //throws a NullReferenceException
    var nullableCity = person.Address?.City; //returns the value of null

Este efeito pode ser encadeado:

    var person = new Person
    {
        Address = new Address
        {
            State = new State
            {
                Country = null
            }
        }
    };
    
    // this will always return a value of at least "null" to be stored instead
    // of throwing a NullReferenceException
    var countryName = person?.Address?.State?.Country?.Name; 

## Operador condicional nulo pode ser usado com o método de extensão
[O método de extensão pode funcionar em referências nulas](https://www.wikiod.com/pt/docs/c%23/20/extension-methods/161/null-checking#t=201607271333507907787), mas você pode usar `?.` para verificação nula de qualquer maneira.

    public class Person 
    {
        public string Name {get; set;}
    }
    
    public static class PersonExtensions
    {
        public static int GetNameLength(this Person person)
        {
            return person == null ? -1 : person.Name.Length;
        }
    }

Normalmente, o método será acionado para referências `null` e retornará -1:

    Person person = null;
    int nameLength = person.GetNameLength(); // returns -1

Usando `?.` o método não será acionado para referências `null`, e o [tipo é `int?`](https://www.wikiod.com/pt/docs/c%23/41/null-conditional-operators/ 173/null-conditional-operator#t=201607271345436018565):

    Person person = null;
    int? nameLength = person?.GetNameLength(); // nameLength is null.

Este comportamento é esperado pela forma como o operador `?.` funciona: ele evitará fazer chamadas de métodos de instância para instâncias nulas, a fim de evitar `NullReferenceExceptions`. No entanto, a mesma lógica se aplica ao método de extensão, apesar da diferença em como o método é declarado.

Para obter mais informações sobre por que o método de extensão é chamado no primeiro exemplo, consulte os [métodos de extensão - verificação nula](https://www.wikiod.com/pt/docs/c%23/20/extension-methods/161/null- checagem#t=201607271333507907787) documentação.

