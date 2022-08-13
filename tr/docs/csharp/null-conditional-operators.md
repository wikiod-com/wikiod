---
title: "Null-Koşullu Operatörler"
slug: "null-kosullu-operatorler"
draft: false
images: []
weight: 9614
type: docs
toc: true
---

## Sözdizimi
- X?.Y; // X null ise null, aksi takdirde X.Y
- X?.Y?.Z; // X null veya Y null ise null, aksi takdirde X.Y.Z
- X?[indeks]; // X null ise null, aksi takdirde X[index]
- X?.ValueMethod(); // X null ise null, aksi takdirde X.ValueMethod();
- X?.VoidMethod(); // X boş ise hiçbir şey yapmayın, aksi takdirde X.VoidMethod()'u çağırın;


`T` değer tipinde boş birleştirme operatörünü kullanırken bir `Nullable<T>` geri alacağınızı unutmayın.

## Null-Koşullu Operatör
`?.` operatörü, ayrıntılı boş kontrollerden kaçınmak için sözdizimsel şekerdir. [Güvenli Navigasyon Operatörü] olarak da bilinir (https://en.wikipedia.org/wiki/Safe_Navigation_Operator).

------

Aşağıdaki örnekte kullanılan sınıf:

    public class Person
    {
        public int Age { get; set; }
        public string Name { get; set; }
        public Person Spouse { get; set; }
    }

Bir nesne potansiyel olarak boşsa (bir referans türü döndüren bir işlev gibi), olası bir 'NullReferenceException'ı önlemek için nesnenin önce boş olup olmadığı kontrol edilmelidir. Boş koşullu operatör olmadan, bu şöyle görünür:

    Person person = GetPerson();

    int? age = null;
    if (person != null)
        age = person.Age;

Boş koşullu operatörü kullanan aynı örnek:

    Person person = GetPerson();

    var age = person?.Age;    // 'age' will be of type 'int?', even if 'person' is not null


----------


Operatörü Zincirleme
---------------------
Boş koşullu işleç, bir nesnenin üyeleri ve alt üyeleri üzerinde birleştirilebilir.

    // Will be null if either `person` or `person.Spouse` are null
    int? spouseAge = person?.Spouse?.Age;


----------


Null-Coalescing Operatörü ile birleştirme
---------------------
Boş koşullu operatör, varsayılan bir değer sağlamak için [boş birleştirme operatörü][1] ile birleştirilebilir:

    // spouseDisplayName will be "N/A" if person, Spouse, or Name is null
    var spouseDisplayName = person?.Spouse?.Name ?? "N/A";



[1]: https://www.wikiod.com/tr/docs/c%23/37/null-coalescing-operator#t=201610192135170167414

## Null-Koşullu Dizin
`?.` operatörüne benzer şekilde, boş koşullu indeks operatörü, null olabilecek bir koleksiyona indeksleme yaparken boş değerleri kontrol eder.

    string item = collection?[index];

için sözdizimsel şekerdir

    string item = null;
    if(collection != null)
    {
        item = collection[index];
    }


## NullReferenceİstisnalarından Kaçınma
    var person = new Person
    {
        Address = null;
    };
    
    var city = person.Address.City; //throws a NullReferenceException
    var nullableCity = person.Address?.City; //returns the value of null

Bu etki birlikte zincirlenebilir:

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

## Null koşullu Operatör, Uzantı Yöntemi ile kullanılabilir
[Uzantı Yöntemi boş referanslar üzerinde çalışabilir](https://www.wikiod.com/tr/docs/c%23/20/extension-methods/161/null-checking#t=201607271333507907787), ancak bunu yapmak için `?.` kullanabilirsiniz. yine de boş kontrol edin.

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

Normalde, yöntem "null" referanslar için tetiklenir ve -1 döndürür:

    Person person = null;
    int nameLength = person.GetNameLength(); // returns -1

'?.' kullanıldığında, yöntem 'boş' referanslar için tetiklenmez ve [type is 'int?'](https://www.wikiod.com/tr/docs/c%23/41/null-conditional-operators/ 173/null-conditional-operator#t=201607271345436018565):

    Person person = null;
    int? nameLength = person?.GetNameLength(); // nameLength is null.

Bu davranış aslında '?.' operatörünün çalışma biçiminden beklenir: 'NullReferenceExceptions'tan kaçınmak için boş örnekler için örnek yöntemi çağrıları yapmaktan kaçınacaktır. Ancak, yöntemin nasıl bildirildiğine ilişkin farklılığa rağmen, aynı mantık uzatma yöntemi için de geçerlidir.

İlk örnekte uzantı yönteminin neden çağrıldığı hakkında daha fazla bilgi için lütfen [Uzantı yöntemleri - boş denetim](https://www.wikiod.com/tr/docs/c%23/20/extension-methods/161/null- bölümüne bakın) kontrol#t=201607271333507907787) belgeleri.

