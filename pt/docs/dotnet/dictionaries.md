---
title: "Dicionários"
slug: "dicionarios"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

## Inicializando um Dicionário com um Inicializador de Coleção
    // Translates to `dict.Add(1, "First")` etc.
    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

    // Translates to `dict[1] = "First"` etc.
    // Works in C# 6.0.
    var dict = new Dictionary<int, string>()
    {
        [1] = "First",
        [2] = "Second",
        [3] = "Third"
    };


## Adicionando a um dicionário
    Dictionary<int, string> dict = new Dictionary<int, string>();
    dict.Add(1, "First");
    dict.Add(2, "Second");
   
    // To safely add items (check to ensure item does not already exist - would throw)
    if(!dict.ContainsKey(3))
    {
       dict.Add(3, "Third");
    }

Alternativamente, eles podem ser adicionados/definidos por meio de um indexador. (Um indexador internamente se parece com uma propriedade, tendo um get e set, mas recebe um parâmetro de qualquer tipo especificado entre colchetes):

    Dictionary<int, string> dict = new Dictionary<int, string>();
    dict[1] = "First";
    dict[2] = "Second";
    dict[3] = "Third";

Ao contrário do método `Add` que lança uma exceção, se uma chave já estiver contida no dicionário, o indexador apenas substitui o valor existente.

Para dicionário thread-safe, use `ConcurrentDictionary<TKey, TValue>`:

    var dict = new ConcurrentDictionary<int, string>();
    dict.AddOrUpdate(1, "First", (oldKey, oldValue) => "First");



## Obtendo um valor de um dicionário
Dado este código de configuração:

    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

Você pode querer ler o valor para a entrada com a chave 1. Se a chave não existir, obter um valor lançará `KeyNotFoundException`, então você pode querer primeiro verificar isso com `ContainsKey`:

    if (dict.ContainsKey(1))
        Console.WriteLine(dict[1]);

Isso tem uma desvantagem: você pesquisará em seu dicionário duas vezes (uma para verificar a existência e outra para ler o valor). Para um dicionário grande, isso pode afetar o desempenho. Felizmente, ambas as operações podem ser realizadas juntas:

    string value;
    if (dict.TryGetValue(1, out value))
        Console.WriteLine(value);


## Faça um dicionário<string, T> com chaves que não diferenciam maiúsculas de minúsculas.
    var MyDict = new Dictionary<string,T>(StringComparison.InvariantCultureIgnoreCase)

## Enumerando um dicionário
Você pode enumerar por meio de um Dicionário de 3 maneiras:

**Usando pares de KeyValue**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(KeyValuePair<int, string> kvp in dict) 
    {
       Console.WriteLine("Key : " + kvp.Key.ToString() + ", Value : " + kvp.Value);
    }

**Usando Chaves**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(int key in dict.Keys)
    {
        Console.WriteLine("Key : " + key.ToString() + ", Value : " + dict[key]);
    }

**Usando valores**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(string s in dict.Values)
    {
        Console.WriteLine("Value : " + s);
    }

## ConcurrentDictionary<TKey, TValue> (do .NET 4.0)
> Representa uma coleção thread-safe de pares chave/valor que podem ser
> acessado por vários threads simultaneamente.

Criando uma instância
--------------------

Criar uma instância funciona praticamente da mesma forma que com ```Dictionary<TKey, TValue>```, por exemplo:

    var dict = new ConcurrentDictionary<int, string>();

Adicionando ou Atualizando
------------------

Você pode se surpreender, que não existe um método `Add`, mas sim `AddOrUpdate` com 2 sobrecargas:

(1) `AddOrUpdate(TKey key, TValue, Func<TKey, TValue, TValue> addValue)` - *Adiciona um par de chave/valor se a chave ainda não existir ou atualiza um par de chave/valor usando a função especificada se a chave já existir.*

(2) `AddOrUpdate(TKey key, Func<TKey, TValue> addValue, Func<TKey, TValue, TValue> updateValueFactory)` - *Usa as funções especificadas para adicionar um par chave/valor ao se a chave ainda não existir , ou para atualizar um par chave/valor se a chave já existir.*

Adicionando ou atualizando um valor, não importa qual fosse o valor se já estivesse presente para determinada chave (1):

    string addedValue = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => "First");

Adicionando ou atualizando um valor, mas agora alterando o valor em atualização, com base no valor anterior (1):

    string addedValue2 = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => $"{valueOld} Updated");

Usando a sobrecarga (2) também podemos adicionar um novo valor usando uma fábrica:

    string addedValue3 = dict.AddOrUpdate(1, (key) => key == 1 ? "First" : "Not First", (updateKey, valueOld) => $"{valueOld} Updated");

Obtendo valor
-----------------
Obter um valor é o mesmo que com o `Dictionary<TKey,TValue>`:

    string value = null;
    bool success = dict.TryGetValue(1, out value);

Obtendo ou Adicionando um Valor
-------------------------
Existem duas sobrecargas de métodos, que **obtêm ou adicionam** um valor de maneira thread-safe.

Obtenha o valor com a chave 2 ou adicione o valor "Second" se a chave não estiver presente:

    string theValue = dict.GetOrAdd(2, "Second");

Usando uma fábrica para adicionar um valor, se o valor não estiver presente:

    string theValue2 = dict.GetOrAdd(2, (key) => key == 2 ? "Second" : "Not Second." );




## IEnumerable to Dictionary (≥ .NET 3.5)
Crie um [Dicionário&lt;TKey, TValue&gt;][1] a partir de um [IEnumerable&lt;T&gt;][2]:

    using System;
    using System.Collections.Generic;
    using System.Linq;

<b></b>

    public class Fruits
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }

<b></b>

    var fruits = new[]
    { 
        new Fruits { Id = 8 , Name = "Apple" },
        new Fruits { Id = 3 , Name = "Banana" },
        new Fruits { Id = 7 , Name = "Mango" },
    };

    
    // Dictionary<int, string>                  key      value
    var dictionary = fruits.ToDictionary(x => x.Id, x => x.Name);

[1]: https://msdn.microsoft.com/en-us/library/xfhwa508(v=vs.100).aspx
[2]: https://msdn.microsoft.com/en-us/library/9eekhta0(v=vs.100).aspx



## Removendo de um Dicionário
Dado este código de configuração:

    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

Use o método `Remove` para remover uma chave e seu valor associado.

    bool wasRemoved = dict.Remove(2);

A execução deste código remove a chave `2` e seu valor do dicionário. `Remove` retorna um valor booleano indicando se a chave especificada foi encontrada e removida do dicionário. Se a chave não existir no dicionário, nada será removido do dicionário e false será retornado (nenhuma exceção será lançada).

É **incorreto** tentar remover uma chave definindo o valor da chave como `null`.

    dict[2] = null; // WRONG WAY TO REMOVE!

Isso não removerá a chave. Ele apenas substituirá o valor anterior por um valor de `null`.

Para remover todas as chaves e valores de um dicionário, use o método `Clear`.

    dict.Clear();

Após executar `Clear` o `Count` do dicionário será 0, mas a capacidade interna permanece inalterada.

## ContémChave(TKey)
Para verificar se um `Dictionary` tem uma chave específica, você pode chamar o método [`ContainsKey(TKey)`][1] e fornecer a chave do tipo `TKey`. O método retorna um valor `bool` quando a chave existe no dicionário. Para amostra:

    var dictionary = new Dictionary<string, Customer>()
    {
       {"F1", new Customer() { FirstName = "Felipe", ... } },
       {"C2", new Customer() { FirstName = "Carl", ... } },
       {"J7", new Customer() { FirstName = "John", ... } },
       {"M5", new Customer() { FirstName = "Mary", ... } },
    };

E verifique se existe um `C2` no Dicionário:

    if (dictionary.ContainsKey("C2")) 
    {
       // exists
    }

O método ContainsKey está disponível na versão genérica [`Dictionary<TKey, TValue>`][1].


[1]: https://msdn.microsoft.com/library/htszx2dy(v=vs.110).aspx

## Dicionário para listar
Criando uma lista de KeyValuePair:

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<KeyValuePair<int, int>> list = new List<KeyValuePair<int, int>>();
    list.AddRange(dictionary);

Criando uma lista de chaves:

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<int> list = new List<int>();
    list.AddRange(dictionary.Keys);

Criando uma lista de valores:

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<int> list = new List<int>();
    list.AddRange(dictionary.Values);


## ConcurrentDictionary aumentado com Lazy'1 reduz a computação duplicada
## Problema

ConcurrentDictionary brilha quando se trata de retornar instantaneamente de chaves existentes do cache, principalmente sem bloqueio e disputando em um nível granular.
Mas e se a criação do objeto for muito cara, superando o custo da troca de contexto, e ocorrerem algumas falhas de cache?

Se a mesma chave for solicitada de várias threads, um dos objetos resultantes das operações de colisão será eventualmente adicionado à coleção, e os demais serão jogados fora, desperdiçando o recurso de CPU para criar o objeto e recurso de memória para armazenar o objeto temporariamente . Outros recursos também podem ser desperdiçados. Isso é muito ruim.

## Solução

Podemos combinar `ConcurrentDictionary<TKey, TValue>` com `Lazy<TValue>`. A ideia é que o método ConcurrentDictionary GetOrAdd possa retornar apenas o valor que foi realmente adicionado à coleção. Os objetos Lazy perdidos também podem ser desperdiçados neste caso, mas isso não é muito problema, pois o próprio objeto Lazy é relativamente barato. A propriedade Value do Lazy perdedor nunca é solicitada, porque somos espertos em solicitar apenas a propriedade Value daquela realmente adicionada à coleção - aquela retornada do método GetOrAdd:

    public static class ConcurrentDictionaryExtensions
    {
        public static TValue GetOrCreateLazy<TKey, TValue>(
            this ConcurrentDictionary<TKey, Lazy<TValue>> d,
            TKey key,
            Func<TKey, TValue> factory)
        {
            return
                d.GetOrAdd(
                    key,
                    key1 =>
                        new Lazy<TValue>(() => factory(key1),
                        LazyThreadSafetyMode.ExecutionAndPublication)).Value;
        }
    }

O armazenamento em cache de objetos XmlSerializer pode ser particularmente caro e também há muita contenção na inicialização do aplicativo. E há mais: se forem serializadores personalizados, também haverá um vazamento de memória pelo resto do ciclo de vida do processo. O único benefício do ConcurrentDictionary nesse caso é que para o resto do ciclo de vida do processo não haverá bloqueios, mas a inicialização do aplicativo e o uso da memória seriam inaceitáveis. Este é um trabalho para nosso ConcurrentDictionary, aumentado com Lazy:

    private ConcurrentDictionary<Type, Lazy<XmlSerializer>> _serializers =
        new ConcurrentDictionary<Type, Lazy<XmlSerializer>>();
    
    public XmlSerializer GetSerialier(Type t)
    {
        return _serializers.GetOrCreateLazy(t, BuildSerializer);
    }
    
    private XmlSerializer BuildSerializer(Type t)
    {
        throw new NotImplementedException("and this is a homework");
    }

