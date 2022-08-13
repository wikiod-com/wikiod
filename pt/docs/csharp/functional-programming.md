---
title: "Programação Funcional"
slug: "programacao-funcional"
draft: false
images: []
weight: 9866
type: docs
toc: true
---

## Função e ação
**Func** fornece um suporte para funções anônimas parametrizadas. Os tipos principais são as entradas e o último tipo é sempre o valor de retorno.

    // square a number.
    Func<double, double> square = (x) => { return x * x; };

    // get the square root.
    // note how the signature matches the built in method.
    Func<double, double> squareroot = Math.Sqrt;

    // provide your workings.
    Func<double, double, string> workings = (x, y) => 
        string.Format("The square of {0} is {1}.", x, square(y))
        
Objetos **Action** são como métodos void, portanto, eles têm apenas um tipo de entrada. Nenhum resultado é colocado na pilha de avaliação.

    // right-angled triangle.
    class Triangle
    {
        public double a;
        public double b;
        public double h;
    }

    // Pythagorean theorem.
    Action<Triangle> pythagoras = (x) => 
        x.h = squareroot(square(x.a) + square(x.b));
    
    Triangle t = new Triangle { a = 3, b = 4 };
    pythagoras(t);
    Console.WriteLine(t.h); // 5.



## Evite referências nulas
Os desenvolvedores de C# recebem muitas exceções de referência nula para lidar. Os desenvolvedores de F# não porque eles têm o tipo Option. Um tipo Option<> (alguns preferem Maybe<> como nome) fornece um tipo de retorno Some e None. Torna explícito que um método pode estar prestes a retornar um registro nulo.

Por exemplo, você não pode ler o seguinte e saber se terá que lidar com um valor nulo.

    var user = _repository.GetUser(id);

Se você souber sobre o possível nulo, poderá introduzir algum código clichê para lidar com isso.

    var username = user != null ? user.Name : string.Empty;

E se tivermos uma Option<> retornada?

    Option<User> maybeUser = _repository.GetUser(id);

O código agora torna explícito que podemos ter um registro None retornado e o código padrão para verificar Some ou None é necessário:

    var username = maybeUser.HasValue ? maybeUser.Value.Name : string.Empty;

O método a seguir mostra como retornar uma Option<>

    public Option<User> GetUser(int id)
    {
        var users = new List<User>
        {
            new User { Id = 1, Name = "Joe Bloggs" },
            new User { Id = 2, Name = "John Smith" }
        };
    
        var user = users.FirstOrDefault(user => user.Id == id);
    
        return user != null ? new Option<User>(user) : new Option<User>();
    }

Aqui está uma implementação mínima de Option<>.

    public struct Option<T>
    {
        private readonly T _value;
    
        public T Value
        {
            get
            {
                if (!HasValue)
                    throw new InvalidOperationException();

                return _value;
            }
        }

        public bool HasValue
        {
            get { return _value != null; }
        }
    
        public Option(T value)
        {
            _value = value;
        }
    
        public static implicit operator Option<T>(T value)
        {
            return new Option<T>(value);
        }
    }

Para demonstrar o acima [avoidNull.csx][1] pode ser executado com o C# REPL.

Como afirmado, esta é uma implementação mínima. Uma pesquisa por ["Talvez" pacotes NuGet][2] resultará em várias bibliotecas boas.


[1]: https://gist.github.com/Boggin/d53660f32aeaa35e0b028919ddc465e3
[2]: https://www.nuget.org/packages?q=talvez

## Funções de ordem superior
Uma função de ordem superior é aquela que recebe outra função como argumento ou retorna uma função (ou ambas).

Isso geralmente é feito com lambdas, por exemplo, ao passar um predicado para uma cláusula LINQ Where:

    var results = data.Where(p => p.Items == 0);

A cláusula Where() pode receber muitos predicados diferentes, o que lhe dá uma flexibilidade considerável.

A passagem de um método para outro método também é vista ao implementar o padrão de design Strategy. Por exemplo, vários métodos de classificação podem ser escolhidos e passados ​​para um método Sort em um objeto, dependendo dos requisitos em tempo de execução.

## Imutabilidade
A imutabilidade é comum na programação funcional e rara na programação orientada a objetos.

Crie, por exemplo, um tipo de endereço com estado mutável:

    public class Address () 
    {
        public string Line1 { get; set; }
        public string Line2 { get; set; }
        public string City  { get; set; }
    }

Qualquer pedaço de código pode alterar qualquer propriedade no objeto acima.

Agora crie o tipo de endereço imutável:

    public class Address () 
    {
        public readonly string Line1;
        public readonly string Line2;
        public readonly string City;

        public Address(string line1, string line2, string city) 
        {
            Line1 = line1;
            Line2 = line2;
            City  = city;
        }
    }

Tenha em mente que ter coleções somente leitura não respeita a imutabilidade. Por exemplo,

    public class Classroom
    {
        public readonly List<Student> Students;
        
        public Classroom(List<Student> students)
        {
            Students = students;
        }
    }

não é imutável, pois o usuário do objeto pode alterar a coleção (adicionar ou remover elementos dela). Para torná-lo imutável, deve-se usar uma interface como IEnumerable<Student>, que não expõe métodos para adicionar, ou torná-lo um ReadOnlyCollection<Student>.

    public class Classroom
    {
        public readonly ReadOnlyCollection<Student> Students;

        public Classroom(ReadOnlyCollection<Student> students)
        {
            Students = students;
        }
    }

    List<Students> list = new List<Student>();
    // add students
    Classroom c = new Classroom(list.AsReadOnly());   


Com o objeto imutável temos os seguintes benefícios:

- Estará em um estado conhecido (outro código não pode alterá-lo).
- É thread-safe.
- O construtor oferece um único local para validação.
- Saber que o objeto não pode ser alterado torna o código mais fácil de entender.

## Coleções imutáveis
O pacote NuGet [`System.Collections.Immutable`][1] fornece classes de coleção imutáveis.

# Criando e adicionando itens

    var stack = ImmutableStack.Create<int>();
    var stack2 = stack.Push(1); // stack is still empty, stack2 contains 1
    var stack3 = stack.Push(2); // stack2 still contains only one, stack3 has 2, 1

# Criando usando o construtor

Certas coleções imutáveis ​​têm uma classe interna `Builder` que pode ser usada para construir instâncias imutáveis ​​grandes de forma barata:

    var builder = ImmutableList.CreateBuilder<int>(); // returns ImmutableList.Builder
    builder.Add(1);
    builder.Add(2);
    var list = builder.ToImmutable();

# Criando a partir de um IEnumerable existente

    var numbers = Enumerable.Range(1, 5);
    var list = ImmutableList.CreateRange<int>(numbers);

Lista de todos os tipos de coleção imutáveis:

- [`System.Collections.Immutable.ImmutableArray<T>`][2]
- [`System.Collections.Immutable.ImmutableDictionary<TKey,TValue>`][3]
- [`System.Collections.Immutable.ImmutableHashSet<T>`][4]
- [`System.Collections.Immutable.ImmutableList<T>`][5]
- [`System.Collections.Immutable.ImmutableQueue<T>`][6]
- [`System.Collections.Immutable.ImmutableSortedDictionary<TKey,TValue>`][7]
- [`System.Collections.Immutable.ImmutableSortedSet<T>`][8]
- [`System.Collections.Immutable.ImmutableStack<T>`][9]


[1]: https://www.nuget.org/packages/System.Collections.Immutable/
[2]: https://msdn.microsoft.com/en-us/library/dn638264(v=vs.111).aspx
[3]: https://msdn.microsoft.com/en-us/library/dn467181(v=vs.111).aspx
[4]: https://msdn.microsoft.com/en-us/library/dn467171(v=vs.111).aspx
[5]: https://msdn.microsoft.com/en-us/library/dn456077.aspx
[6]: https://msdn.microsoft.com/en-us/library/dn467186(v=vs.111).aspx
[7]: https://msdn.microsoft.com/en-us/library/dn467194(v=vs.111).aspx
[8]: https://msdn.microsoft.com/en-us/library/dn467193(v=vs.111).aspx
[9]: https://msdn.microsoft.com/en-us/library/dn467197(v=vs.111).aspx

