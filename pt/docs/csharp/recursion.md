---
title: "Recursão"
slug: "recursao"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Observe que o uso de recursão pode ter um impacto severo em seu código, pois cada chamada de função recursiva será anexada à pilha. Se houver muitas chamadas, isso pode levar a uma exceção **StackOverflow**. A maioria das "funções recursivas naturais" podem ser escritas como uma construção de loop `for`, `while` ou `foreach` e, embora não pareçam tão **posh** ou **clever** serão mais eficientes.

Sempre pense duas vezes e use a recursão com cuidado - saiba por que você a usa:

- a recursão deve ser usada quando você sabe que o número de chamadas recursivas não é *excessivo*
- *excessivo* significa que depende da quantidade de memória disponível
- a recursão é usada porque é uma versão de código mais clara e limpa, é mais legível do que uma função iterativa ou baseada em loop. Muitas vezes, esse é o caso porque fornece um código mais limpo e compacto (também conhecido como menos linhas de código).
- mas fique atento, pode ser menos eficiente! Por exemplo, na recursão de Fibonacci, para calcular o *nº* número na sequência, o tempo de cálculo crescerá exponencialmente!

Se quiser mais teoria, leia:
- https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/recursion2.html
- https://en.wikipedia.org/wiki/Recursion#In_computer_science


## Recursão em inglês simples
A recursão pode ser definida como:

> Um método que chama a si mesmo até que uma condição específica seja atendida.

Um exemplo excelente e simples de recursão é um método que obterá o fatorial de um determinado número:

    public int Factorial(int number)
    {
        return number == 0 ? 1 : n * Factorial(number - 1);
    }

Neste método, podemos ver que o método receberá um argumento, `number`.

Passo a passo:

Dado o exemplo, executando `Factorial(4)`

1. O `número (4) == 1`?
2. Não? return `4 * Fatorial(número-1)` (3)
3. Como o método é chamado novamente, ele agora repete o primeiro passo usando `Factorial(3)` como o novo argumento.
4. Isso continua até que `Factorial(1)` seja executado e `number (1) == 1` retorne 1.
5. No geral, o cálculo "acumula" `4 * 3 * 2 * 1` e, finalmente, retorna 24.

A chave para entender a recursão é que o método chama uma *nova instância* de si mesmo. Após o retorno, a execução da instância de chamada continua.

## Sequência de Fibonacci
Você pode calcular um número na sequência de Fibonacci usando recursão.

Seguindo a teoria matemática de F(n) = F(n-2) + F(n-1), para qualquer i > 0,

    // Returns the i'th Fibonacci number
    public int fib(int i) {
        if(i <= 2) {
            // Base case of the recursive function.
            // i is either 1 or 2, whose associated Fibonacci sequence numbers are 1 and 1.
            return 1;
        }
        // Recursive case. Return the sum of the two previous Fibonacci numbers.
        // This works because the definition of the Fibonacci sequence specifies
        // that the sum of two adjacent elements equals the next element.
        return  fib(i - 2) + fib(i - 1);
        
    }

    fib(10); // Returns 55

## Descrever recursivamente uma estrutura de objeto


## Usando a recursão para obter a árvore de diretórios
Um dos usos da recursão é navegar por uma estrutura de dados hierárquica, como uma árvore de diretórios do sistema de arquivos, sem saber quantos níveis a árvore possui ou o número de objetos em cada nível. Neste exemplo, você verá como usar a recursão em uma árvore de diretórios para localizar todos os subdiretórios de um diretório especificado e imprimir a árvore inteira no console.

    internal class Program
    {
        internal const int RootLevel = 0;
        internal const char Tab = '\t';

        internal static void Main()
        {
            Console.WriteLine("Enter the path of the root directory:");
            var rootDirectorypath = Console.ReadLine();

            Console.WriteLine(
                $"Getting directory tree of '{rootDirectorypath}'");

            PrintDirectoryTree(rootDirectorypath);
            Console.WriteLine("Press 'Enter' to quit...");
            Console.ReadLine();
        }

        internal static void PrintDirectoryTree(string rootDirectoryPath)
        {
            try
            {
                if (!Directory.Exists(rootDirectoryPath))
                {
                    throw new DirectoryNotFoundException(
                        $"Directory '{rootDirectoryPath}' not found.");
                }

                var rootDirectory = new DirectoryInfo(rootDirectoryPath);
                PrintDirectoryTree(rootDirectory, RootLevel);
            }
            catch (DirectoryNotFoundException e)
            {
                Console.WriteLine(e.Message);
            }
        }

        private static void PrintDirectoryTree(
            DirectoryInfo directory, int currentLevel)
        {
            var indentation = string.Empty;
            for (var i = RootLevel; i < currentLevel; i++)
            {
                indentation += Tab;
            }

            Console.WriteLine($"{indentation}-{directory.Name}");
            var nextLevel = currentLevel + 1;
            try
            {
                foreach (var subDirectory in directory.GetDirectories())
                {
                    PrintDirectoryTree(subDirectory, nextLevel);
                }
            }
            catch (UnauthorizedAccessException e)
            {
                Console.WriteLine($"{indentation}-{e.Message}");
            }
        }
    }

Esse código é um pouco mais complicado do que o mínimo necessário para concluir essa tarefa, pois inclui verificação de exceção para lidar com quaisquer problemas ao obter os diretórios. Abaixo, você encontrará uma divisão do código em segmentos menores com explicações de cada um.

`Principal`:

O método main recebe uma entrada de um usuário como uma string, que deve ser usada como o caminho para o diretório raiz. Ele então chama o método `PrintDirectoryTree` com esta string como parâmetro.

`PrintDirectoryTree(string)`:

Este é o primeiro de dois métodos que lidam com a impressão real da árvore de diretórios. Esse método usa uma string representando o caminho para o diretório raiz como parâmetro. Ele verifica se o caminho é um diretório real e, se não, lança um `DirectoryNotFoundException` que é tratado no bloco catch. Se o caminho é um diretório real, um objeto `DirectoryInfo` `rootDirectory` é criado a partir do caminho, e o segundo método `PrintDirectoryTree` é chamado com o objeto `rootDirectory` e `RootLevel`, que é uma constante inteira com um valor de zero.

`PrintDirectoryTree(DirectoryInfo, int)`:

Este segundo método lida com o peso do trabalho. Leva um `DirectoryInfo` e um inteiro como parâmetros. O `DirectoryInfo` é o diretório atual, e o inteiro é a profundidade do diretório em relação à raiz. Para facilitar a leitura, a saída é recuada para cada nível de profundidade do diretório atual, para que a saída fique assim:

    -Root
        -Child 1
        -Child 2
            -Grandchild 2.1
        -Child 3

Depois que o diretório atual é impresso, seus subdiretórios são recuperados e esse método é chamado em cada um deles com um valor de nível de profundidade de um a mais que o atual. Essa parte é a recursão: o método chamando a si mesmo. O programa será executado dessa maneira até que tenha visitado todos os diretórios da árvore. Quando atingir um diretório sem subdiretórios, o método retornará automaticamente.

Este método também captura uma `UnauthorizedAccessException`, que é lançada se algum dos subdiretórios do diretório atual estiver protegido pelo sistema. A mensagem de erro é impressa no nível de recuo atual para consistência.

O método abaixo fornece uma abordagem mais básica para este problema:

    internal static void PrintDirectoryTree(string directoryName)
    {
        try
        {
            if (!Directory.Exists(directoryName)) return;
            Console.WriteLine(directoryName);
            foreach (var d in Directory.GetDirectories(directoryName))
            {
                PrintDirectoryTree(d);
            }
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }

Isso não inclui a verificação de erros específica ou a formatação de saída da primeira abordagem, mas efetivamente faz a mesma coisa. Como ele usa apenas strings em vez de `DirectoryInfo`, ele não pode fornecer acesso a outras propriedades de diretório, como permissões.

## Cálculo do PowerOf
O cálculo da potência de um determinado número também pode ser feito recursivamente.
Dado um número base `n` e um expoente `e`, precisamos ter certeza de dividir o problema em partes diminuindo o expoente `e`.

Exemplo Teórico:

- 2² = 2x2
- 2³ = 2x2x2
ou, 2³ = 2² x 2<br/>Aí está o segredo do nosso algoritmo recursivo (veja o código abaixo). Trata-se de pegar o problema e separá-lo em pedaços menores e mais simples de resolver.
- **Notas**
- quando o número base é 0, temos que estar atentos para retornar 0 como 0³ = 0 x 0 x 0
- quando o expoente for 0, temos que estar atentos para retornar sempre 1, pois esta é uma regra matemática.

Exemplo de código:

    public int CalcPowerOf(int b, int e) {
        if (b == 0) { return 0; } // when base is 0, it doesn't matter, it will always return 0
        if (e == 0) { return 1; } // math rule, exponent 0 always returns 1
        return b * CalcPowerOf(b, e - 1); // actual recursive logic, where we split the problem, aka: 2³ = 2 * 2² etc..
    }

Testes no xUnit para verificar a lógica:<br/>
Embora isso não seja necessário, é sempre bom escrever testes para verificar sua lógica. Eu incluo aqueles aqui escritos no [framework xUnit][1].

        [Theory]
        [MemberData(nameof(PowerOfTestData))]
        public void PowerOfTest(int @base, int exponent, int expected) {
            Assert.Equal(expected, CalcPowerOf(@base, exponent));
        }

        public static IEnumerable<object[]> PowerOfTestData() {
            yield return new object[] { 0, 0, 0 };
            yield return new object[] { 0, 1, 0 };
            yield return new object[] { 2, 0, 1 };
            yield return new object[] { 2, 1, 2 };
            yield return new object[] { 2, 2, 4 };
            yield return new object[] { 5, 2, 25 };
            yield return new object[] { 5, 3, 125 };
            yield return new object[] { 5, 4, 625 };
    }


[1]: https://xunit.github.io/

## Cálculo fatorial
O fatorial de um número (indicado com !, como por exemplo 9!) é a multiplicação desse número pelo fatorial de um menor. Assim, por exemplo, 9! = 9x8! = 9 x 8 x 7! = 9 x 8 x 7 x 6 x 5 x 4 x 3 x 2 x 1.

Então, no código que se torna, usando recursão:

    long Factorial(long x)
    {
        if (x < 1)
        {
            throw new OutOfRangeException("Factorial can only be used with positive numbers.");
        }
    
        if (x == 1)
        {
            return 1;
        } else {
            return x * Factorial(x - 1);
        }
    }



