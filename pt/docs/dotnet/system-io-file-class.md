---
title: "Classe System.IO.File"
slug: "classe-systemiofile"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Sintaxe
- fonte de strings;
- destino da string;

## Parâmetros
| Parâmetro | Detalhes |
| ---------------- | ------- |
| `fonte` | O arquivo que deve ser movido para outro local. |
| `destino` | O diretório para o qual você gostaria de mover `source` (essa variável também deve conter o nome (e a extensão do arquivo) do arquivo. |

## Excluir um arquivo
Para excluir um arquivo (se você tiver as permissões necessárias) é tão simples quanto:

    File.Delete(path);

No entanto, muitas coisas podem dar errado:

* Você não tem permissões necessárias (`UnauthorizedAccessException` é lançada).
* O arquivo pode estar em uso por outra pessoa (`IOException` é lançada).
* O arquivo não pode ser excluído devido a um erro de baixo nível ou a mídia é somente leitura (`IOException` é lançada).
* O arquivo não existe mais (`IOException` é lançado).

Observe que o último ponto (arquivo não existe) geralmente é _contornado_ com um trecho de código como este:

    if (File.Exists(path))
        File.Delete(path);

No entanto, não é uma operação atômica e o arquivo pode ser excluído por outra pessoa entre a chamada para `File.Exists()` e antes de `File.Delete()`. A abordagem correta para lidar com a operação de E/S requer tratamento de exceção (assumindo que um curso alternativo de ações possa ser tomado quando a operação falhar):

    if (File.Exists(path))
    {
        try
        {
            File.Delete(path);
        }
        catch (IOException exception)
        {
            if (!File.Exists(path))
                return; // Someone else deleted this file
    
            // Something went wrong...
        }
        catch (UnauthorizedAccessException exception)
        {
            // I do not have required permissions
        }
    }

Observe que esses erros de E/S às vezes são transitórios (arquivo em uso, por exemplo) e, se uma conexão de rede estiver envolvida, ela poderá se recuperar automaticamente sem nenhuma ação da nossa parte. É então comum _repetir_ uma operação de E/S algumas vezes com um pequeno atraso entre cada tentativa:

    public static void Delete(string path)
    {
        if (!File.Exists(path))
            return;
    
        for (int i=1; ; ++i)
        {
            try
            {
                File.Delete(path);
                return;
            }
            catch (IOException e)
            {
                if (!File.Exists(path))
                    return;
    
                if (i == NumberOfAttempts)
                    throw;
    
                Thread.Sleep(DelayBetweenEachAttempt);
            }
    
            // You may handle UnauthorizedAccessException but this issue
            // will probably won't be fixed in few seconds...
        }
    }
    
    private const int NumberOfAttempts = 3;
    private const int DelayBetweenEachAttempt = 1000; // ms

Nota: no ambiente Windows, o arquivo não será realmente excluído quando você chamar esta função, se outra pessoa abrir o arquivo usando `FileShare.Delete`, o arquivo poderá ser excluído, mas isso acontecerá somente quando o proprietário fechar o arquivo.

## Tira linhas indesejadas de um arquivo de texto
Alterar um arquivo de texto não é fácil porque seu conteúdo deve ser movido. Para arquivos _pequenos_, o método mais fácil é ler seu conteúdo na memória e, em seguida, escrever de volta o texto modificado.

Neste exemplo, lemos todas as linhas de um arquivo e descartamos todas as linhas em branco e, em seguida, escrevemos de volta no caminho original:

    File.WriteAllLines(path,
        File.ReadAllLines(path).Where(x => !String.IsNullOrWhiteSpace(x)));

Se o arquivo for muito grande para carregá-lo na memória e o caminho de saída for diferente do caminho de entrada:

    File.WriteAllLines(outputPath,
        File.ReadLines(inputPath).Where(x => !String.IsNullOrWhiteSpace(x)));
    

## Converter codificação de arquivo de texto
O texto é salvo codificado (consulte também o tópico [Strings][1]), então às vezes você pode precisar alterar sua codificação, este exemplo assume (para simplificar) que o arquivo não é muito grande e pode ser totalmente lido na memória:

    public static void ConvertEncoding(string path, Encoding from, Encoding to)
    {
        File.WriteAllText(path, File.ReadAllText(path, from), to);
    }

Ao realizar conversões não esqueça que o arquivo pode conter BOM (Byte Order Mark), para entender melhor como ele é gerenciado consulte [Encoding.UTF8.GetString não leva em consideração o Preâmbulo/BOM][2].


[1]: https://www.wikiod.com/pt/dotnet/cordas
[2]: http://stackoverflow.com/q/11701341/1207195

## Enumera arquivos mais antigos que uma quantidade especificada
Este trecho é uma função auxiliar para enumerar todos os arquivos mais antigos que uma idade especificada, é útil - por exemplo - quando você precisa excluir arquivos de log antigos ou dados em cache antigos.

    static IEnumerable<string> EnumerateAllFilesOlderThan(
                                   TimeSpan maximumAge,
                                   string path,
                                   string searchPattern = "*.*",
                                   SearchOption options = SearchOption.TopDirectoryOnly)
    {
        DateTime oldestWriteTime = DateTime.Now - maximumAge;

        return Directory.EnumerateFiles(path, searchPattern, options)
            .Where(x => Directory.GetLastWriteTime(x) < oldestWriteTime);
    }

Usado assim:

    var oldFiles = EnumerateAllFilesOlderThan(TimeSpan.FromDays(7), @"c:\log", "*.log");

Algumas coisas a serem observadas:

* A pesquisa é realizada usando `Directory.EnumerateFiles()` em vez de `Directory.GetFiles()`. A enumeração é _alive_ então você não precisará esperar até que todas as entradas do sistema de arquivos tenham sido buscadas.
* Estamos verificando o horário da última gravação, mas você pode usar o horário de criação ou o horário do último acesso (por exemplo, para excluir arquivos em cache _unused_, observe que o horário de acesso pode estar desabilitado).
* A granularidade não é uniforme para todas essas propriedades (tempo de gravação, tempo de acesso, tempo de criação), verifique o MSDN para obter detalhes sobre isso.


## Mover um arquivo de um local para outro
<h1>Arquivo.Mover</h1>
Para mover um arquivo de um local para outro, uma simples linha de código pode fazer isso:

`File.Move(@"C:\TemporaryFile.txt", @"C:\TemporaryFiles\TemporaryFile.txt");`

No entanto, há muitas coisas que podem dar errado com esta operação simples. Por exemplo, e se o usuário que está executando seu programa não tiver uma unidade rotulada como 'C'? E se eles o fizessem - mas eles decidiram renomeá-lo para 'B' ou 'M'?

E se o arquivo de origem (o arquivo no qual você gostaria de mover) foi movido sem o seu conhecimento - ou se ele simplesmente não existir.

Isso pode ser contornado verificando primeiro se o arquivo de origem existe:

    string source = @"C:\TemporaryFile.txt", destination = @"C:\TemporaryFiles\TemporaryFile.txt";
    if(File.Exists("C:\TemporaryFile.txt"))
    {
        File.Move(source, destination);
    }

Isso garantirá que, naquele exato momento, o arquivo exista e possa ser movido para outro local. Pode haver momentos em que uma simples chamada para `File.Exists` não será suficiente. Se não estiver, verifique novamente, transmita ao usuário que a operação falhou - ou trate a exceção.

Um `FileNotFoundException` não é a única exceção que você provavelmente encontrará.

Veja abaixo as possíveis exceções:

| Tipo de exceção | Descrição |
| ------ | ------ |
| `IOException` | O arquivo já existe ou o arquivo de origem não foi encontrado.
| `ArgumentNullException`| O valor dos parâmetros Source e/ou Destination é nulo. |
| `ArgumentException` | O valor dos parâmetros Source e/ou Destination está vazio ou contém caracteres inválidos. |
| `UnauthorizedAccessException` | Você não tem as permissões necessárias para executar esta ação. |
| `PathTooLongException` | A Origem, Destino ou caminho(s) especificado(s) excedem o comprimento máximo. No Windows, o comprimento de um caminho deve ter menos de 248 caracteres, enquanto os nomes de arquivo devem ter menos de 260 caracteres.
| `DirectoryNotFoundException` | O diretório especificado não foi encontrado. |
| `NotSupportedException` | Os caminhos ou nomes de arquivo de Origem ou Destino estão em um formato inválido.

## "Toque" uma grande quantidade de arquivos (para atualizar a última hora de gravação)
Este exemplo atualiza a última hora de gravação de um grande número de arquivos (usando `System.IO.Directory.EnumerateFiles` em vez de `System.IO.Directory.GetFiles()`). Opcionalmente, você pode especificar um padrão de pesquisa (o padrão é `"*.*"` e, eventualmente, pesquisar em uma árvore de diretórios (não apenas no diretório especificado):

    public static void Touch(string path,
                             string searchPattern = "*.*",
                             SearchOptions options = SearchOptions.None)
    {
        var now = DateTime.Now;
    
        foreach (var filePath in Directory.EnumerateFiles(path, searchPattern, options))
        {
            File.SetLastWriteTime(filePath, now);
        }
    }

