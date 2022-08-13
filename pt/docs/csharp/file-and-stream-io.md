---
title: "ES de arquivo e fluxo"
slug: "es-de-arquivo-e-fluxo"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

Gerencia arquivos.

## Sintaxe
- `novo System.IO.StreamWriter(caminho da string)`
- `new System.IO.StreamWriter(string path, bool append)`
- `System.IO.StreamWriter.WriteLine(string text)`
- `System.IO.StreamWriter.WriteAsync(string text)`
- `System.IO.Stream.Close()`
- `System.IO.File.ReadAllText(caminho da string)`
- `System.IO.File.ReadAllLines(string path)`
- `System.IO.File.ReadLines(string path)`
- `System.IO.File.WriteAllText(caminho da string, texto da string)`
- `System.IO.File.WriteAllLines(string path, IEnumerable<string> contents)`
- `System.IO.File.Copy(string source, string dest)`
- `System.IO.File.Create(string path)`
- `System.IO.File.Delete(string path)`
- `System.IO.File.Move(string source, string dest)`
- `System.IO.Directory.GetFiles(string path)`

## Parâmetros
| Parâmetro | Detalhes |
| ------ | ------ |
| caminho | A localização do arquivo. |
| anexar | Se o arquivo existir, true adicionará dados ao final do arquivo (anexar), false substituirá o arquivo. |
| texto | Texto a ser escrito ou armazenado. |
|conteúdo | Uma coleção de strings a serem escritas. |
| fonte | O local do arquivo que você deseja usar. |
| destino | O local para o qual você deseja que um arquivo vá. |

- Sempre certifique-se de fechar objetos `Stream`. Isso pode ser feito com um bloco `using` como mostrado acima ou chamando manualmente `myStream.Close()`.
- Certifique-se de que o usuário atual tenha as permissões necessárias no caminho em que você está tentando criar o arquivo.
- [Strings Verbatim](https://www.wikiod.com/pt/docs/c%23/16/verbatim-strings) devem ser usadas ao declarar uma string de caminho que inclui barras invertidas, assim: `@"C:\MyFolder\MyFile .txt"`

## Lendo de um arquivo usando a classe System.IO.File
Você pode usar o **[System.IO.File.ReadAllText](https://msdn.microsoft.com/en-us/library/system.io.file.readalltext(v=vs.110).aspx)* * função para ler todo o conteúdo de um arquivo em uma string.

    string text = System.IO.File.ReadAllText(@"C:\MyFolder\MyTextFile.txt");

Você também pode ler um arquivo como uma matriz de linhas usando o **[System.IO.File.ReadAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.readlines(v =vs.110).aspx)** função:
    
    string[] lines = System.IO.File.ReadAllLines(@"C:\MyFolder\MyTextFile.txt");

## Lendo lentamente um arquivo linha por linha por meio de um IEnumerable
Ao trabalhar com arquivos grandes, você pode usar o método `System.IO.File.ReadLines` para ler todas as linhas de um arquivo em um `IEnumerable<string>`. Isso é semelhante ao `System.IO.File.ReadAllLines`, exceto que ele não carrega o arquivo inteiro na memória de uma só vez, tornando-o mais eficiente ao trabalhar com arquivos grandes.

    IEnumerable<string> AllLines = File.ReadLines("file_name.txt", Encoding.Default);

_O segundo parâmetro de File.ReadLines é opcional. Você pode usá-lo quando for necessário especificar a codificação._

É importante notar que chamar `ToArray`, `ToList` ou outra função similar irá forçar todas as linhas a serem carregadas de uma vez, significando que o benefício de usar `ReadLines` é anulado. É melhor enumerar o `IEnumerable` usando um loop `foreach` ou LINQ se estiver usando este método.


## Escrevendo linhas em um arquivo usando a classe System.IO.StreamWriter
A classe **[System.IO.StreamWriter](https://msdn.microsoft.com/en-us/library/system.io.streamwriter(v=vs.110).aspx)**:
>Implementa um TextWriter para escrever caracteres em um fluxo em uma codificação específica.

Usando o método `WriteLine`, você pode escrever conteúdo linha por linha em um arquivo.

Observe o uso da palavra-chave `using` que garante que o objeto StreamWriter seja descartado assim que sair do escopo e assim o arquivo ser fechado.

    string[] lines = { "My first string", "My second string", "and even a third string" };
    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt"))
    {
        foreach (string line in lines)
        {
            sw.WriteLine(line);
        }
    }

Observe que o StreamWriter pode receber um segundo parâmetro `bool` em seu construtor, permitindo `Append` a um arquivo em vez de sobrescrever o arquivo:

    bool appendExistingFile = true;
    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt", appendExistingFile ))
    {
        sw.WriteLine("This line will be appended to the existing file");
    }

## Gravando em um arquivo usando a classe System.IO.File
Você pode usar o **[System.IO.File.WriteAllText](https://msdn.microsoft.com/en-us/library/system.io.file.writealltext(v=vs.110).aspx)* * função para escrever uma string em um arquivo.

    string text = "String that will be stored in the file";
    System.IO.File.WriteAllText(@"C:\MyFolder\OutputFile.txt", text);

Você também pode usar o **[System.IO.File.WriteAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.writealllines(v=vs.110).aspx) ** função que recebe um `IEnumerable<String>` como segundo parâmetro (em oposição a uma única string no exemplo anterior). Isso permite que você escreva conteúdo de uma matriz de linhas.

    string[] lines = { "My first string", "My second string", "and even a third string" };
    System.IO.File.WriteAllLines(@"C:\MyFolder\OutputFile.txt", lines);

## Copiar arquivo
**Classe estática do arquivo**

A classe estática `File` pode ser facilmente usada para este propósito.

    File.Copy(@"sourcePath\abc.txt", @"destinationPath\abc.txt");
    File.Copy(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

**Observação:** por esse método, o arquivo é copiado, o que significa que ele será lido da origem e depois gravado no caminho de destino. Este é um processo que consome recursos, levaria um tempo relativo ao tamanho do arquivo e pode fazer com que seu programa congele se você não utilizar threads.

## Gravação assíncrona de texto em um arquivo usando StreamWriter


## Criar arquivo
**Classe estática do arquivo**

Usando o método `Create` da classe estática `File` podemos criar arquivos. O método cria o arquivo no caminho dado, ao mesmo tempo que abre o arquivo e nos dá o `FileStream` do arquivo. Certifique-se de fechar o arquivo depois de terminar com ele.

ex1:

    var fileStream1 = File.Create("samplePath");
    /// you can write to the fileStream1
    fileStream1.Close();

ex2:

    using(var fileStream1 = File.Create("samplePath"))
    {
        /// you can write to the fileStream1
    }

ex3:

    File.Create("samplePath").Close();

**Aula FileStream**

Existem muitas sobrecargas deste construtor de classes que estão bem documentadas [aqui][1]. O exemplo abaixo é para aquele que abrange as funcionalidades mais utilizadas desta classe.

    var fileStream2 = new FileStream("samplePath", FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None);

Você pode verificar as enumerações para [FileMode][2], [FileAccess][3] e [FileShare][4] nesses links. O que eles basicamente significam são os seguintes:

*FileMode:* Respostas "O arquivo deve ser criado? aberto? criar se não existir, então abrir?" tipo perguntas.

*FileAccess:* Respostas "Devo ser capaz de ler o arquivo, gravar no arquivo ou ambos?" tipo perguntas.

*FileShare:* Respostas "Os outros usuários devem poder ler, gravar etc. no arquivo enquanto eu o estiver usando simultaneamente?" tipo perguntas.


[1]: https://msdn.microsoft.com/en-us/library/system.io.filestream(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.io.filemode(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/4z36sx0f(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.io.fileshare(v=vs.110).aspx

## Mover arquivo
**Classe estática do arquivo**

A classe estática do arquivo pode ser facilmente usada para essa finalidade.

    File.Move(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

**Observação1:** altera apenas o índice do arquivo (se o arquivo for movido no mesmo volume). Esta operação não leva tempo relativo ao tamanho do arquivo.

**Remark2:** não é possível substituir um arquivo existente no caminho de destino.

## Excluir arquivo
    string path = @"c:\path\to\file.txt";
    File.Delete(path);

Enquanto `Delete` não lança exceção se o arquivo não existir, ele lançará exceção, por exemplo se o caminho especificado for inválido ou o chamador não tiver as permissões necessárias. Você deve sempre agrupar chamadas para `Delete` dentro de [try-catch block][1] e tratar todas as exceções esperadas. No caso de possíveis condições de corrida, envolva a lógica dentro de [instrução de bloqueio][2].


[1]: https://www.wikiod.com/pt/docs/c%23/26/keywords/148/try-catch-finally-throw#t=201608021340222162938
[2]: https://www.wikiod.com/pt/docs/c%23/1495/lock-statement/4865/simple-usage#t=201608021343504970522

## Arquivos e diretórios
**Obter todos os arquivos no diretório**
    
     var FileSearchRes = Directory.GetFiles(@Path, "*.*", SearchOption.AllDirectories);

Retorna um array de `FileInfo`, representando todos os arquivos no diretório especificado.

**Obter arquivos com extensão específica**

     var FileSearchRes = Directory.GetFiles(@Path, "*.pdf", SearchOption.AllDirectories);

Retorna um array de `FileInfo`, representando todos os arquivos no diretório especificado com a extensão especificada.

