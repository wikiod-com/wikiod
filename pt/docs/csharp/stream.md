---
title: "Fluxo"
slug: "fluxo"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Usando fluxos
Um fluxo é um objeto que fornece um meio de baixo nível para transferir dados. Eles próprios não atuam como contêineres de dados.

Os dados com os quais lidamos estão na forma de byte array(`byte []`). As funções de leitura e escrita são todas orientadas a byte, por ex. `WriteByte()`.

Não há funções para lidar com inteiros, strings, etc. Isso torna o fluxo muito genérico, mas menos simples de se trabalhar se, digamos, você quiser apenas transferir texto. Os fluxos podem ser particularmente úteis quando você está lidando com uma grande quantidade de dados.

Precisaremos usar um tipo diferente de Stream com base em onde ele precisa ser escrito/lido (ou seja, o armazenamento de backup). Por exemplo, se a fonte for um arquivo, precisamos usar `FileStream`:

    string filePath = @"c:\Users\exampleuser\Documents\userinputlog.txt";
    using (FileStream fs = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
    {
        // do stuff here...
    
        fs.Close();
    }

Da mesma forma, `MemoryStream` é usado se o backing store for memória:

    // Read all bytes in from a file on the disk.
    byte[] file = File.ReadAllBytes(“C:\\file.txt”);

    // Create a memory stream from those bytes.
    using (MemoryStream memory = new MemoryStream(file))
    {
       // do stuff here...
    }

Da mesma forma, `System.Net.Sockets.NetworkStream` é usado para acesso à rede.

Todos os Streams são derivados da classe genérica `System.IO.Stream`. Os dados não podem ser lidos ou gravados diretamente de fluxos. O .NET Framework fornece classes auxiliares como `StreamReader`, `StreamWriter`, `BinaryReader` e `BinaryWriter` que convertem entre tipos nativos e a interface de fluxo de baixo nível e transferem os dados para ou do fluxo para você.

Ler e escrever em streams pode ser feito via `StreamReader` e `StreamWriter`. Deve-se ter cuidado ao fechá-los. Por padrão, o fechamento também fechará o fluxo contido e o tornará inutilizável para outros usos. Este comportamento padrão pode ser alterado usando um [construtor][1] que possui o parâmetro `bool leaveOpen` e configurando seu valor como `true`.


`StreamWriter`:

    FileStream fs = new FileStream("sample.txt", FileMode.Create);
    StreamWriter sw = new StreamWriter(fs);
    string NextLine = "This is the appended line.";
    sw.Write(NextLine);
    sw.Close();
    //fs.Close(); There is no need to close fs. Closing sw will also close the stream it contains.

`StreamReader`:

    using (var ms = new MemoryStream())
    {
        StreamWriter sw = new StreamWriter(ms);
        sw.Write(123);
        //sw.Close();     This will close ms and when we try to use ms later it will cause an exception
        sw.Flush();     //You can send the remaining data to stream. Closing will do this automatically
        // We need to set the position to 0 in order to read 
        // from the beginning.
        ms.Position = 0;
        StreamReader sr = new StreamReader(ms);
        var myStr = sr.ReadToEnd();
        sr.Close();
        ms.Close();
    }

Como as classes `Stream`, `StreamReader`, `StreamWriter`, etc. implementam a interface `IDisposable`, podemos chamar o método `Dispose()` em objetos dessas classes.


[1]: https://msdn.microsoft.com/en-us/library/gg712952(v=vs.110).aspx

