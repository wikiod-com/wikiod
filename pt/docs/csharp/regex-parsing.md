---
title: "Análise de Regex"
slug: "analise-de-regex"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Sintaxe
- `new Regex(pattern);` //*Cria uma nova instância com um padrão definido.*
- `Regex.Match(input);` //*Inicia a pesquisa e retorna o Match.*
- `Regex.Matches(input);` //*Inicia a pesquisa e retorna uma MatchCollection*


## Parâmetros
| Nome | Detalhes|
| ------ | ------ |
| Padrão | O padrão `string` que deve ser usado para a pesquisa. Para mais informações: [msdn][1]|
| RegexOptions *[Opcional]* | As opções comuns aqui são `Singleline` e `Multiline`. Eles estão mudando o comportamento de elementos de padrão como o ponto (.) que não cobre uma `NewLine` (\n) em `Multiline-Mode` mas em `SingleLine-Mode`. Comportamento padrão: [msdn][2] |
| Tempo limite *[Opcional]* | Onde os padrões estão ficando mais complexos, a pesquisa pode consumir mais tempo. Este é o tempo limite passado para a pesquisa, assim como conhecido da programação de rede.|


[1]: https://msdn.microsoft.com/en-us/library/ae5bf541(v=vs.90).aspx
[2]: https://msdn.microsoft.com/en-US/library/yd1hzczs(v=vs.110).aspx#Default

**Necessário usar**

    using System.Text.RegularExpressions;

**Bom ter**

- Você pode testar seus padrões online sem a necessidade de compilar sua solução para obter resultados aqui: [Clique em mim][1]
- Exemplo Regex101: [Clique em mim][2]

_________

*Especialmente os iniciantes tendem a sobrecarregar suas tarefas com regex porque parece poderoso e no lugar certo para pesquisas baseadas em texto mais complexas. Este é o ponto em que as pessoas tentam analisar documentos xml com regex sem nem mesmo perguntar a si mesmas se poderia haver uma classe já concluída para esta tarefa como `XmlDocument`.*

*Regex deve ser a última arma a escolher contra a complexidade. Pelo menos não se esqueça de fazer algum esforço para procurar o 'caminho certo' antes de escrever 20 linhas de padrões.*


[1]: https://regex101.com/
[2]: https://regex101.com/r/cG9lP5/1


## Partida única
*`usando System.Text.RegularExpressions;`*

    string pattern = ":(.*?):";
    string lookup = "--:text in here:--";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    // Get the match from your regex-object
    Match mLookup = rgxLookup.Match(lookup);
    
    // The group-index 0 always covers the full pattern.
    // Matches inside parentheses will be accessed through the index 1 and above.
    string found = mLookup.Groups[1].Value;

**Resultado:**

    found = "text in here"

## Várias correspondências
*`usando System.Text.RegularExpressions;`*

    List<string> found = new List<string>();
    string pattern = ":(.*?):";
    string lookup = "--:text in here:--:another one:-:third one:---!123:fourth:";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    MatchCollection mLookup = rgxLookup.Matches(lookup);
    
    foreach(Match match in mLookup)
    {
        found.Add(match.Groups[1].Value);
    }

**Resultado:**

    found = new List<string>() { "text in here", "another one", "third one", "fourth" }

