---
title: "Cordas"
slug: "cordas"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Em strings .NET `System.String` são seqüências de caracteres `System.Char`, cada caractere é uma unidade de código codificada em UTF-16. Essa distinção é importante porque a definição de _linguagem falada_ de _character_ e .NET (e muitas outras linguagens) definição de caractere são diferentes.

Um _character_, que deve ser corretamente chamado [grapheme][1], é exibido como um [glyph][2] e é definido por um ou mais Unicode [code-points][3]. Cada code-point é então codificado em uma sequência de [code-unidades][4]. Agora deve ficar claro porque um único `System.Char` nem sempre representa um grafema, vamos ver no mundo real como eles são diferentes:

* Um grafema, devido a [combinação de caracteres][5], pode resultar em dois ou mais code-points: <kbd>à</kbd> é composto por dois code-points: _U+0061 LATIN SMALL LETTER A_ e _U+ 0300 COMBINAÇÃO DE ACENTO GRAVE_. Este é o erro mais comum porque `"à".Length == 2` enquanto você pode esperar `1`.
* Existem caracteres duplicados, por exemplo <kbd>à</kbd> pode ser um único code-point _U+00E0 LATIN SMALL LETTER A WITH GRAVE_ ou dois code-points como explicado acima. Obviamente, eles devem comparar o mesmo: `"\u00e0" == "\u0061\u0300"` (mesmo se `"\u00e0".Length != "\u0061\u0300".Length`). Isso é possível devido à _normalização de string_ realizada pelo método `String.Normalize()`.
* Uma sequência Unicode pode conter uma sequência composta ou decomposta, por exemplo, o caractere <kbd>한</kbd> _U+D55C HAN CHARACTER_ pode ser um único ponto de código (codificado como uma única unidade de código em UTF-16) ou um seqüência decomposta de suas sílabas <kbd>ᄒ</kbd>, <kbd>ᅡ</kbd> e <kbd>ᆫ</kbd>. Eles devem ser comparados iguais.
* Um ponto de código pode ser codificado para mais de uma unidade de código: caractere <kbd>𠂊</kbd> _U+2008A HAN CHARACTER_ é codificado como dois `System.Char` (`"\ud840\udc8a"`) mesmo se for apenas um ponto de código: a codificação UTF-16 não tem tamanho fixo! Esta é uma fonte de inúmeros bugs (também sérios bugs de segurança), se, por exemplo, seu aplicativo aplica um comprimento máximo e trunca cegamente a string, então você pode criar uma string inválida.
* Alguns idiomas têm [dígrafo][6] e trígrafos, por exemplo em tcheco <kbd>ch</kbd> é uma letra independente (depois de <kbd>h</kbd> e antes de <kbd>i</kbd> então ao encomendar uma lista de strings você terá *fyzika* antes de *chemie*.

Há muito mais problemas sobre manipulação de texto, veja, por exemplo, [Como posso executar uma comparação de caractere com reconhecimento de Unicode por caractere?][7] para uma introdução mais ampla e mais links para argumentos relacionados.

Em geral, ao lidar com texto _international_, você pode usar esta função simples para enumerar elementos de texto em uma string (evitando quebrar substitutos e codificação Unicode):

    public static class StringExtensions
    {
        public static IEnumerable<string> EnumerateCharacters(this string s)
        {
            if (s == null)
                return Enumerable.Empty<string>();

            var enumerator = StringInfo.GetTextElementEnumerator(s.Normalize());
            while (enumerator.MoveNext())
                yield return (string)enumerator.Value;
        }
    }


[1]: https://en.wikipedia.org/wiki/Grapheme
[2]: https://en.wikipedia.org/wiki/Glyph
[3]: https://en.wikipedia.org/wiki/Code_point
[4]: https://en.wikipedia.org/wiki/Character_encoding#Code_unit
[5]: https://en.wikipedia.org/wiki/Combining_character
[6]: https://en.wikipedia.org/wiki/Digraph_(ortografia)
[7]: http://stackoverflow.com/q/27229589/1207195

## Contar caracteres
Se você precisa contar _characters_ então, pelas razões explicadas na seção _Remarks_, você não pode simplesmente usar a propriedade Length porque é o comprimento da matriz de `System.Char` que não são caracteres, mas unidades de código (não Unicode code- pontos nem grafemas). O código correto é então:

    int length = text.EnumerateCharacters().Count();

Uma pequena otimização pode reescrever o método de extensão `EnumerateCharacters()` especificamente para este propósito:

    public static class StringExtensions
    {
        public static int CountCharacters(this string text)
        {
            if (String.IsNullOrEmpty(text))
                return 0;
    
            int count = 0;
            var enumerator = StringInfo.GetTextElementEnumerator(text);
            while (enumerator.MoveNext())
                ++count;
    
            return count;
        }
    }

## Contar caracteres distintos
Se você precisar contar caracteres distintos, pelas razões explicadas na seção *Observações*, você não pode simplesmente usar a propriedade `Length` porque é o comprimento da matriz de `System.Char` que não são caracteres, mas unidades de código (não pontos de código Unicode nem grafemas). Se, por exemplo, você simplesmente escrever `text.Distinct().Count()`, você obterá resultados incorretos, código correto:

    int distinctCharactersCount = text.EnumerateCharacters().Count();

Um passo adiante é **contar ocorrências de cada caractere**, se o desempenho não for um problema, você pode simplesmente fazer assim (neste exemplo, independentemente do caso):

    var frequencies = text.EnumerateCharacters()
        .GroupBy(x => x, StringComparer.CurrentCultureIgnoreCase)
        .Select(x => new { Character = x.Key, Count = x.Count() };

## Converter string para/de outra codificação
Strings .NET contêm `System.Char` (unidades de código UTF-16). Se você deseja salvar (ou gerenciar) texto com outra codificação, você deve trabalhar com um array de `System.Byte`.

As conversões são realizadas por classes derivadas de `System.Text.Encoder` e `System.Text.Decoder` que, juntos, podem converter de/para outra codificação (de um array codificado _X_ byte `byte[]` para um UTF-16 codificado `System.String` e vice-versa).

Como o codificador/decodificador geralmente funciona muito próximo um do outro, eles são agrupados em uma classe derivada de `System.Text.Encoding`, as classes derivadas oferecem conversões de/para codificações populares (UTF-8, UTF-16 e assim por diante ).

Exemplos:
=

Converter uma string para UTF-8
-
    byte[] data = Encoding.UTF8.GetBytes("This is my text");
---
Converter dados UTF-8 em uma string
-
    var text = Encoding.UTF8.GetString(data);

---
Alterar a codificação de um arquivo de texto existente
-

Este código lerá o conteúdo de um arquivo de texto codificado em UTF-8 e o salvará novamente codificado como UTF-16. Observe que este código não é ideal se o arquivo for grande porque ele lerá todo o seu conteúdo na memória:

    var content = File.ReadAllText(path, Encoding.UTF8);
    File.WriteAllText(content, Encoding.UTF16);

## Comparando strings
Apesar de `String` ser um tipo de referência, o operador `==` compara valores de string em vez de referências.

Como você deve saber, `string` é apenas um array de caracteres. Mas se você acha que a verificação e comparação de igualdade de strings é feita caractere por caractere, está enganado. Essa operação é específica da cultura (consulte Comentários abaixo): algumas sequências de caracteres podem ser tratadas como iguais dependendo da [cultura][1].

Pense duas vezes antes de verificar a igualdade de curto-circuito comparando `Length` [propriedades][2] de duas strings!

Use sobrecargas de `String.Equals` [método][3] que aceitam valor adicional de `StringComparison` [enumeração][4], se você precisar alterar o comportamento padrão.


[1]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx
[2]: https://msdn.microsoft.com/library/system.string.length(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/t4411bks(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.stringcomparison(v=vs.110).aspx

## Contar ocorrências de um personagem
Por causa dos motivos explicados na seção _Observações_, você não pode simplesmente fazer isso (a menos que queira contar ocorrências de uma unidade de código específica):

    int count = text.Count(x => x == ch);

Você precisa de uma função mais complexa:

    public static int CountOccurrencesOf(this string text, string character)
    {
        return text.EnumerateCharacters()
            .Count(x => String.Equals(x, character, StringComparer.CurrentCulture));
    }

Observe que a comparação de strings (em contraste com a comparação de caracteres, que é invariante de cultura) sempre deve ser executada de acordo com as regras de uma cultura específica.

## Divida a string em blocos de comprimento fixo
Não podemos quebrar uma string em pontos arbitrários (porque um `System.Char` pode não ser válido sozinho porque é um caractere de combinação ou parte de um substituto), então o código deve levar isso em consideração (observe que com _length_ quero dizer o número de _graphemes_ não o número de _code-units_):

    public static IEnumerable<string> Split(this string value, int desiredLength)
    {
        var characters = StringInfo.GetTextElementEnumerator(value);
        while (characters.MoveNext())
            yield return String.Concat(Take(characters, desiredLength));
    }
    
    private static IEnumerable<string> Take(TextElementEnumerator enumerator, int count)
    {
        for (int i = 0; i < count; ++i)
        {
            yield return (string)enumerator.Current;
    
            if (!enumerator.MoveNext())
                yield break;
        }
    }

## Método virtual Object.ToString()
Tudo em .NET é um objeto, portanto, cada tipo tem `ToString()` [método][1] definido em `Object` [class][2] que pode ser substituído. A implementação padrão deste método apenas retorna o nome do tipo:

    public class Foo
    {
    }
    
    var foo = new Foo();
    Console.WriteLine(foo); // outputs Foo

`ToString()` é chamado implicitamente ao concatenar valor com uma string:

    public class Foo
    {
        public override string ToString()
        {
            return "I am Foo";
        }
    }
    
    var foo = new Foo();
    Console.WriteLine("I am bar and "+foo);// outputs I am bar and I am Foo

O resultado deste método também é amplamente utilizado por ferramentas de depuração. Se, por algum motivo, você não quiser substituir esse método, mas quiser personalizar como o depurador mostra o valor do seu tipo, use [DebuggerDisplay Attribute][4] ([MSDN][3]):

    // [DebuggerDisplay("Person = FN {FirstName}, LN {LastName}")]
    [DebuggerDisplay("Person = FN {"+nameof(Person.FirstName)+"}, LN {"+nameof(Person.LastName)+"}")]
    public class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set;}
        // ...
    }


[1]: https://msdn.microsoft.com/en-us/library/system.object.tostring(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.object(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.diagnostics.debuggerdisplayattribute(v=vs.110).aspx
[4]: https://www.wikiod.com/pt/docs/c%23/1062/attributes/4689/debuggerdisplay-attribute#t=201702221225586559231

## Imutabilidade de strings
Strings são imutáveis. Você simplesmente não pode alterar a string existente. Qualquer operação na string cria uma nova instância da string com um novo valor. Isso significa que se você precisar substituir um único caractere em uma string muito longa, a memória será alocada para um novo valor.

    string veryLongString = ...
    // memory is allocated
    string newString = veryLongString.Remove(0,1); // removes first character of the string.

Se você precisar realizar muitas operações com valor de string, use `StringBuilder` [class][1] que é projetado para manipulação eficiente de strings:

    var sb = new StringBuilder(someInitialString);
    foreach(var str in manyManyStrings)
    {
        sb.Append(str);
    } 
    var finalString = sb.ToString();

[1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

