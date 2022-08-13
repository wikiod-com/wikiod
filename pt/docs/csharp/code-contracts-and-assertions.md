---
title: "Contratos e declarações de código"
slug: "contratos-e-declaracoes-de-codigo"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Asserções para verificar a lógica devem sempre ser verdadeiras
Asserções são usadas não para realizar testes de parâmetros de entrada, mas para verificar se o fluxo do programa está correto -- ou seja, se você pode fazer certas suposições sobre seu código em um determinado momento. Em outras palavras: um teste feito com `Debug.Assert` deve *sempre* assumir que o valor testado é `true`.

Debug.Assert só é executado em compilações DEBUG; ele é filtrado de compilações RELEASE. Deve ser considerado uma ferramenta de depuração além do teste de unidade e não como uma substituição de contratos de código ou métodos de validação de entrada.

Por exemplo, esta é uma boa afirmação:

    var systemData = RetrieveSystemConfiguration();
    Debug.Assert(systemData != null);

Aqui assert é uma boa escolha porque podemos supor que RetrieveSystemConfiguration() retornará um valor válido e nunca retornará null.

Aqui está outro bom exemplo:

    UserData user = RetrieveUserData();
    Debug.Assert(user != null);
    Debug.Assert(user.Age > 0);
    int year = DateTime.Today.Year - user.Age;

Primeiro, podemos supor que RetrieveUserData() retornará um valor válido. Então, antes de usar a propriedade Age, verificamos a suposição (que sempre deve ser verdadeira) de que a idade do usuário é estritamente positiva.

Este é um mau exemplo de assert:

    string input = Console.ReadLine();
    int age = Convert.ToInt32(input);
    Debug.Assert(age > 16);
    Console.WriteLine("Great, you are over 16");

Assert não é para validação de entrada porque é incorreto supor que essa asserção sempre será verdadeira. Você deve usar métodos de validação de entrada para isso. No caso acima, você também deve verificar se o valor de entrada é um número em primeiro lugar.


