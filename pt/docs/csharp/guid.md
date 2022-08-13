---
title: "Guia"
slug: "guia"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

GUID (ou UUID) é um acrônimo para 'Globally Unique Identifier' (ou 'Universal Unique Identifier'). É um número inteiro de 128 bits usado para identificar recursos.

`Guid`s são *Identificadores Exclusivos Globais*, também conhecidos como *UUID*'s, *Identificadores Exclusivos Universais*.

São valores pseudoaleatórios de 128 bits. Existem tantos `Guid`s válidos (cerca de 10^18 `Guid`s para cada célula de cada povo na Terra) que, se forem gerados por um bom algoritmo pseudo-aleatório, podem ser considerados únicos em todo o universo por todos os métodos práticos. significa.

`Guid`s são mais frequentemente usados ​​como chaves primárias em bancos de dados. A vantagem deles é que você não precisa chamar o banco de dados para obter um novo ID que é (quase) garantido como exclusivo.

## Obtendo a representação de string de um Guid
Uma representação de string de um Guid pode ser obtida usando o método embutido `ToString`

    string myGuidString = myGuid.ToString();

Dependendo de suas necessidades, você também pode formatar o Guid, adicionando um argumento de tipo de formato à chamada `ToString`.

    var guid = new Guid("7febf16f-651b-43b0-a5e3-0da8da49e90d");

    // None          "7febf16f651b43b0a5e30da8da49e90d"
    Console.WriteLine(guid.ToString("N"));

    // Hyphens       "7febf16f-651b-43b0-a5e3-0da8da49e90d"
    Console.WriteLine(guid.ToString("D"));

    // Braces        "{7febf16f-651b-43b0-a5e3-0da8da49e90d}"
    Console.WriteLine(guid.ToString("B"));

    // Parentheses   "(7febf16f-651b-43b0-a5e3-0da8da49e90d)"
    Console.WriteLine(guid.ToString("P"));

    // Hex           "{0x7febf16f,0x651b,0x43b0{0xa5,0xe3,0x0d,0xa8,0xda,0x49,0xe9,0x0d}}"
    Console.WriteLine(guid.ToString("X"));


## Criando um guia
Estas são as formas mais comuns de criar uma instância de Guid:

- Criando um guid vazio (`00000000-0000-0000-0000-000000000000`):


    Guid g = Guid.Empty;
    Guid g2 = new Guid();

- Criando um novo Guid (pseudorandom):


    Guid g = Guid.NewGuid();

- Criação de Guids com um valor específico:


    Guid g = new Guid("0b214de7-8958-4956-8eed-28f9ba2c47c6");
    Guid g2 = new Guid("0b214de7895849568eed28f9ba2c47c6");
    Guid g3 = Guid.Parse("0b214de7-8958-4956-8eed-28f9ba2c47c6");


## Declarando um GUID anulável
Como outros tipos de valor, o GUID também possui um tipo anulável que pode receber valor nulo.

Declaração:

    Guid? myGuidVar = null;

Isso é particularmente útil ao recuperar dados do banco de dados quando existe a possibilidade de que o valor de uma tabela seja NULL.

