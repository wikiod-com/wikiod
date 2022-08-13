---
title: "Inicializando Propriedades"
slug: "inicializando-propriedades"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

Ao decidir sobre como criar uma propriedade, comece com uma propriedade autoimplementada para simplicidade e brevidade.

Mude para uma propriedade com um campo de apoio somente quando as circunstâncias o exigirem. Se você precisar de outras manipulações além de um simples set e get, talvez seja necessário introduzir um campo de apoio.

## C# 6.0: Inicialize uma propriedade implementada automaticamente
Crie uma propriedade com getter e/ou setter e inicialize tudo em uma linha:

    public string Foobar { get; set; } = "xyz";

## Inicializando a propriedade com um campo de apoio
    public string Foobar { 
        get { return _foobar; }
        set { _foobar = value; }
    }
    private string _foobar = "xyz";

## Inicializando Propriedade no Construtor
    class Example
    {
        public string Foobar { get; set; }
        public List<string> Names { get; set; }
        public Example()
        {
            Foobar = "xyz";
            Names = new List<string>(){"carrot","fox","ball"};
        }
    }

## Inicialização da propriedade durante a instanciação do objeto
As propriedades podem ser definidas quando um objeto é instanciado.

    var redCar = new Car 
    {
        Wheels = 2,
        Year = 2016,
        Color = Color.Red
    };

