---
title: "Inicializando Propiedades"
slug: "inicializando-propiedades"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

Al decidir cómo crear una propiedad, comience con una propiedad implementada automáticamente por simplicidad y brevedad.

Cambie a una propiedad con un campo de respaldo solo cuando las circunstancias lo exijan. Si necesita otras manipulaciones más allá de un simple conjunto y obtención, es posible que deba introducir un campo de respaldo.

## C# 6.0: inicializar una propiedad implementada automáticamente
Cree una propiedad con getter y/o setter e inicialice todo en una línea:

    public string Foobar { get; set; } = "xyz";

## Inicializar propiedad con un campo de respaldo
    public string Foobar { 
        get { return _foobar; }
        set { _foobar = value; }
    }
    private string _foobar = "xyz";

## Inicializando Propiedad en Constructor
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

## Inicialización de propiedades durante la creación de instancias de objetos
Las propiedades se pueden establecer cuando se crea una instancia de un objeto.

    var redCar = new Car 
    {
        Wheels = 2,
        Year = 2016,
        Color = Color.Red
    };

