---
title: "Initialisation des propriétés"
slug: "initialisation-des-proprietes"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

Lorsque vous décidez comment créer une propriété, commencez par une propriété mise en œuvre automatiquement pour plus de simplicité et de brièveté.

Basculez vers une propriété avec un champ de sauvegarde uniquement lorsque les circonstances l'exigent. Si vous avez besoin d'autres manipulations au-delà d'un simple set and get, vous devrez peut-être introduire un champ de sauvegarde.

## C# 6.0 : Initialiser une propriété implémentée automatiquement
Créez une propriété avec getter et/ou setter et initialisez le tout sur une seule ligne :

    public string Foobar { get; set; } = "xyz";

## Initialisation de la propriété avec un champ de support
    public string Foobar { 
        get { return _foobar; }
        set { _foobar = value; }
    }
    private string _foobar = "xyz";

## Initialisation de la propriété dans le constructeur
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

## Initialisation de la propriété lors de l'instanciation de l'objet
Les propriétés peuvent être définies lorsqu'un objet est instancié.

    var redCar = new Car 
    {
        Wheels = 2,
        Year = 2016,
        Color = Color.Red
    };

