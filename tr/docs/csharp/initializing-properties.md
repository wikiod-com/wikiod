---
title: "Özellikleri Başlatma"
slug: "ozellikleri-baslatma"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

Bir özelliğin nasıl oluşturulacağına karar verirken, basitlik ve kısalık için otomatik olarak uygulanan bir özellik ile başlayın.

Yalnızca koşullar gerektirdiğinde bir destek alanı olan bir özelliğe geçin. Basit bir set ve almanın ötesinde başka manipülasyonlara ihtiyacınız varsa, bir destek alanı tanıtmanız gerekebilir.

## C# 6.0: Otomatik Uygulanan Özelliği Başlatma
Alıcı ve/veya ayarlayıcı ile bir özellik oluşturun ve hepsini tek bir satırda başlatın:

    public string Foobar { get; set; } = "xyz";

## Özelliği Destek Alanıyla Başlatma
    public string Foobar { 
        get { return _foobar; }
        set { _foobar = value; }
    }
    private string _foobar = "xyz";

## Constructor'da Özelliği Başlatma
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

## Nesne başlatma sırasında Özellik Başlatma
Özellikler, bir nesne başlatıldığında ayarlanabilir.

    var redCar = new Car 
    {
        Wheels = 2,
        Year = 2016,
        Color = Color.Red
    };

