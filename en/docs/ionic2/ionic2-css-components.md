---
title: "Ionic2 CSS components"
slug: "ionic2-css-components"
draft: false
images: []
weight: 9877
type: docs
toc: true
---

## Grid
Ionic’s grid system is based on flexbox, a CSS feature supported by all devices that Ionic supports. The grid is composed of three units-grid, rows and columns. Columns will expand to fill their row, and will resize to fit additional columns.

| Class| Width|
| ------ | ------ |
| width-10 | 10%|
| width-20 | 20%|
| width-25 | 25%|
|width-33  |  33.3333%|
|width-50 |   50%|
|width-67    |66.6666%|
|width-75    |75%|
|width-80    |80%|
|width-90    |90%|

  Example.

        <ion-grid>
          <ion-row>
            <ion-col width-10>This column will take 10% of space</ion-col>
          </ion-row>
        </ion-grid>

## Cards
Cards are a great way to display important pieces of content, and are quickly emerging as a core design pattern for apps. They're are a great way to contain and organize information, while also setting up predictable expectations for the user. With so much content to display at once, and often so little screen real estate, cards have fast become the design pattern of choice for many companies.

Example.

    <ion-card>
      <ion-card-header>
            Header
      </ion-card-header>
      <ion-card-content>
            The British use the term "header", but the American term "head-shot" the English simply refuse to adopt.
      </ion-card-content>
    </ion-card>

