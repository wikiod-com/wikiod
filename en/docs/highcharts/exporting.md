---
title: "Exporting"
slug: "exporting"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Exporting is the feature where we can export our chart data to the format we need. Normally we have two kind of exporting,

 1. Server side exporting
 2. Client side exporting

## Server side exporting
If you click on export icon, you will given options to choose like on which format you need to export the data. Just click on anything you wish. You can see that the data is being send to the server http://export.highcharts.com/ in the top left side.

    exporting: {
                chartOptions: {
                    plotOptions: {
                        series: {
                            dataLabels: {
                                enabled: true
                            }
                        }
                    }
                },
                scale: 3
            }

## Client side exporting
You need to add a reference of the file called [offline-exporting.js][1] to make the client side exporting available. Please be noted this feature is not supported in browser IE 8, So if you load this chart in any unsupported browser, the module will fall back to the export server. This can be handled with the option fallbackToExportServer: false. You need to set this option in your exporting settings as follows.

    exporting: {
              chartOptions: {
                  plotOptions: {
                      series: {
                          dataLabels: {
                              enabled: true
                          }
                      }
                  }
              },
              scale: 3,
              fallbackToExportServer: false
          }


  [1]: https://code.highcharts.com/modules/offline-exporting.js

