---
title: "Converting Data"
slug: "converting-data"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## JSON
        import 'dart:convert';

        void main() {
            var jsonString = """
              {
                "cats": {
                        "abysinnian": {
                            "origin": "Burma",
                            "behavior": "playful"
                        }
                    }
            }
          """;

          var obj = JSON.decode(jsonString);

          print(obj['cats']['abysinnian']['behavior']); // playful
        }

See example on dartpad: https://dartpad.dartlang.org/7d5958cf10e611b36326f27b062108fe

