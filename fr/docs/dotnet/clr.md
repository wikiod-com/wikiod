---
title: "CLR"
slug: "clr"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Une introduction au Common Language Runtime
Le **Common Language Runtime (CLR)** est un environnement de machine virtuelle et fait partie du .NET Framework. Il contient:

- Un langage de bytecode portable appelé **Common Intermediate Language** (en abrégé CIL ou IL)
- Un compilateur Just-In-Time qui génère du code machine
- Un ramasse-miettes de traçage qui assure une gestion automatique de la mémoire
- Prise en charge de sous-processus légers appelés AppDomains
- Mécanismes de sécurité à travers les concepts de code vérifiable et niveaux de confiance

Le code qui s'exécute dans le CLR est appelé *code géré* pour le distinguer du code exécuté en dehors du CLR (généralement du code natif) qui est appelé *code non géré*. Il existe divers mécanismes qui facilitent l'interopérabilité entre le code managé et non managé.

