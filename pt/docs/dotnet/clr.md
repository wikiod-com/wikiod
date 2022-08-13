---
title: "CLR"
slug: "clr"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Uma introdução ao Common Language Runtime
O **Common Language Runtime (CLR)** é um ambiente de máquina virtual e parte do .NET Framework. Contém:

- Uma linguagem de bytecode portátil chamada **Common Intermediate Language** (abreviada como CIL ou IL)
- Um compilador Just-In-Time que gera código de máquina
- Um coletor de lixo de rastreamento que fornece gerenciamento automático de memória
- Suporte para subprocessos leves chamados AppDomains
- Mecanismos de segurança através dos conceitos de código verificável e níveis de confiança

O código executado no CLR é chamado de *código gerenciado* para distingui-lo do código executado fora do CLR (geralmente código nativo) que é chamado de *código não gerenciado*. Existem vários mecanismos que facilitam a interoperabilidade entre código gerenciado e não gerenciado.

