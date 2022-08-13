---
title: "CLR"
slug: "clr"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Una introducción a Common Language Runtime
**Common Language Runtime (CLR)** es un entorno de máquina virtual y parte de .NET Framework. Contiene:

- Un lenguaje de código de bytes portátil llamado **Lenguaje intermedio común** (abreviado CIL o IL)
- Un compilador Just-In-Time que genera código de máquina
- Un recolector de basura de seguimiento que proporciona gestión de memoria automática
- Soporte para subprocesos ligeros llamados AppDomains
- Mecanismos de seguridad a través de los conceptos de código verificable y niveles de confianza

El código que se ejecuta en CLR se denomina *código administrado* para distinguirlo del código que se ejecuta fuera de CLR (generalmente código nativo), que se conoce como *código no administrado*. Existen varios mecanismos que facilitan la interoperabilidad entre código administrado y no administrado.

