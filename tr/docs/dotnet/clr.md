---
title: "CLR"
slug: "clr"
draft: false
images: []
weight: 9937
type: docs
toc: true
---

## Common Language Runtime'a giriş
**Common Language Runtime (CLR)** bir sanal makine ortamıdır ve .NET Framework'ün bir parçasıdır. Bu içerir:

- **Common Intermediate Language** (kısaltılmış CIL veya IL) adlı taşınabilir bir bayt kodu dili
- Makine kodu üreten bir Just-In-Time derleyici
- Otomatik bellek yönetimi sağlayan bir izleme çöp toplayıcı
- AppDomains adı verilen hafif alt süreçler için destek
- Doğrulanabilir kod ve güven seviyeleri kavramları aracılığıyla güvenlik mekanizmaları

CLR'de çalışan kod, *yönetilen kod* olarak adlandırılan ve CLR dışında çalışan koddan (genellikle yerel kod) ayırt etmek için *yönetilen kod* olarak adlandırılır. Yönetilen ve yönetilmeyen kod arasında birlikte çalışabilirliği kolaylaştıran çeşitli mekanizmalar vardır.

