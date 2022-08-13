---
title: "Bootstrap Containers"
slug: "bootstrap-containers"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Use .container for a responsive fixed width container.

Use .container-fluid for a full width container, spanning the entire width of the viewport.

## Containers
.container has one fixed width for each screen size in bootstrap (xs,sm,md,lg); 

.container-fluid expands to fill the available width.

    @media (min-width: 568px) {
      .container {
        width: 550px;
      }
    }
    @media (min-width: 992px) {
      .container {
        width: 970px;
      }
    }
    @media (min-width: 1200px) {
      .container {
        width: 1170px;
      }
    }

Depending on the width of the viewport that the webpage is being viewed on, the container class gives its div a specific fixed width. 

Your .container-fluid element, on the other hand, will constantly resize as you make even the smallest changes to your browser width.



