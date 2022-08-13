---
title: "Add Citation"
slug: "add-citation"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Add citation to already existing LaTeX document
At the end of the document add the following: 

**\bibliographystyle{*style*}**

**\bibliography{*file location*}**

Create a file with extension *.bib* and save the citation as follows: 

    @inproceedings{citation_name,
      title={Paper Title},
      author={List Authors},
      pages={45--48},
      year={2013},
      organization={organization name}
    }

 
To cite use the following: **\citet{*citation_name*}**
 




