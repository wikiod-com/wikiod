---
title: "Copy a file, byte for byte"
slug: "copy-a-file-byte-for-byte"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

If you're using SAS to produce reporting of some sort, you're going to find yourself needing to copy a file at some point. I've mostly used this method for copying an excel template, and then dumping data via PROC EXPORT into the new file I've created. 

This is a great example I've found from Chris Hemedinger (http://blogs.sas.com/content/sasdummy/2011/06/17/how-to-use-sas-data-step-to-copy-a-file-from-anywhere/).




## Copying any file, byte by byte


    /* these IN and OUT filerefs can point to anything */
    filename in "anyfilehere.xlsx"; 
    filename out "anyfilehere.xlsx"; 
    
    
    /* copy the file byte-for-byte  */
    data _null_;
      length filein 8 fileid 8;
      filein = fopen('in','I',1,'B');
      fileid = fopen('out','O',1,'B');
      rec = '20'x;
      do while(fread(filein)=0);
         rc = fget(filein,rec,1);
         rc = fput(fileid, rec);
    
         rc =fwrite(fileid);
      end;
      rc = fclose(filein);
      rc = fclose(fileid);
    run;
     
    filename in clear;
    filename out clear;

