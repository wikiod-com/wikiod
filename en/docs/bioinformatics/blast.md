---
title: "BLAST"
slug: "blast"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Create a DNA blastdb
In order to compare query sequences against reference sequences, you must create a blastdb of your reference(s).  This is done using `makeblastdb` which is included when you install blast.
````
makeblastdb -in <input fasta> -dbtype nucl -out <label for database>
````
So if you had a file `reference.fasta` containing the following records:

```
>reference_1
ATCGATAAA
>reference_2
ATCGATCCC
```
You would run the following:
```
makeblastdb -in reference.fasta -dbtype nucl -out my_database
```
This would create the following files:

* `my_database.nhr`
* `my_database.nin`
* `my_database.nsq`

Note, the database files are labelled with the `-out` argument.

## Extract fasta sequences from a nucl blastdb
You can extract fasta sequence from a blastdb constructed from a fasta file using `blastdbcmd` which should be installed when you install `makeblastdb`.

```
blastdbcmd -entry all -db <database label> -out <outfile>
```

If you had a database called `my_database` which contained the files `my_database.nhr`, `my_database.nsq`, `my_database.nin` and you wanted your fasta output file to be called `reference.fasta` you would run the following:

```
blastdbcmd -entry all -db my_database -out reference.fasta
```


## Install blast on ubuntu
```
apt-get install ncbi-blast+
```
You can check the version that will be installed in advance here:

http://packages.ubuntu.com/xenial/ncbi-blast+

## Extract GI and taxid from blastdb
Data can be extracted from a blastdb using ``blastdbcmd`` which should be included in a blast installation.  You can specify from the options below as part of ``-outfmt`` what metadata to include and in what order.

From the man page:

     -outfmt <String>
       Output format, where the available format specifiers are:
           %f means sequence in FASTA format
           %s means sequence data (without defline)
           %a means accession
           %g means gi
           %o means ordinal id (OID)
           %i means sequence id
           %t means sequence title
           %l means sequence length
           %h means sequence hash value
           %T means taxid
           %X means leaf-node taxids
           %e means membership integer
           %L means common taxonomic name
           %C means common taxonomic names for leaf-node taxids
           %S means scientific name
           %N means scientific names for leaf-node taxids
           %B means BLAST name
           %K means taxonomic super kingdom
           %P means PIG

The example snippet shows how gi and taxid can be extracted from blastdb.  The [NCBI 16SMicrobial][1] (ftp) blastdb was chosen for this example:

    # Example:
    # blastdbcmd -db <db label> -entry all -outfmt "%g %T" -out <outfile>
    blastdbcmd -db 16SMicrobial -entry all -outfmt "%g %T" -out 16SMicrobial.gi_taxid.tsv

Which will produce a file ``16SMicrobial.gi_taxid.tsv`` that looks like this:
    
    939733319 526714
    636559958 429001
    645319546 629680


  [1]: ftp://ftp.ncbi.nih.gov/blast/db/16SMicrobial.tar.gz

