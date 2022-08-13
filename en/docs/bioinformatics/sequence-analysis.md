---
title: "Sequence analysis"
slug: "sequence-analysis"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Calculate the GC% of a sequence
In molecular biology and genetics, GC-content (or guanine-cytosine content, GC% in short) is the percentage of nitrogenous bases on a DNA molecule that are either guanine or cytosine (from a possibility of four different ones, also including adenine and thymine).

**Using BioPython:**

    >>> from Bio.Seq import Seq
    >>> from Bio.Alphabet import IUPAC
    >>> from Bio.SeqUtils import GC
    >>> my_seq = Seq('GATCGATGGGCCTATATAGGATCGAAAATCGC', IUPAC.unambiguous_dna)
    >>> GC(my_seq)
    46.875

**Using BioRuby:**

    bioruby> require 'bio'
    bioruby> seq = Bio::Sequence::NA.new("atgcatgcaaaa")
    ==> "atgcatgcaaaa"
    bioruby> seq.gc_percent 

    ==> 33

**Using R:**

    # Load the SeqinR package.
    library("seqinr")
    mysequence <- s2c("atgcatgcaaaa")
    GC(mysequence)

    # [1] 0.3333333

**Using Awk:**


    echo atgcatgcaaaa |\
    awk '{dna=$0;  gsub(/[^GCSgcs]/,""); print dna,": GC=",length($0)/length(dna)}'

    # atgcatgcaaaa : GC= 0.333333



