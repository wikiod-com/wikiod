---
title: "Linearizing a fastq file"
slug: "linearizing-a-fastq-file"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Using Paste


```
$ gunzip -c input.fastq.gz | paste - - - - | head

@IL31_4368:1:1:996:8507/2    TCCCTTACCCCCAAGCTCCATACCCTCCTAATGCCCACACCTCTTACCTTAGGA    +    FFCEFFFEEFFFFFFFEFFEFFFEFCFC<EEFEFFFCEFF<;EEFF=FEE?FCE
@IL31_4368:1:1:996:21421/2    CAAAAACTTTCACTTTACCTGCCGGGTTTCCCAGTTTACATTCCACTGTTTGAC    +    >DBDDB,B9BAA4AAB7BB?7BBB=91;+*@;5<87+*=/*@@?9=73=.7)7*
@IL31_4368:1:1:997:10572/2    GATCTTCTGTGACTGGAAGAAAATGTGTTACATATTACATTTCTGTCCCCATTG    +    E?=EECE<EEEE98EEEEAEEBD??BE@AEAB><EEABCEEDEC<<EBDA=DEE
@IL31_4368:1:1:997:15684/2    CAGCCTCAGATTCAGCATTCTCAAATTCAGCTGCGGCTGAAACAGCAGCAGGAC    +    EEEEDEEE9EAEEDEEEEEEEEEECEEAAEEDEE<CD=D=*BCAC?;CB,<D@,
@IL31_4368:1:1:997:15249/2    AATGTTCTGAAACCTCTGAGAAAGCAAATATTTATTTTAATGAAAAATCCTTAT    +    EDEEC;EEE;EEE?EECE;7AEEEEEE07EECEA;D6D>+EE4E7EEE4;E=EA
@IL31_4368:1:1:997:6273/2    ACATTTACCAAGACCAAAGGAAACTTACCTTGCAAGAATTAGACAGTTCATTTG    +    EEAAFFFEEFEFCFAFFAFCCFFEFEF>EFFFFB?ABA@ECEE=<F@DE@DDF;
@IL31_4368:1:1:997:1657/2    CCCACCTCTCTCAATGTTTTCCATATGGCAGGGACTCAGCACAGGTGGATTAAT    +    A;0A?AA+@A<7A7019/<65,3A;'''07<A=<=>?7=?6&)'9('*%,>/(<
@IL31_4368:1:1:997:5609/2    TCACTATCAGAAACAGAATGTATAACTTCCAAATCAGTAGGAAACACAAGGAAA    +    AEECECBEC@A;AC=<AEEEEAEEEE>AC,CE?ECCE9EAEC4E:<C>AC@EE)
@IL31_4368:1:1:997:14262/2    TGTTTTTTCTTTTTCTTTTTTTTTTGACAGTGCAGAGATTTTTTATCTTTTTAA    +    97'<2<.64.?7/3(891?=(6??6+<6<++/*..3(:'/'9::''&(1<>.(,
@IL31_4368:1:1:998:19914/2    GAATGAAAGCAGAGACCCTGATCGAGCCCCAGAAAGATACACCTCCAGATTTTA    +    C?=CECE4CD<?8@==;EBE<=0@:@@92@???6<991>.<?A=@5?@99;971
```

## Using Awk
```
$ gunzip -c input.fastq.gz | awk '{printf("%s%s",$0,((NR+1)%4==1?"\n":"\t"));}' | head

@IL31_4368:1:1:996:8507/2    TCCCTTACCCCCAAGCTCCATACCCTCCTAATGCCCACACCTCTTACCTTAGGA    +    FFCEFFFEEFFFFFFFEFFEFFFEFCFC<EEFEFFFCEFF<;EEFF=FEE?FCE
@IL31_4368:1:1:996:21421/2    CAAAAACTTTCACTTTACCTGCCGGGTTTCCCAGTTTACATTCCACTGTTTGAC    +    >DBDDB,B9BAA4AAB7BB?7BBB=91;+*@;5<87+*=/*@@?9=73=.7)7*
@IL31_4368:1:1:997:10572/2    GATCTTCTGTGACTGGAAGAAAATGTGTTACATATTACATTTCTGTCCCCATTG    +    E?=EECE<EEEE98EEEEAEEBD??BE@AEAB><EEABCEEDEC<<EBDA=DEE
@IL31_4368:1:1:997:15684/2    CAGCCTCAGATTCAGCATTCTCAAATTCAGCTGCGGCTGAAACAGCAGCAGGAC    +    EEEEDEEE9EAEEDEEEEEEEEEECEEAAEEDEE<CD=D=*BCAC?;CB,<D@,
@IL31_4368:1:1:997:15249/2    AATGTTCTGAAACCTCTGAGAAAGCAAATATTTATTTTAATGAAAAATCCTTAT    +    EDEEC;EEE;EEE?EECE;7AEEEEEE07EECEA;D6D>+EE4E7EEE4;E=EA
@IL31_4368:1:1:997:6273/2    ACATTTACCAAGACCAAAGGAAACTTACCTTGCAAGAATTAGACAGTTCATTTG    +    EEAAFFFEEFEFCFAFFAFCCFFEFEF>EFFFFB?ABA@ECEE=<F@DE@DDF;
@IL31_4368:1:1:997:1657/2    CCCACCTCTCTCAATGTTTTCCATATGGCAGGGACTCAGCACAGGTGGATTAAT    +    A;0A?AA+@A<7A7019/<65,3A;'''07<A=<=>?7=?6&)'9('*%,>/(<
@IL31_4368:1:1:997:5609/2    TCACTATCAGAAACAGAATGTATAACTTCCAAATCAGTAGGAAACACAAGGAAA    +    AEECECBEC@A;AC=<AEEEEAEEEE>AC,CE?ECCE9EAEC4E:<C>AC@EE)
@IL31_4368:1:1:997:14262/2    TGTTTTTTCTTTTTCTTTTTTTTTTGACAGTGCAGAGATTTTTTATCTTTTTAA    +    97'<2<.64.?7/3(891?=(6??6+<6<++/*..3(:'/'9::''&(1<>.(,
@IL31_4368:1:1:998:19914/2    GAATGAAAGCAGAGACCCTGATCGAGCCCCAGAAAGATACACCTCCAGATTTTA    +    C?=CECE4CD<?8@==;EBE<=0@:@@92@???6<991>.<?A=@5?@99;971
```


```
