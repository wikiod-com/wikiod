---
title: "Common File Formats"
slug: "common-file-formats"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## FASTA 
The FASTA file format is used for representing one or more nucleotide or amino acid sequences as a continuous string of characters.  Sequences are annotated with a comment line, which starts with the `>` character, that precedes each sequence.  The comment line is typically formatted in a uniform way, dictated by the sequence's source database or generating software.  For example:

    >gi|62241013|ref|NP_001014431.1| RAC-alpha serine/threonine-protein kinase [Homo sapiens]
    MSDVAIVKEGWLHKRGEYIKTWRPRYFLLKNDGTFIGYKERPQDVDQREAPLNNFSVAQCQLMKTERPRP
    NTFIIRCLQWTTVIERTFHVETPEEREEWTTAIQTVADGLKKQEEEEMDFRSGSPSDNSGAEEMEVSLAK
    PKHRVTMNEFEYLKLLGKGTFGKVILVKEKATGRYYAMKILKKEVIVAKDEVAHTLTENRVLQNSRHPFL
    TALKYSFQTHDRLCFVMEYANGGELFFHLSRERVFSEDRARFYGAEIVSALDYLHSEKNVVYRDLKLENL
    MLDKDGHIKITDFGLCKEGIKDGATMKTFCGTPEYLAPEVLEDNDYGRAVDWWGLGVVMYEMMCGRLPFY
    NQDHEKLFELILMEEIRFPRTLGPEAKSLLSGLLKKDPKQRLGGGSEDAKEIMQHRFFAGIVWQHVYEKK
    LSPPFKPQVTSETDTRYFDEEFTAQMITITPPDQDDSMECVDSERRPHFPQFSYSASGTA

The above example illustrates the amino acid sequence of an isoform of the human AKT1 genes, as fetched from the NCBI protein database.  The header line specifies that this sequence may be identified with the [GI ID](http://www.ncbi.nlm.nih.gov/genbank/sequenceids/) `62241013` and the protein transcript ID `NP_001014431.1`.  This protein is named `RAC-alpha serine/threonine-protein kinase` and is derived from the species, `Homo sapiens`.

## Mutation Annotation Format (MAF)
The [MAF file format](https://wiki.nci.nih.gov/display/TCGA/Mutation+Annotation+Format+(MAF)+Specification) is a tab-delimited text file format intended for describing somatic DNA mutations detected in sequencing results, and is distinct from the Multiple Alignment Format file type, which is intended for representing aligned nucleotide sequences.  Column headers and ordering may sometimes vary between files of different sources, but the names and orders of columns, as defined in the specification, are the following:

    Hugo_Symbol
    Entrez_Gene_Id
    Center
    NCBI_Build
    Chromosome
    Start_Position
    End_Position
    Strand
    Variant_Classification
    Variant_Type
    Reference_Allele
    Tumor_Seq_Allele1
    Tumor_Seq_Allele2
    dbSNP_RS
    dbSNP_Val_Status
    Tumor_Sample_Barcode
    Matched_Norm_Sample_Barcode
    Match_Norm_Seq_Allele1
    Match_Norm_Seq_Allele2
    Tumor_Validation_Allele1
    Tumor_Validation_Allele2
    Match_Norm_Validation_Allele1
    Match_Norm_Validation_Allele2
    Verification_Status4
    Validation_Status4
    Mutation_Status
    Sequencing_Phase
    Sequence_Source
    Validation_Method
    Score
    BAM_File
    Sequencer
    Tumor_Sample_UUID
    Matched_Norm_Sample_UUID

Many MAF files, such as those available from the TCGA, also contain additional columns expanding on the variant annotation.  These columns can include reference nucleotide transcript IDs for corresponding genes, representative codon or amino acid changes, QC metrics, population statistics, and more.

## GCT
The [GCT file format](http://www.broadinstitute.org/cancer/software/genepattern/file-formats-guide#GCT) is a tab-delimited text file format used for describing processed gene expression or RNAi data, typically derived from microarray chip analysis.  This data is arranged with a single annotated gene or probe per line, and a single chip sample per column (beyond the annotation columns).  For example:

    #1.2            
    22215    2        
    Name    Description    Tumor_One    Normal_One
    1007_s_at    DDR1    -0.214548    -0.18069
    1053_at    RFC2    0.868853    -1.330921
    117_at    HSPA6    1.124814    0.933021
    121_at    PAX8    -0.825381    0.102078
    1255_g_at    GUCA1A    -0.734896    -0.184104
    1294_at    UBE1L    -0.366741    -1.209838

In this example, the first line specifies the version of the GCT file specification, which in this case is `1.2`.  The second line specifies the number of rows of data (`22215`) and the number of samples (`2`).  The header row specifies two annotation columns (`Name` for the chip probe set identifiers and `Description` for the gene symbols the probe set covers) and the names of the samples being assayed (`Tumor_One` and `Normal_One`).  Each row of data beyond the header lists a single probe set identifier (in this case, Affymetrix gene chip probe sets), its corresponding gene symbol (if one exists), and the normalized values for each sample.  Sample data values will vary based upon assay type and normalization methods, but are typically signed floating point numeric values.

## Sequence Writing In fasta Format
This a python example function for sequence writing in fasta format. 

**Parameters:**

 - filename(String) - A file name for writing sequence in fasta format.
 - seq(String) - A DNA or RNA sequence.
 - id(String) - The ID of the given sequence.
 - desc(String) - A short description of the given sequence.


    import math

        def save_fsta(filename,seq,id,desc):
            fo = open(filename+'.fa',"a")
            header= str(id)+' <'+desc+'> \n'
            fo.write(header)
            count=math.floor(len(seq)/80+1)
            iteration = range(count)
            for i in iteration:
                fo.write(seq[80*(i):80*(i+1)]+'\n')        
            fo.write('\n \n')
            fo.close()

Another way is using `textwrap`


    import textwrap
    
    def save_fasta(filename,seq, id, desc):
        filename+='.fa'
        with open(filename, 'w') as f:
            f.write('>'+id+' <'+desc+'>\n');
            text = textwrap.wrap(seq,80);
            for x in text:
                f.write(x+'\n');

