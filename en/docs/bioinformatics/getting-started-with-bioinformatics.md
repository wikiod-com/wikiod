---
title: "Getting started with bioinformatics"
slug: "getting-started-with-bioinformatics"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Using mapping of DNA sequences to answer biological questions
Many biological questions can be translated into a DNA sequencing problem. For instance, if you want to know the expression level of a gene you can: copy its mRNAs into complementary DNA molecules, sequence each of the resulting DNA molecules, map those sequences back to the reference genome, and then use the count of alignments overlapping the gene as a proxy of its expression (see [RNA-seq][1]). Other examples include: determining the [3D structure of the genome][2], locating [histone marks][3], and mapping [RNA-DNA interactions][4]. A not up-to-date list of biological questions addressed by clever DNA-sequencing methods can be found [here][5]. 

Typically, the wet-lab scientists (the people wearing white coats and goggles) will design and perform the experiments to get the sequenced DNA samples. Then, a bioinformatician (the people using computers and drinking coffee) will take these sequences --encoded as [FASTQ files][6]-- and will map them to a reference genome, saving the results as [BAM files][7].

Going back to our gene expression example, this is how a bioinformatician would generate a BAM file from a FASTQ file (using a Linux system):

    STAR --genomeDir path/to/reference/genome --outSAMtype BAM --readFilesIn my_reads.fastq

Where [STAR][8] is a spliced-tolerant aligner (necessary for the exon-intron junctions that may be present on the mRNA).

PS: Once the mapping results are obtained, the creative part begins. Here is where bioinformaticians devised statistical test to check whether the data is showing biologically meaningful patterns or spurious signals born out of noise.


  [1]: http://www.nature.com/nmeth/journal/v5/n7/full/nmeth.1226.html
  [2]: http://science.sciencemag.org/content/326/5950/289
  [3]: http://science.sciencemag.org/content/316/5830/1497
  [4]: http://dx.doi.org/10.1016/j.cub.2017.01.011
  [5]: https://liorpachter.wordpress.com/seq/
  [6]: https://en.wikipedia.org/wiki/FASTQ_format
  [7]: http://software.broadinstitute.org/software/igv/bam
  [8]: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4631051/

## .GFF file parser (as buffer) with filter to keep only rows
    
    """ A [GFF parser script][1] in Python for [www.VigiLab.org][2]
    
        Description: 
            - That performs buffered reading, and filtering (see: @filter) of .GFF input file (e.g. "[./toy.gff][3]") to keep only rows whose field (column) values are equal to "transcript"...
        
        Args:
            - None (yet)

        Returns:
            - None (yet)

        Related:
            - [1]: https://github.com/a1ultima/vigilab_intergeneShareGFF/blob/master/README.md
            - [2]: http://www.vigilab.org/
            - [3]: https://github.com/a1ultima/vigilab_intergeneShareGFF/blob/master/toy.gff
    
    """
    gene_to_field = {}  # dict whose keys: genes represented (i.e. later slice-able/index-able) as 1..n, values, where n = 8 total #fields (cols) of a gff row, whose version is unknown but example is: https://github.com/a1ultima/vigilab_intergeneShareGFF/blob/master/toy.gff

    gene_i = 0

    with open("./toy.gff", "r") as fi:

        print("Reading GFF file into: gene_to_field (dict), index as such: gene_to_field[gene_i], where gene_i is between 1-to-n...")
    
    while True: # breaks once there are no more lines in the input .gff file, see "@break"

        line = fi.readline().rstrip() # no need for trailing newline chars ("\n")

        if line == "":  # @break
            break
        
        line_split = line.split("\t") # turn a line of input data into a list, each element = different field value, e.g. [...,"transcript",...]

        if line_split[2] != "transcript": # @@filter incoming rows so only those with "transcript" are not skipped by "continue"
            continue

        gene_i += 1  # indexing starts from 1 (i.e. [1] = first gene) ends at n

        ##@TEST: sometimes 4.00 instead of 4.0 (trivial)   # some @deprecated code, but may be useful one day
        #if not (str(line_split[5])==str(float(line_split[5]))):
        #    print("oops")
        #    print("\t"+str(line_split[5])+"___"+str(float(line_split[5])))


        # create a dict key, for gene_to_field dict, and set its values according to list elements in line_split

        gene_to_field[gene_i] = { \
            "c1_reference_seq":line_split[0],# e.g. 'scaffold_150' \
            "c2_source":line_split[1],# e.g. 'GWSUNI' \
            "c3_type":line_split[2],# e.g. 'transcript' \
            "c4_start":int(line_split[3]),# e.g. '1372' \
            "c5_end":int(line_split[4]),# e.g. '2031' \
            "c6_score":float(line_split[5]),# e.g. '45.89' \
            "c7_strand":line_split[6],# e.g. '+' \
            "c8_phase":line_split[7],# e.g. '.' @Note: codon frame (0,1,2) \
            "c9_attributes":line_split[8]# e.g. <see @gff3.md> \
        }


  

## Definition
> (Wikipedia) Bioinformatics  is an interdisciplinary field that develops methods and software tools for understanding biological data. As an interdisciplinary field of science, bioinformatics combines computer science, statistics, mathematics, and engineering to analyze and interpret biological data. Bioinformatics has been used for in silico analyses of biological queries using mathematical and statistical techniques.

