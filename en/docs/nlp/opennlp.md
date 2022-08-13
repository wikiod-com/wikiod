---
title: "OpenNLP"
slug: "opennlp"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Syntax
- opennlp SentenceDetector ./en-sent.bin < ./input.txt > output.txt
-  Initialize SentenceDetectorME like this: SentenceDetectorME sentenceDetector = new SentenceDetectorME(model);

- Use ‘sentDetect’ method to get sentences like this: String sentences[] = sentenceDetector.sentDetect("string of information");







download models(like en-sent.bin) from the following [link](http://opennlp.sourceforge.net/models-1.5/)

## Sentence Detection using openNLP using CLI and Java API
 ***using CLI:***  

    $ opennlp SentenceDetector ./en-sent.bin < ./input.txt > output.txt

***using API:***

    import static java.nio.file.Files.readAllBytes;
    import static java.nio.file.Paths.get;

    import java.io.IOException;
    import java.util.Objects;

    public class FileUtils {
    /**
     * Get file data as string
     * 
     * @param fileName
     * @return
     */
        public static String getFileDataAsString(String fileName) {
            Objects.nonNull(fileName);
            try {
                String data = new String(readAllBytes(get(fileName)));
                return data;
            } catch (IOException e) {
                System.out.println(e.getMessage());
                return null;
            }
        }
    }

class sentecedetectorutil:

    
    import java.io.FileInputStream;
    import java.io.FileNotFoundException;
    import java.io.IOException;
    import java.io.InputStream;
    import java.util.Objects;
    
    import opennlp.tools.sentdetect.SentenceDetectorME;
    import opennlp.tools.sentdetect.SentenceModel;
    
    public class SentenceDetectorUtil {
        private SentenceModel model = null;
        SentenceDetectorME sentenceDetector = null;
    
        public SentenceDetectorUtil(String modelFile) {
            Objects.nonNull(modelFile);
            initSentenceModel(modelFile);
            initSentenceDetectorME();
        }
    
        private void initSentenceDetectorME() {
            sentenceDetector = new SentenceDetectorME(model);
        }
    
        private SentenceModel initSentenceModel(String file) {
            InputStream modelIn;
            try {
                modelIn = new FileInputStream(file);
            } catch (FileNotFoundException e) {
                System.out.println(e.getMessage());
                return null;
            }
    
            try {
                model = new SentenceModel(modelIn);
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                if (modelIn != null) {
                    try {
                        modelIn.close();
                    } catch (IOException e) {
                    }
                }
            }
            return model;
        }
    
        public String[] getSentencesFromFile(String inputFile) {
            String data = FileUtils.getFileDataAsString(inputFile);
            return sentenceDetector.sentDetect(data);
        }
    
        public String[] getSentences(String data) {
            return sentenceDetector.sentDetect(data);
        }
    
    }
    }

main class:

    public class Main {
     public static void main(String args[]) {
      SentenceDetectorUtil util = new SentenceDetectorUtil(
        "path//to//your//en-sent.bin");
    
      String data = "Welcome to Stackoverflow Documentation.This is the first example in OenNLP.";
    
      String[] sentences = util.getSentences(data);
    
      for (String s : sentences)
       System.out.println(s +"\n");
     }
    }

output will be:

> Welcome to Stackoverflow Documentation.
> 
> This is the first example in OpenNLP.



