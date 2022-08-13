---
title: "RESTlet - Process external documents"
slug: "restlet---process-external-documents"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

When retrieving a document from an external system, it requires us to ensure the correct document extension is affixed to the document. The sample code shows how to store a document properly in NetSuite's File Cabinet as well as attaching it to its corresponding record. 

## RESTlet - store and attach file
     /**
     * data - passed in object
     * switch - get file extension if there is one
     * nlapiCreateFile - create file in File Cabinet 
     * nlapiAttachRecord - attach file to record
     */
    
    function StoreAttachFile(data)
    {
        var record_type = data.recordType
        var record_id = data.recordId;
    
        if(record_id && record_type == 'vendorbill')
        {
            try
            {
                var file_type = data.fileType;
                var file_extension;
    
                switch (file_type)
                {
                case "pdf":
                    file_extension = "pdf";
                    break;
                case "docx":
                    file_extension = "doc";
                    break;
                case "txt":
                    file_extension = "txt";
                    break;
                case "JPGIMAGE":
                    file_extension = "jpg";
                    break;
                case "png":
                    file_extension = "png";
                    break;
                default:
                    // unknown type
                    // there should probably be some error-handling
                }
    
                var file_name = data.fileName + "." + file_extension;
                var file = data.fileContent;
                
                var doc = nlapiCreateFile(file_name, file_type, file);
                doc.setFolder(115);//Get Folder ID from: Documents > File > File Cabinet
                
                var file_id = nlapiSubmitFile(doc);
                
                nlapiAttachRecord("file", file_id, record_type, record_id);
                nlapiLogExecution('DEBUG', 'after submit', file_id);
            }
    
            catch (err)
            {
                var errMessage = err;
                if(err instanceof nlobjError)
                {
                    errMessage = errMessage + ' ' + err.getDetails();
                }
                nlapiLogExecution('DEBUG', 'Error', errMessage)
            }
        }
        return true;
    }

