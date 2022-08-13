---
title: "Check if a file exists"
slug: "check-if-a-file-exists"
draft: false
images: []
weight: 9715
type: docs
toc: true
---

## Using loop control to run a dataflow task for every file.
If you want to check the existence of one file or do a couple of actions for every file in a folder you can use the Foreach Loop Container.

You give the path and the file mask and it will run it for every file it finds

[![For each file enumerator][1]][1]


  [1]: http://i.stack.imgur.com/ryD8u.jpg

## Steps to check if a file exist or not
To complete this objective following tasks are required.
1. **Foreach Loop Container:** To iterate over a user configured directory for files.
2. **Expression Task:** To update a variable if file exists.

**Steps**
1. First goto **Solution Explorer** double click on **Project.params** and create a parameter *FolderPath* of type string, put value like E:\DataDir\SourceFiles\.
2. Create user variables *FileNameFromFolder* (String), *FileToSearch* (String) assign value that you want to check and create a variable *IsFound* (Boolean).
3. Drag and drop a *Foreach Loop Container* from the SSIS Toolbox under Containers section.
4. Double click on the *Foreach Loop Container* on the left hand side of Foreach Loop Editor click on the **Collection**. On the right side set Enumerator as **Foreach File Enumerator**, now for the Expression click on the three dots which will open a Property Expression Editor. Select Directory as property and for expression select *@[$Project::FolderPath]*. Click OK.
[![Foreach Loop Editor Expression Property][1]][1]
5. Now in Foreach Loop Editor for the value of Files set ***.txt**, for the value of *Retrieve file name* select Name only, normally we select Fully Qualified as it returns file name with the complete path. Check the *Traverse subfolders* if there can be more than one folder inside a folder.
[![Foreach properties][2]][2]
6. On the left select **Variable Mappings**, on the right side select *User::FileNameFromFolder* which will automatically get Index as 0. The file names from the *FolderPath* will be assigned one by one to the *FileNameFromFolder* variable. Click OK.
[![Foreach Loop Variable Mappings][3]][3]
7. Drag and drop a *Expression Task* inside the *Foreach Loop Container* from the SSIS toolbox present under the section **Common**.     
8. Double click on the *Expression Task*, in the Expression Builder write following code. Click OK.

    @[User::IsFound] =  @[User::FileNameFromFolder] == @[User::FileToSearch] ? TRUE : FALSE
[![enter image description here][4]][4]
9. The Code above compares the file name that we want to check with the file name from the folder, if both are matched it sets *IsFound* to True (File Exists).
10. Now the value of IsFound can be used with precedence constraint according to need.      


  [1]: https://i.stack.imgur.com/03MIC.png
  [2]: https://i.stack.imgur.com/o813I.png
  [3]: https://i.stack.imgur.com/0IfmM.png
  [4]: https://i.stack.imgur.com/IVgX6.png

