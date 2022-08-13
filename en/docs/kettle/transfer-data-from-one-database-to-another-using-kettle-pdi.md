---
title: "Transfer Data from one Database to another using Kettle (PDI)"
slug: "transfer-data-from-one-database-to-another-using-kettle-pdi"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

You can use Pentaho Data Integration (Kettle) to Transfer Data from One Database to another. Its very easy as the transformation is automatically created just by creating the connection.and it can transfer Lacs of records very quickly.


## Steps to Move One table of MySQL to another table in postgres:


Click on New Files --> Transformation
once a new transformation is open, Click on views tab ,under views create two connections (source ) and (destination). 

source: table where data is available
destination: table where you want to push your data.

[![screenshot of successful connection][1]][1]


Once done.

on the top bar you have **tools** --> Wizard --> Copy table

It will ask you to provide source and destination Connection details.

[![Provide Source and Destination][2]][2]

and Click on Finish.

Once done you will find a transformation Created with Source to destination mapping.

[![Screenshot of transformation][3]][3]


Just check once before you run the Kettles that table inside the database which you want to move is properly selected. 

[![Data Moved Successfully][4]][4]


  [1]: https://i.stack.imgur.com/X83kS.png
  [2]: https://i.stack.imgur.com/NNaDL.png
  [3]: https://i.stack.imgur.com/ijrjI.png
  [4]: https://i.stack.imgur.com/eErmh.png

as you can see in the above screenshot 720 Records are Moved Successfully from one table to another,where one table was MySQL and another table was in Postgres!!

