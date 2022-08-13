---
title: "Macro security and signing of VBA-projects-modules"
slug: "macro-security-and-signing-of-vba-projects-modules"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Create a valid digital self-signed certificate SELFCERT.EXE
To run macros and maintain the security Office applications provide against malicious code, it is necessary to digitally sign the VBAProject.OTM from the *VBA editor > Tools > Digital Signature*.

[![enter image description here][1]][1]


Office comes with a utility to create a self-signed digital certificate that you can employ on the PC to sign your projects. 

This utility **SELFCERT.EXE** is in the Office program folder, 

Click on Digital Certificate for VBA Projects to open the certificate *wizard*. 

In the dialog enter a suitable name for the certificate and click OK.

[![enter image description here][2]][2]

If all goes well you will see a confirmation:

[![enter image description here][3]][3]

You can now close the **SELFCERT** wizard and turn your attention to the certificate you have created.

If you try to employ the certificate you have just created and you check its properties

[![enter image description here][4]][4]

[![enter image description here][5]][5]

You will see that the certificate is not trusted and the reason is indicated in the dialog.

The certificate has been created in the Current User > Personal > Certificates store. It needs to go in Local Computer > Trusted Root Certificate Authorities > Certificates store, so you need to export from the former and import to the latter.


Pressing the Windows <kbd> **Key+R** </kbd> which will open the 'Run' Window. then Enter 'mmc' in the window as shown below and click 'OK '.

[![enter image description here][6]][6]

The Microsoft Management Console will open and look like the following.

[![enter image description here][7]][7]


From the File menu, select Add/Remove Snap-in... Then from the ensuing dialog, double click Certificates and then click OK


[![enter image description here][8]][8]


Expand the dropdown in the left window for *Certificates - Current User*' and select certificates as shown below. The center panel will then show the certificates in that location, which will include the certificate you created earlier:

[![enter image description here][9]][9]

Right click the certificate and select All Tasks > Export:

[![enter image description here][10]][10]

Export Wizard

[![enter image description here][11]][11]

Click Next

[![enter image description here][12]][12]

the Only one pre-selected option will be available, so click 'Next' again:

[![enter image description here][13]][13]

The top item will already be pre-selected. Click Next again and choose a name and location to save the exported certificate.

[![enter image description here][14]][14]

Click Next again to save the certificate

Once focus is returned to the Management Console.

Expand the *Certificates* menu and from the Trusted Root Certification Authorities menu, select *Certificates*.

[![enter image description here][15]][15]

Right click. Select *All Tasks* and *Import*

[![enter image description here][16]][16]

[![enter image description here][17]][17]

Click next and Save to the *Trusted Root Certification Authorities store*:

[![enter image description here][18]][18]

Then Next > Finish,  now close the Console.

If you now use the certificate and check its properties, you will see that it is a trusted certificate and you can use it to sign your project:

[![enter image description here][19]][19]


  [1]: https://i.stack.imgur.com/FjwVD.png
  [2]: https://i.stack.imgur.com/tbaZ5.png
  [3]: https://i.stack.imgur.com/XVtmg.png
  [4]: https://i.stack.imgur.com/2zTPg.png
  [5]: https://i.stack.imgur.com/ZJiw2.png
  [6]: https://i.stack.imgur.com/gIi1B.png
  [7]: https://i.stack.imgur.com/WwrQL.png
  [8]: https://i.stack.imgur.com/K0dW0.png
  [9]: https://i.stack.imgur.com/xUFK4.png
  [10]: https://i.stack.imgur.com/Rp7Rd.png
  [11]: https://i.stack.imgur.com/VlTfq.png
  [12]: https://i.stack.imgur.com/Fa4jb.png
  [13]: https://i.stack.imgur.com/tVRfM.png
  [14]: https://i.stack.imgur.com/6xmub.png
  [15]: https://i.stack.imgur.com/ymFx2.png
  [16]: https://i.stack.imgur.com/vYVwj.png
  [17]: https://i.stack.imgur.com/Y5Lp5.png
  [18]: https://i.stack.imgur.com/o0avf.png
  [19]: https://i.stack.imgur.com/DlxuT.png

