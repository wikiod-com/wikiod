---
title: "Getting started with wix"
slug: "getting-started-with-wix"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Download and install the WiX Toolset from [wixtoolset.org][1].

The installer for the WiX Toolset provides also the integration with [Visual Studio][2], after the installation you should be able to create WiX specific projects.

# Warnings

Admin rights are needed.

Some versions of WiX are compatible only with particular version of Visual Studio:

   - V3.11 and afterwards doesn't include the extensions for Visual Studio, you have to download the extensions for your versions of Visual Studio
   - V3.10 and below doesn't works with Visual Studio 2017 but includes the project templates for Visual Studio

# Details

The installer is build with WiX itself and present an unusual window:
[![WiX installer window][3]][3]

It is composed by 6 parts:
- WiX Toolset: show the version installed by the installer and launch the [Wix Toolset website][1] on click
- License: show the license
- Install: start the install
- Up To Date: check if a new version is available
- News: launch the [WiX news][4]
- Exit: close the installer

  [1]: http://wixtoolset.org
  [2]: http://wixtoolset.org/releases/
  [3]: https://i.stack.imgur.com/ztWjF.png
  [4]: http://wixtoolset.org/news/

## Simple Setup
This example assume that a solution with an application named `MyApp` already exists.

   - Add a new project to the solution:
[![Add Setup project][1]][1]


   - In the Setup project, add a new reference to `MyApp` from the Projects tab:
[![Add MyApp reference][2]][2]


   - In the `Product.wxs` file, valorize the `Manufacturer` attribute of the `Product` node with `HelloWorld`:

    <Product Id="*" Name="MyApp.Setup" Language="1033" Version="1.0.0.0" Manufacturer="HelloWorld" UpgradeCode="52f2c69b-5901-4d18-bb96-8c1c86cd1a3e">

In the `Fragment` node containing the `Directory` nodes, wrap the last with a new `Directory`:

    <Directory Id="ManufacturerFolder" Name="!(bind.property.Manufacturer)">
        <Directory Id="INSTALLFOLDER" Name="MyApp.Setup" />
    </Directory>

In the `ComponentGroup` node, uncomment the commented nodes and remove the `TODO` then add a `File` node in the `Component`:

    <File Source="$(var.MyApplication.TargetPath)" />

The Source attribute specifies where to find the file for packaging during the build. Rather than hard-code values for these attributes into our source code, we use the WiX preprocessor variables that are passed to the WiX compiler.

   - Build the WiX project.

That's it! Now you have a working installer that installs and uninstalls the application.

---

Full `Product.wxs` file:

    <?xml version="1.0" encoding="UTF-8"?>
    <Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">
        <Product Id="*" Name="MyApp.Setup" Language="1033" Version="1.0.0.0" Manufacturer="HelloWorld" UpgradeCode="52f2c69b-5901-4d18-bb96-8c1c86cd1a3e">
            <Package InstallerVersion="200" Compressed="yes" InstallScope="perMachine" />
    
            <MajorUpgrade DowngradeErrorMessage="A newer version of [ProductName] is already installed." />
            <MediaTemplate />
    
            <Feature Id="ProductFeature" Title="MyApp.Setup" Level="1">
                <ComponentGroupRef Id="ProductComponents" />
            </Feature>
        </Product>
    
        <Fragment>
            <Directory Id="TARGETDIR" Name="SourceDir">
                <Directory Id="ProgramFilesFolder">
                    <Directory Id="ManufacturerFolder" Name="!(bind.property.Manufacturer)">
                        <Directory Id="INSTALLFOLDER" Name="MyApp.Setup" />
                    </Directory>
                </Directory>
            </Directory>
        </Fragment>
    
        <Fragment>
            <ComponentGroup Id="ProductComponents" Directory="INSTALLFOLDER">
                <Component Id="ProductComponent">
                    <File Source="$(var.MyApp.TargetPath)" />
                </Component>
            </ComponentGroup>
        </Fragment>
    </Wix>

  [1]: https://i.stack.imgur.com/VGV5w.png
  [2]: https://i.stack.imgur.com/EyA1M.png

