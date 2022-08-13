---
title: "Installing Web Deploy"
slug: "installing-web-deploy"
draft: false
images: []
weight: 9850
type: docs
toc: true
---

Installing Web Deploy allows quick installation of web applications directly from the development environment using Visual Studio. 

Installation types:

online - target Windows server has Internet access - installation is simple using Installing and configuring Web deployment on servers with Internet connection example below

offline - target Windows server is offline - installation is rather complex because several components and configuration must be done manually (or scripted)

## Installing WebDeploy on IIS with ASP.NET
1. Install WebServer Role
2. Install WebDeploy 3.6 from [MSDN][1]
2. Activate **ASP.NET 4.6** under *Web Server (IIS)* > *Web Server* > *Application Development* 

  [1]: https://www.iis.net/downloads/microsoft/web-deploy

## Installing and configuring Web deployment on servers with Internet connection
In order to be able to directly deploy web project changes to an Web Server, the following steps must be followed. If target server has Internet access, the process is quite simple, as Microsoft has a Web Platform package to do almost everything that is needed.

1) **Cleanup**

Make sure that `C:\Program Files\IIS` does not contain older version of Microsoft Web Deploy. If it contains, uninstall it (them) from Add/remove programs and remove any files left

2) **Web platform installer**

Install Web platform installer (WPI) from [here][1]

3) **Web deploy installation**

Run WPI, search for Web Deploy 3.6 for Hosting Servers and install everything it contains. It will take care to install all the required dependencies (e.g. for SQL deployment) and also make IIS ready to be configured for Web deployments 

4) **IIS configuration**

IIS must be configured in order to allow web deployments on one or more of its Web sites. 

- **Rights**: access web site -> IIS manager permissions and configure which users are allowed to publish. **NOTE**: in some particular configuration, trying to select a user might lead to IIS manager crashing. In these rare cases, just enter username manually - e.g. domain\username

- **Web deploy Publishing configuration** - Right click on Web site -> Deploy... -> Configure Web Deploy Publishing 

**NOTE:** In order to for publishing to work, selected port (default is 8172) must be opened (it might be blocked in some companies).

Full installation details (step by step with relevant pictures) can be found [here][2]


  [1]: https://www.microsoft.com/web/downloads/platform.aspx?lang=
  [2]: https://www.iis.net/learn/install/installing-publishing-technologies/installing-and-configuring-web-deploy-on-iis-80-or-later

## Installing and configuring Web deployment on servers without Internet connection
Fully configuration of Web deployment without Internet connection (offline) is harder to make because Web platform installer (UI) operates by querying packages list and content from an Internet location.

1) **Cleanup**
Make sure that C:\Program Files\IIS does not contain older version of Microsoft Web Deploy. If it contains, uninstall it (them) from Add/remove programs and remove any files left

2) **Web Platform Installer v5 Command Line** (WebPICMD.exe) allows to export and install packages without the "online" requirement. Full installation and usage instructions can be found [here][1].

3) **Required packages for Web Deploy**

- install Web Deploy into an "online" machine
- export packages for it using WPI Command Line 


    C:\Program Files\Microsoft\Web Platform Installer>WebPICMD.exe /Offline /Product s:"WDeployPS" /Path:C:\OfflineCache

Dependencies list will be output:

    The software that you obtain using the Web Platform Installer Command Line Tool
    is licensed to you by its owner.  Microsoft grants you no rights for third part
     software.
    Loading products in online feeds ...
    
    Loading products in offline feeds...
    Creating offline cache for following products:
    WDeployPS
    netframework2
    WindowsInstaller31
    PowerShell2
    PowerShellMsu
    ManagementService
    IISManagementConsole
    WASConfigurationAPI
    NetFx4Extended-ASPNET45
    WASNetFxEnvironment
    NetFx3
    IIS7
    StaticContent
    WASProcessModel
    DefaultDocument
    DirectoryBrowse
    HTTPErrors
    HTTPLogging
    LoggingTools
    RequestMonitor
    RequestFiltering
    StaticContentCompression
    ASPNET
    NETExtensibility
    ISAPIExtensions
    ISAPIFilters
    WDeploy_3_5
    WDeployNoSMO
    WDeployNoSMO_3_5
    NetFx4
    WDeploy_Only_3_5
    NETFramework4
    WindowsImagingComponent
    IIS51
    IIS60
    NETFramework45
    VWD11_Only_BaseLocale
    VWD2012IncompatibleOSs
    VS11_Not_RTM_Block
    VWD11_RC_Below_Block
    SMO
    SMO_11_1
    SQLCLRTypes_11_1
    WindowsInstaller45
    SQLCLRTypes_Only_x86_11_1
    SQLCLRTypes_Only_x64_11_1
    SMO_11_1_Only_x86
    SMO_11_1_Only_x64
    SMO_10_5
    SQLNativeClient_10_5
    SQLCLRTypes_10_5
    SQLCLRTypes_x86_10_5
    SQLCLRTypes_x64_10_5
    SMO_Only_x86_10_5
    SMO_Only_x64_10_5
    WindowsVista_OrUp
    WindowsVista_Below
    DACFX_3_1
    SQLDOM_11_0
    SQLCLRTypes_11_0
    SQLCLRTypes_Only_x86_11_0
    SQLCLRTypes_Only_x64_11_0
    DACFX_X64_3_1
    DACFX_X86_3_1
    WindowsVista_Below_DACFX
    SMO_11_0
    SQLNativeClient_11_0
    SMO_Only_x64_11_0
    SMO_Only_x86_11_0

    ...

    All offline cache operations completed successfully.
    To use the new offline feed, please run the following from the command line:

    WebPiCmd.exe /Install /Products:<products you want> /XML:<Offline main feed>
    Done !

The list is much longer than the actual needed packages for a particular Windows Server because it exports for both x86 and x64 and assumes that nothing is installed.

One way is to use `/Install` option to install the packages, but a much faster way is to install **Microsoft Visual Studio Express 2012 for Web** from [here][2]. This will install all required prerequisites. Of course, Visual Studio itself is not required on target server and can be uninstalled.

Also, **Microsoft Web Deploy 3.5** can be upgraded to [Microsoft Web Deploy 3.6][3].

4) **IIS Management Service installation**

Install **Management Service** from Server Manager -> Local Server -> Server Roles -> Web Server (IIS) -> Management Tools -> Management Service

**NOTE:** sanity check before going to configuration

- check that **Web Deployment Agent Service** is running
- check that **Web Management Service** is running

5) **IIS configuration**
IIS must be configured in order to allow web deployments on one or more of its Web sites.

Rights: access web site -> IIS manager permissions and configure which users are allowed to publish. NOTE: in some particular configuration, trying to select a user might lead to IIS manager crashing. In these rare cases, just enter username manually - e.g. domain\username

Web deploy Publishing configuration - Right click on Web site -> Deploy... -> Configure Web Deploy Publishing

NOTE: In order for publishing to work, selected port (default is 8172) must be opened (it might be blocked in some companies).

Full installation details (step by step with relevant pictures) can be found [here][4]


  [1]: https://docs.microsoft.com/en-us/iis/install/web-platform-installer/web-platform-installer-v4-command-line-webpicmdexe-rtw-release
  [2]: https://www.microsoft.com/en-us/download/details.aspx?id=30669
  [3]: https://www.microsoft.com/en-us/download/details.aspx?id=43717
  [4]: https://www.iis.net/learn/install/installing-publishing-technologies/installing-and-configuring-web-deploy-on-iis-80-or-later

## Excluding Files and Folders from Web Deployment
Typically, some files from the Web Application should not be overwritten when performing the deployment (e.g. `web.config`). This can be accomplished by:

**1) Excluding from output** - which means setting **Build action** to **None**. This is the easiest way, but it might not work for some particular files or folders, that must be in the output for the application to run locally

**2) Excluding Files and Folders from a Web Package** by creating a special xml file in the Web application root folder. E.g.:

    File name = [project name].wpp.targets
    File content = 
        
    <Project ToolsVersion="4.0" 
             xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <ItemGroup>   
        <!-- this will exclude all files from Internal folder -->
        <ExcludeFromPackageFolders Include="Internal">
          <FromTarget>ContactManager.Mvc.wpp.targets</FromTarget>
        </ExcludeFromPackageFolders>

        <!-- this will exclude specified files -->
        <ExcludeFromPackageFiles Include="Scripts\jquery-1.4.4-
    vsdoc.js;Scripts\jquery-1.4.4.js;Scripts\jquery-ui.js;Scripts\jquery.unobtrusive-ajax.js;Scripts\jquery.validate-vsdoc.js;Scripts\jquery.validate.js;Scripts\jquery.validate.unobtrusive.js;Scripts\MicrosoftAjax.debug.js;Scripts\MicrosoftMvcValidation.debug.js">
          <FromTarget>ContactManager.Mvc.wpp.targets</FromTarget>
        </ExcludeFromPackageFiles>
      </ItemGroup>
    </Project>

More details can be found [here][1].


  [1]: https://www.asp.net/web-forms/overview/deployment/advanced-enterprise-web-deployment/excluding-files-and-folders-from-deployment

## Web deployment automatic backups
Web deployment offers the option to automatically backup target Web site (not target Web application!) on deployment. This is recommended to allow web application rollback.

In order to configure automatic backups, the following steps must be followed:

**1) Enable backups**

Open `%programfiles%\IIS\Microsoft Web Deploy V3\scripts\BackupScripts.ps1` in Powershell 

Execute the following commands:

    # Turns on all backup functionality
    TurnOn-Backups -On $true
    
    # Turns off all backup functionality
    TurnOn-Backups -On $false
    
    # Changes default global backup behavior to enabled
    Configure-Backups -Enabled $true
    
    # Changes default backup behavior for site "foo" to enabled
    Configure-Backups -SiteName "foo" -Enabled $true
    
    # Changes the path of where backups are stored to a sibling directory named "siteName_snapshots".  
    # For more information about path variables, see the "backupPath" attribute in the section 
    # "Configuring  Backup Settings on the Server for Global usage manually in IIS Config"
    Configure-Backups -BackupPath "{SitePathParent}\{siteName}_snapshots"
    
    # Configures default backup limit to 5 backups
    Configure-Backups -NumberOfBackups 5
    
    # Configures sync behavior to fail if a sync fails for any reason
    Configure-Backups -ContinueSyncOnBackupFailure $false
    
    # Adds providers to skip when performing a backup
    Configure-Backups -AddExcludedProviders @("dbmysql","dbfullsql")
    
    # Allows a site administrator to enable backups and set the number of backups at the site level
    Configure-BackupSettingsProvider -CanSetEnabled $true -CanSetNumBackups $true
    
    # Allows a site administrator to control which providers they want to skip in a backup, as 
    # well as whether they can continue a sync after a backup failure
    Configure-BackupSettingsProvider -CanSetContinueSyncOnBackupFailure $true -CanAddExcludedProviders $true

**2) Check backup settings** on global or site level

    Get-BackupSettings
    Get-BackupSettings -SiteName "Default Web Site"

**3) Further backup customization**

Backup settings can be configured for each web site. Open `applicationHost.config` and add backup settings for its specific location:

    <location path="siteName">
        <system.webServer>
            <wdeploy>
                <backup enabled="true" numberOfBackups="4">
                    <excludedProviders>
                        <clear />
                        <provider name="dbfullsql" />
                    </excludedProviders>
                </backup>
            </wdeploy>
        </system.webServer>
    </location>

For security related information and other information related to command line usage, access [this article][1].


  [1]: https://www.iis.net/learn/publish/using-web-deploy/web-deploy-automatic-backups

