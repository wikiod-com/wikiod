---
title: "The DNN Manifest File for a Module"
slug: "the-dnn-manifest-file-for-a-module"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

This provides an overview of the Manifest file for DNN. Manifest files are the files necessary to install and register a module with the DNN framework. 

The manifest file can be utilized for extensions in DNN besides just Modules. You can have manifest files for Providers, Skins (Themes) and Containers

## The Manifest File
The Manifest file in DNN provides the framework with the information necessary to install and register a module. Here's a sample Manifest file for the dnnSimpleArticle module.

    <dotnetnuke type="Package" version="5.0">
      <packages>
        <package name="dnnsimplearticle" type="Module" version="00.02.03">
          <friendlyName>dnnsimplearticle</friendlyName>
          <description>An open source articles module for DotNetNuke, from Christoc.com</description>
          <iconFile>desktopmodules/dnnsimplearticle/i/SimpleArticleLogo.jpg</iconFile>
          <owner>
            <name>Chris Hammond</name>
            <organization>Christoc.com Software Solutions</organization>
            <url>http://www.christoc.com</url>
            <email>modules@christoc.com</email>
          </owner>
          <license src="License.txt"/>
          <releaseNotes src="ReleaseNotes.txt" />
    
          <dependencies>
            <dependency type="CoreVersion">08.00.00</dependency>
            <!-- .NET 4.5 -->
            <dependency type="type">System.Reflection.ReflectionContext</dependency>
          </dependencies>
    
          <components>
            <component type="Script">
              <scripts>
                <basePath>DesktopModules\dnnsimplearticle</basePath>
                <script type="Install">
                  <path>Providers\DataProviders\SqlDataProvider</path>
                  <name>00.00.01.SqlDataProvider</name>
                  <version>00.00.01</version>
                </script>
                <script type="UnInstall">
                  <path>Providers\DataProviders\SqlDataProvider</path>
                  <name>Uninstall.SqlDataProvider</name>
                  <version>00.00.01</version>
                </script>
                <script type="Install">
                  <path>Providers\DataProviders\SqlDataProvider</path>
                  <name>00.00.03.SqlDataProvider</name>
                  <version>00.00.03</version>
                </script>
                <script type="Install">
                  <path>Providers\DataProviders\SqlDataProvider</path>
                  <name>00.00.07.SqlDataProvider</name>
                  <version>00.00.07</version>
                </script>
                <script type="Install">
                  <path>Providers\DataProviders\SqlDataProvider</path>
                  <name>00.01.00.SqlDataProvider</name>
                  <version>00.01.00</version>
                </script>
              </scripts>
            </component>
    
            <component type="ResourceFile">
              <resourceFiles>
                <basePath>DesktopModules/dnnsimplearticle</basePath>
                <resourceFile>
                  <name>Resources.zip</name>
                </resourceFile>
              </resourceFiles>
            </component>
    
            <component type="Module">
              <desktopModule>
                <moduleName>dnnsimplearticle</moduleName>
                <foldername>dnnsimplearticle</foldername>
                <businessControllerClass>Christoc.Modules.dnnsimplearticle.Components.FeatureController</businessControllerClass>
                <supportedFeatures />
                <moduleDefinitions>
                  <moduleDefinition>
                    <friendlyName>dnnsimplearticle</friendlyName>
                    <defaultCacheTime>0</defaultCacheTime>
                    <moduleControls>
                      <moduleControl>
                        <controlKey />
                        <controlSrc>DesktopModules/dnnsimplearticle/View.ascx</controlSrc>
                        <supportsPartialRendering>False</supportsPartialRendering>
                        <controlTitle />
                        <controlType>View</controlType>
                        <iconFile />
                        <helpUrl />
                        <viewOrder>0</viewOrder>
                      </moduleControl>
                      <moduleControl>
                        <controlKey>Edit</controlKey>
                        <controlSrc>DesktopModules/dnnsimplearticle/Edit.ascx</controlSrc>
                        <supportsPartialRendering>False</supportsPartialRendering>
                        <controlTitle>Edit Content</controlTitle>
                        <controlType>Edit</controlType>
                        <iconFile />
                        <helpUrl />
                        <viewOrder>0</viewOrder>
                      </moduleControl>
                      <moduleControl>
                        <controlKey>Settings</controlKey>
                        <controlSrc>DesktopModules/dnnsimplearticle/Settings.ascx</controlSrc>
                        <supportsPartialRendering>False</supportsPartialRendering>
                        <controlTitle>dnnsimplearticle Settings</controlTitle>
                        <controlType>Edit</controlType>
                        <iconFile />
                        <helpUrl />
                        <viewOrder>0</viewOrder>
                      </moduleControl>
                    </moduleControls>
                  </moduleDefinition>
                </moduleDefinitions>
              </desktopModule>
            </component>
            <component type="Assembly">
              <assemblies>
                <basePath>bin</basePath>
                <assembly>
                  <name>dnnsimplearticle.dll</name>
                </assembly>
              </assemblies>
            </component>
    
            <component type="Config">
              <config>
                <configFile>web.config</configFile>
                <install>
                  <configuration>
                    <nodes>
                      <node path="/configuration/dotnetnuke/sitemap/providers" action="update" key="name" collision="overwrite">
                        <add name="DNNSimpleArticleSiteMapProvider" type="Christoc.Modules.dnnsimplearticle.Providers.Sitemap.Sitemap, DNNSimpleArticle" providerPath="~\DesktopModules\dnnsimplearticle\Providers\Sitemap\" />
                      </node>
                    </nodes>
                  </configuration>
                </install>
                <uninstall>
                  <configuration>
                    <nodes />
                  </configuration>
                </uninstall>
              </config>
            </component>
            
          </components>
        </package>
      </packages>
    </dotnetnuke>

