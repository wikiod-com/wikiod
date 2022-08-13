---
title: "Getting started with accessibility"
slug: "getting-started-with-accessibility"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Accessibility Standards and APIs
Standards

* [Accessibility/Laws - GNOME Wiki!](https://wiki.gnome.org/Accessibility/Laws)

* [Information and Communication Technology (ICT) Standards and Guidelines: Section 508](http://www.section508.gov/)

* [Information and Communication Technology (ICT) Standards and Guidelines: Section 508 Refresh](https://www.access-board.gov/guidelines-and-standards/communications-and-it/about-the-ict-refresh)

* [Accessible Rich Internet Applications (WAI-ARIA) 1.0](https://www.w3.org/TR/wai-aria/) (W3C Recommendation, March 2014)

* [Web Content Accessibility Guidelines (WCAG) 2.0](http://www.w3.org/TR/WCAG20/) (W3C Recommendation, December 2008; also available as [ISO/IEC 40500:2012](http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=58625))

* [Guidance on Applying WCAG 2.0 to Non-Web Information and Communications Technologies (WCAG2ICT)](https://www.w3.org/TR/wcag2ict/) (W3C Working Group Note, September 2013)

* [User Agent Accessibility Guidelines (UAAG) 1.0](https://www.w3.org/TR/WAI-USERAGENT/) (W3C Recommendation, December 2002)

* [User Agent Accessibility Guidelines (UAAG) 2.0](https://www.w3.org/TR/UAAG20/) (W3C Working Group Note, December 2015)

* [Authoring Tool Accessibility Guidelines (ATAG) 2.0](https://www.w3.org/TR/ATAG20/) (W3C Recommendation, September 2015)

APIs

* [ATK - Accessibility Toolkit](https://developer.gnome.org/atk/stable/)

* [IAccessible2](https://wiki.linuxfoundation.org/accessibility/iaccessible2/start)

* [Assistive Technology Service Provider Interface (AT-SPI)](https://wiki.linuxfoundation.org/accessibility/atk/at-spi/start)

* [Accessibility Tools Framework | projects.eclipse.org](https://projects.eclipse.org/projects/technology.actf)

* [Microsoft Active Accessibility (MSAA)](https://msdn.microsoft.com/en-us/library/windows/desktop/dd373592)

* [Microsoft UI Automation (UIA)](https://msdn.microsoft.com/en-us/library/windows/desktop/bb892135.aspx)

* [NSAccessibility - AppKit | Apple Developer Documentation](https://developer.apple.com/reference/appkit/1658526-nsaccessibility)

## Installation and Setup
OSX

Implement the contract of the role-specific protocol (NSAccessibilityButton, NSAccessibilityImage,  NSAccessibilityGroup, etc) within the NSAccessibility protocol that best matches the behavior of the GUI element being rendered.

Linux / BSD

For GNOME applications, the GNOME Accessibility Implementation Library (GAIL) bridges GNOME widgets and the Accessibility Toolkit (ATK). ATK bridges to the Assistive Technology Service Provider Interface (AT-SPI). AT-SPI is currently used by GTK2, Java and OpenOffice.

Windows 

Microsoft Windows SDK includes all the tools necessary for MSAA and/or UI Automation.  The IAccessibleEx interface the bridges between the two worlds.

**References**

* [Windows Automation API SDK Tools – Microsoft Windows UI Automation Blog](https://blogs.msdn.microsoft.com/winuiautomation/2009/06/03/windows-automation-api-sdk-tools/)

* [NSAccessibility - AppKit | Apple Developer Documentation](https://developer.apple.com/reference/appkit/nsaccessibility)

* [Introducing ATK, AT-SPI, GAIL and GTK+](https://developer.gnome.org/accessibility-devel-guide/stable/dev-start-5.html.en)

