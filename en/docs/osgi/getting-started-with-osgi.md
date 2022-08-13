---
title: "Getting started with osgi"
slug: "getting-started-with-osgi"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Downloading and Using Equinox
Download and extract the OSGi starter kit for your platform from [Equinox download page for Neon release](http://download.eclipse.org/equinox/drops/R-Neon-201606061100/index.php).

Start the framework from the `rt/plugins folder` with the following command (or your platform's `rt` executable from the `rt` folder):

    rt/plugins$ java -jar org.eclipse.equinox.launcher_1.3.200.v20160318-1642.jar -console
    osgi>

In the OSGi console prompt, you can manage the bundles, for example:

    osgi>ss
    "Framework is launched."
    
    id      State       Bundle
    0       ACTIVE      org.eclipse.osgi_3.11.0.v20160603-1336
                        Fragments=1
    1       RESOLVED    org.eclipse.osgi.compatibility.state_1.0.200.v20160504-1419
                        Master=0
    2       RESOLVED    org.apache.commons.codec_1.6.0.v201305230611
    3       RESOLVED    org.apache.commons.logging_1.1.1.v201101211721
    4       ACTIVE      org.apache.felix.gogo.command_0.10.0.v201209301215
    5       ACTIVE      org.apache.felix.gogo.runtime_0.10.0.v201209301036
    6       ACTIVE      org.apache.felix.gogo.shell_0.10.0.v201212101605
    7       RESOLVED    org.apache.httpcomponents.httpclient_4.3.6.v201511171540
    8       RESOLVED    org.apache.httpcomponents.httpcore_4.3.3.v201411290715
    9       STARTING    org.eclipse.core.jobs_3.8.0.v20160509-0411
    10      STARTING    org.eclipse.ecf_3.8.0.v20160405-1820
                        Fragments=17
    11      STARTING    org.eclipse.ecf.filetransfer_5.0.0.v20160405-1820
    12      STARTING    org.eclipse.ecf.identity_3.7.0.v20160405-1820
    13      STARTING    org.eclipse.ecf.provider.filetransfer_3.2.200.v20160405-1820
                        Fragments=16
    14      STARTING    org.eclipse.ecf.provider.filetransfer.httpclient4_1.1.100.v20160405-1820
                        Fragments=15
    15      RESOLVED    org.eclipse.ecf.provider.filetransfer.httpclient4.ssl_1.1.0.v20160405-1820
                        Master=14
    16      RESOLVED    org.eclipse.ecf.provider.filetransfer.ssl_1.0.0.v20160405-1820
                        Master=13
    17      RESOLVED    org.eclipse.ecf.ssl_1.2.0.v20160405-1820
                        Master=10
    18      STARTING    org.eclipse.equinox.app_1.3.400.v20150715-1528
    19      ACTIVE      org.eclipse.equinox.common_3.8.0.v20160509-1230
    20      STARTING    org.eclipse.equinox.concurrent_1.1.0.v20130327-1442
    21      ACTIVE      org.eclipse.equinox.console_1.1.200.v20150929-1405
    22      RESOLVED    org.eclipse.equinox.ds_1.4.400.v20160226-2036
    23      STARTING    org.eclipse.equinox.frameworkadmin_2.0.300.v20160504-1450
    24      ACTIVE      org.eclipse.equinox.frameworkadmin.equinox_1.0.700.v20160102-2223
    25      RESOLVED    org.eclipse.equinox.launcher_1.3.200.v20160318-1642
                        Fragments=26
    26      RESOLVED    org.eclipse.equinox.launcher.win32.win32.x86_64_1.1.400.v20160518-1444
                        Master=25
    27      STARTING    org.eclipse.equinox.p2.artifact.repository_1.1.500.v20160419-0834
    28      ACTIVE      org.eclipse.equinox.p2.console_1.0.500.v20160504-1450
    29      ACTIVE      org.eclipse.equinox.p2.core_2.4.100.v20160419-0834
    30      STARTING    org.eclipse.equinox.p2.director_2.3.300.v20160504-1450
    31      STARTING    org.eclipse.equinox.p2.engine_2.4.100.v20160419-0834
    32      STARTING    org.eclipse.equinox.p2.garbagecollector_1.0.300.v20160504-1450
    33      RESOLVED    org.eclipse.equinox.p2.jarprocessor_1.0.500.v20160504-1450
    34      ACTIVE      org.eclipse.equinox.p2.metadata_2.3.100.v20160427-2220
    35      STARTING    org.eclipse.equinox.p2.metadata.repository_1.2.300.v20160419-0834
    36      STARTING    org.eclipse.equinox.p2.operations_2.4.200.v20160504-1450
    37      ACTIVE      org.eclipse.equinox.p2.repository_2.3.200.v20160421-0324
    38      STARTING    org.eclipse.equinox.p2.touchpoint.eclipse_2.1.400.v20160419-0834
    39      STARTING    org.eclipse.equinox.p2.touchpoint.natives_1.2.100.v20160419-0834
    40      STARTING    org.eclipse.equinox.p2.transport.ecf_1.1.200.v20160606-1311
    41      STARTING    org.eclipse.equinox.preferences_3.6.0.v20160120-1756
    42      ACTIVE      org.eclipse.equinox.registry_3.6.100.v20160223-2218
    43      STARTING    org.eclipse.equinox.security_1.2.200.v20150715-1528
                        Fragments=44
    44      RESOLVED    org.eclipse.equinox.security.win32.x86_64_1.0.100.v20130327-1442
                        Master=43
    45      STARTING    org.eclipse.equinox.simpleconfigurator_1.1.200.v20160504-1450
    46      ACTIVE      org.eclipse.equinox.simpleconfigurator.manipulator_2.0.200.v20160504-1450
    47      STARTING    org.eclipse.equinox.util_1.0.500.v20130404-1337
    48      RESOLVED    org.eclipse.osgi.services_3.5.100.v20160504-1419
    49      RESOLVED    org.sat4j.core_2.3.5.v201308161310
    50      RESOLVED    org.sat4j.pb_2.3.5.v201404071733
    51      RESOLVED    org.tukaani.xz_1.3.0.v201308270617

## Using Apache Felix
Download the [Apache Felix Framework Distribution](http://felix.apache.org/downloads.cgi#framework) and extract it into a directory:

<pre>
$ tar xf org.apache.felix.main.distribution-5.4.0.tar.gz
$ cd felix-framework-5.4.0
</pre>

And then start the framework with the following command:

<pre>
$ java -jar bin/felix.jar
____________________________
Welcome to Apache Felix Gogo
    
g!
</pre>

By default, Felix uses [Apache Felix Gogo](http://felix.apache.org/documentation/subprojects/apache-felix-gogo.html) as its interactive shell. The default command prompt, `g!`, is shown above.

The Gogo shell provides some simple builtin commands to interact with both the Gogo runtime environment, as well as with the OSGi framework itself. Type the `help` command at the `g!` prompt to see a list of builtin commands.

One example is the `lb` (short for _list bundles_) command which outputs a listing of the OSGi bundles that are currently installed in the framework:

    g! lb
    START LEVEL 1
       ID|State      |Level|Name
        0|Active     |    0|System Bundle (5.4.0)|5.4.0
        1|Active     |    1|Apache Felix Bundle Repository (2.0.6)|2.0.6
        2|Active     |    1|Apache Felix Gogo Command (0.16.0)|0.16.0
        3|Active     |    1|Apache Felix Gogo Runtime (0.16.2)|0.16.2
        4|Active     |    1|Apache Felix Gogo Shell (0.10.0)|0.10.0


