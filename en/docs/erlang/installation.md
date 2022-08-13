---
title: "Installation"
slug: "installation"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Build and Install Erlang/OTP on FreeBSD
The following examples show 3 main methods for installing Erlang/OTP on FreeBSD.  

# Method 1 - Pre-built Binary Package
Use pkg to install the pre-built binary package:

    $ pkg install erlang

Test your new installation:

    $ erl
    Erlang/OTP 18 [erts-7.3.1] [source] [64-bit] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]
    
    Eshell V7.3.1  (abort with ^G)


# Method 2 - Build and install using the port collection (recommended)
Build and install the port as usual:

    $ make -C /usr/ports/lang/erlang install clean

Test your new installation:

    $ erl
    Erlang/OTP 18 [erts-7.3.1] [source] [64-bit] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]
    
    Eshell V7.3.1  (abort with ^G)


This will fetch the release tarball from the official website, apply a few patches if needed, build the release and install it. Obviously, it will take some time.

# Method 3 - Build and install from the release tarball
Note: building the release manually works, but using the two above methods should be preferred, since the port collection embeds patches that render the release more FreeBSD friendly.

Download the release file:

    $ fetch 'http://erlang.org/download/otp_src_18.3.tar.gz'

Check that its MD5 sum is correct:
    
    $ fetch 'http://erlang.org/download/MD5'
    MD5                                           100% of   24 kB  266 kBps 00m00s
    
    $ grep otp_src_18.3.tar.gz MD5
    MD5(otp_src_18.3.tar.gz)= 7e4ff32f97c36fb3dab736f8d481830b
    
    $ md5 otp_src_18.3.tar.gz
    MD5 (otp_src_18.3.tar.gz) = 7e4ff32f97c36fb3dab736f8d481830b

Extract the tarball:

    $ tar xzf otp_src_18.3.tar.gz
    
Configure:

    $ ./configure --disable-hipe
    
If you want to build Erlang with HiPe, you will need to apply the patches from  the port collection.
    
Build:

    $ gmake
    
Install:

    $ gmake install
    
Test your new installation:

    $ erl
    Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:2:2] [async-threads:10] [kernel-poll:false]
    
    Eshell V7.3  (abort with ^G)

## Build and install using kerl
[kerl](https://github.com/kerl/kerl) is a tool that helps you build and install Erlang/OTP releases.

Install curl:

    $ make -C /usr/ports/ftp/curl install clean

Download kerl:

    $ fetch 'https://raw.githubusercontent.com/kerl/kerl/master/kerl'
    $ chmod +x kerl

Update the list of available releases:

    $ ./kerl update releases
    The available releases are:
    R10B-0 R10B-10 R10B-1a R10B-2 R10B-3 R10B-4 R10B-5 R10B-6 R10B-7 R10B-8 R10B-9 R11B-0 R11B-1 R11B-2 R11B-3 R11B-4 R11B-5 R12B-0 R12B-1 R12B-2 R12B-3 R12B-4 R12B-5 R13A R13B01 R13B02-1 R13B02 R13B03 R13B04 R13B R14A R14B01 R14B02 R14B03 R14B04 R14B R14B_erts-5.8.1.1 R15B01 R15B02 R15B02_with_MSVCR100_installer_fix R15B03-1 R15B03 R15B R16A_RELEASE_CANDIDATE R16B01 R16B02 R16B03-1 R16B03 R16B 17.0-rc1 17.0-rc2 17.0 17.1 17.3 17.4 17.5 18.0 18.1 18.2 18.2.1 18.3 19.0

Build the required release:

    $ ./kerl build 18.3 erlang-18.3

Check that the build is present in the build list:

    $ ./kerl list builds
    18.3,erlang-18.3

Install the build somewhere:

    $ ./kerl install erlang-18.3 ./erlang-18.3

Source the `activate` file if you're running bash or the fish shell. If you're running a cshell, add the build bin directory to the PATH:

    $ setenv PATH "/some/where/erlang-18.3/bin/:$PATH"
    
Test your new installation:
    
    $ which erl
    /some/where/erlang-18.3/bin//erl
    
    $ erl
    Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]
    
    Eshell V7.3  (abort with ^G)

    
## Other releases

If you want to build another version of Erlang/OTP, look for the other ports in the collection:
  * [lang/erlang-runtime15](https://www.freshports.org/lang/erlang-runtime15/)
  * [lang/erlang-runtime16](https://www.freshports.org/lang/erlang-runtime16/)
  * [lang/erlang-runtime17](https://www.freshports.org/lang/erlang-runtime17/)
  * [lang/erlang-runtime18](https://www.freshports.org/lang/erlang-runtime18/)
    
## Reference
* [FreeBSD Handbook -> Chapter 4. Installing Applications: Packages and Ports](https://www.freebsd.org/doc/handbook/ports.html)
* [Erlang on FreshPorts](https://www.freshports.org/lang/erlang/)
* [Kerl documentation on GitHub](https://github.com/kerl/kerl)
 


## Build and Install Erlang/OTP on Ubuntu
The following examples show two main methods for installing Erlang/OTP on Ubuntu.

# Method 1 - Pre-built Binary Package
Simply run this command and it will download and install the latest stable Erlang release from [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html).

    $ sudo apt-get install erlang

# Method 2 - Build and Install from Source

Download the tar file:

    $ wget http://erlang.org/download/otp_src_19.0.tar.gz

Extract the tar file:

    $ tar -zxf otp_src_19.0.tar.gz

Enter the extracted directory and set `ERL_TOP` to be the current path:

    $ cd otp_src_19.0
    $ export ERL_TOP=`pwd`

Now before configuring the build, you want to make sure you have all the dependencies you need in order to install Erlang:

**Basic dependencies:**

    $ sudo apt-get install autoconf libncurses-dev build-essential

**Other applications dependencies**

| Application | Dependency install |
| ------ | ------ |
| HiPE | `$ sudo apt-get install m4` |
| ODBC   | `$ sudo apt-get install unixodbc-dev`   |
| OpenSSL | `$ sudo apt-get install libssl-dev` |
| wxWidgets | `$ sudo apt-get install libwxgtk3.0-dev libglu-dev` |
| Documentation | `$ sudo apt-get install fop xsltproc` |
| Orber and other C++ projects | `$ sudo apt-get install g++` |
| jinterface | `$ sudo apt-get install default-jdk` |

Configure and build:  
You can set your own options, or leave it blank to run the default configuration. [Advanced configuration and build for Erlang/OTP](http://erlang.org/doc/installation_guide/INSTALL.html#id64709).

    $ ./configure [ options ]
    $ make

Testing the build:  

    $ make release_tests
    $ cd release/tests/test_server
    $ $ERL_TOP/bin/erl -s ts install -s ts smoke_test batch -s init stop

After running these commands, open `$ERL_TOP/release/tests/test_server/index.html` with your web browser and check that you don't have any fails. If all tests passed we're ok to continue to the installation.

Installing:

    $ make install

## Build and Install Erlang/OTP on OpenBSD
Erlang on OpenBSD is currently broken on `alpha`, `sparc` and `hppa` architectures. 

# Method 1 - Pre-built Binary Package

OpenBSD let you choose desired version you want to install on your system:

<!-- language: lang-sh -->

    ######################################################################
    # free-choice:
    ######################################################################
    $ pkg_add erlang
    # a       0: <None>
    #   1: erlang-16b.03p10v0
    #   2: erlang-17.5p6v0
    #   3: erlang-18.1p1v0
    #   4: erlang-19.0v0

    ######################################################################
    # manual-choice:
    ######################################################################
    pkg_add erlang%${version}
    # example: 
    pkg_add erlang%19

OpenBSD can support multiple version of Erlang. To make thinks easier to use, each binaries are installed Erlang version in its name. So, if you have installed `erlang-19.0v0`, your `erl` binary will be `erl19`.

If you want to use `erl`, you can create a symlink:

<!-- language: lang-sh -->

    ln -s /usr/local/bin/erl19 /usr/local/bin/erl

or create an alias in your shell configuration file or in `.profile` file:

<!-- language: lang-sh -->

    echo 'alias erl="erl19"' >> ~/.profile
    # or
    echo 'alias erl="erl19"' >> ~/.shrc

You can now run `erl`:

<!-- language: lang-sh -->

    erl19
    # or if you have an alias or symlink
    erl
    # Erlang/OTP 19 [erts-8.0] [source] [async-threads:10] [kernel-poll:false]
    # 
    # Eshell V8.0  (abort with ^G)

# Method 2 - Build and install using ports

<!-- language: lang-sh -->

     RELEASE=OPENBSD_$(uname -r  | sed 's/\./_/g')
     cd /usr
     cvs -qz3 -danoncvs@anoncvs.openbsd.org:/cvs co -r${RELEASE}
     cd /usr/ports/lang/erlang
     ls -p 
     # 16/ 17/ 18/ 19/  CVS/  Makefile  Makefile.inc  erlang.port.mk
     cd 19
     make && make install

# Method 3 - Build from source

Build from source require additional packages:

 * `git`
 * `gmake`
 * `autoconf-2.59`

<!-- language: lang-sh -->

    pkg_add git gmake autoconf%2.59
    git clone https://github.com/erlang/otp.git
    cd otp
    AUTOCONF_VERSION="2.59" ./build_build all

# References

 * http://openports.se/lang/erlang
 * http://cvsweb.openbsd.org/cgi-bin/cvsweb/ports/lang/erlang/
 * https://www.openbsd.org/faq/faq15.html
 * http://man.openbsd.org/OpenBSD-current/man1/pkg_add.1

