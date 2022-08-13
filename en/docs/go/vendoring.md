---
title: "Vendoring"
slug: "vendoring"
draft: false
images: []
weight: 9765
type: docs
toc: true
---

Vendoring is a method of ensuring that all of your 3rd party packages that you use in your Go project are consistent for everyone who develops for your application.

When your Go package imports another package, the compiler normally checks `$GOPATH/src/` for the path of the imported project. However if your package contains a folder named `vendor`, the compiler will check in that folder *first*. This means that you can import other parties packages inside your own code repository, without having to modify their code.

Vendoring is a standard feature in Go 1.6 and up. In Go 1.5, you need to set the environment variable of `GO15VENDOREXPERIMENT=1` to enable vendoring.

## Use govendor to add external packages
[Govendor][1] is a tool that is used to import 3rd party packages into your code repository in a way that is compatible with golang's vendoring. 

Say for example that you are using a 3rd party package `bosun.org/slog`:

    package main
    
    import "bosun.org/slog"
    
    func main() {
        slog.Infof("Hello World")
    }


Your directory structure might look like:

    $GOPATH/src/
    ├── github.com/me/helloworld/
    |   ├── hello.go 
    ├── bosun.org/slog/
    |   ├── ... (slog files)

However someone who clones `github.com/me/helloworld` may not have a `$GOPATH/src/bosun.org/slog/` folder, causing _their_ build to fail due to missing packages.

Running the following command at your command prompt will grab all the external packages from your Go package and package the required bits into a vendor folder:

    govendor add +e

This instructs govendor to add all of the external packages into your current repository.

Your application's directory structure would now look like:

    $GOPATH/src/
    ├── github.com/me/helloworld/
    |   ├── vendor/
    |   |   ├── bosun.org/slog/
    |   |   |   ├── ... (slog files)
    |   ├── hello.go 

and those who clone your repository will also grab the required 3rd party packages.

  [1]: https://github.com/kardianos/govendor

## Using trash to manage ./vendor
[`trash`](https://github.com/rancher/trash) is a minimalistic vendoring tool that you configure with `vendor.conf` file. This example is for `trash` itself:

```
# package
github.com/rancher/trash

github.com/Sirupsen/logrus                      v0.10.0
github.com/urfave/cli                           v1.18.0
github.com/cloudfoundry-incubator/candiedyaml   99c3df8  https://github.com/imikushin/candiedyaml.git
github.com/stretchr/testify                     v1.1.3
github.com/davecgh/go-spew                      5215b55
github.com/pmezard/go-difflib                   792786c
golang.org/x/sys                                a408501
```

The first non-comment line is the package we're managing ./vendor for (note: this can be literally any package in your project, not just the root one).

Commented lines begin with `#`.

Each non-empty and non-comment line lists one dependency. Only the "root" package of the dependency needs to be listed.

After the package name goes the version (commit, tag or branch) and optionally the package repository URL (by default, it's inferred from the package name).

To populate your ./vendor dir, you need to have `vendor.conf` file in the current dir and just run:

```bash
$ trash
```

Trash will clone the vendored libraries into `~/.trash-cache` (by default), checkout requested versions, copy the files into `./vendor` dir and **prune non-imported packages and test files**. This last step keeps your ./vendor lean and mean and helps save space in your project repo.

Note: as of v0.2.5 trash is available for Linux and macOS, and only supports git to retrieve packages, as git's the most popular one, but we're working on adding all the others that `go get` supports.

## Use golang/dep
[golang/dep](https://github.com/golang/dep) is a prototype dependency management tool. Soon to be an official versioning tool. Current status **Alpha**.

## Usage
Get the tool via
```sh
$ go get -u github.com/golang/dep/...
```
Typical usage on a new repo might be
```sh
$ dep init
$ dep ensure -update
```
To update a dependency to a new version, you might run
```sh
$ dep ensure github.com/pkg/errors@^0.8.0
```
Note that the manifest and lock file formats **have now been finalized**. These will remain compatible even as the tool changes.

## vendor.json using Govendor tool
  
    # It creates vendor folder and vendor.json inside it
    govendor init

    # Add dependencies in vendor.json
    govendor fetch <dependency>

    # Usage on new repository
    # fetch depenencies in vendor.json
    govendor sync


Example vendor.json

    {

    "comment": "",
    "ignore": "test",
    "package": [
        {
            "checksumSHA1": "kBeNcaKk56FguvPSUCEaH6AxpRc=",
            "path": "github.com/golang/protobuf/proto",
            "revision": "2bba0603135d7d7f5cb73b2125beeda19c09f4ef",
            "revisionTime": "2017-03-31T03:19:02Z"
        },
        {
            "checksumSHA1": "1DRAxdlWzS4U0xKN/yQ/fdNN7f0=",
            "path": "github.com/syndtr/goleveldb/leveldb/errors",
            "revision": "8c81ea47d4c41a385645e133e15510fc6a2a74b4",
            "revisionTime": "2017-04-09T01:48:31Z"
        }
    ],
    "rootPath": "github.com/sample"

    }


