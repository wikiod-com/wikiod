---
title: "CMake integration in GitHub CI tools"
slug: "cmake-integration-in-github-ci-tools"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Configure Travis CI with stock CMake
Travis CI has CMake 2.8.7 pre-installed.

A minimal `.travis.yml` script for an out-of source build

    language: cpp
    
    compiler:
      - gcc
    
    before_script:
      # create a build folder for the out-of-source build
      - mkdir build
      # switch to build directory
      - cd build
      # run cmake; here we assume that the project's
      # top-level CMakeLists.txt is located at '..'
      - cmake ..
    
    script:
      # once CMake has done its job we just build using make as usual
      - make
      # if the project uses ctest we can run the tests like this
      - make test

## Configure Travis CI with newest CMake
The CMake version preinstalled on Travis is very old. You can use [the official Linux binaries][1] to build with a newer version.

Here is an example `.travis.yml`:

    language: cpp
    
    compiler:
      - gcc

    # the install step will take care of deploying a newer cmake version
    install:    
      # first we create a directory for the CMake binaries
      - DEPS_DIR="${TRAVIS_BUILD_DIR}/deps"
      - mkdir ${DEPS_DIR} && cd ${DEPS_DIR}
      # we use wget to fetch the cmake binaries
      - travis_retry wget --no-check-certificate https://cmake.org/files/v3.3/cmake-3.3.2-Linux-x86_64.tar.gz
      # this is optional, but useful:
      # do a quick md5 check to ensure that the archive we downloaded did not get compromised
      - echo "f3546812c11ce7f5d64dc132a566b749 *cmake-3.3.2-Linux-x86_64.tar.gz" > cmake_md5.txt
      - md5sum -c cmake_md5.txt
      # extract the binaries; the output here is quite lengthy,
      # so we swallow it to not clutter up the travis console
      - tar -xvf cmake-3.3.2-Linux-x86_64.tar.gz > /dev/null
      - mv cmake-3.3.2-Linux-x86_64 cmake-install
      # add both the top-level directory and the bin directory from the archive
      # to the system PATH. By adding it to the front of the path we hide the
      # preinstalled CMake with our own.
      - PATH=${DEPS_DIR}/cmake-install:${DEPS_DIR}/cmake-install/bin:$PATH
      # don't forget to switch back to the main build directory once you are done
      - cd ${TRAVIS_BUILD_DIR}

    before_script:
      # create a build folder for the out-of-source build
      - mkdir build
      # switch to build directory
      - cd build
      # run cmake; here we assume that the project's
      # top-level CMakeLists.txt is located at '..'
      - cmake ..
    
    script:
      # once CMake has done its job we just build using make as usual
      - make
      # if the project uses ctest we can run the tests like this
      - make test


  [1]: https://cmake.org/download/

