---
title: "Maven Build Cycle"
slug: "maven-build-cycle"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

Following is a complete list of Maven's default build lifecycle phases. Each of these phases is invoked by adding it to the `mvn` command, e.g. `mvn install`.

## Maven Build Lifecycle Phases
    validate

Validates whether the project is correct and all the required information are available for the build.

    initialize

Initializes the build environment, e.g. sets properties or creates directories.

    generate-sources

Generates source code to be processed in the 'compile' phase.

    process-sources

Processes the source code in case some filter need to be applied.

    generate-resources

Generates resources to be included in the artifact.

    process-resources

Processes and copies resources into the output directory (`${basedir}/target/classes`).

    compile

Compiles the project's source code in the source directory (`${basedir}/src/main/[java|groovy|...]`) into the output directory (`${basedir}/target/classes`).

    process-classes

Processes `.class` files generated in the `compile` phase, e.g. to perform bytecode enhancements.

    generate-test-sources

Generates test source code to be processed in the `test-compile` phase.

    process-test-sources

Processes test source code in case some filter need to be applied.

    generate-test-resources

Generates resources for testing.

    process-test-resources

Processes and copies test resources in the resources directory (`${basedir}/src/main/resources`) into the test output directory (`${basedir}/target/test-classes`).

    test-compile

Compiles source code in the test source directory ('${basedir}/src/test/[java|groovy|...]') into the test output directory(`${basedir}/target/test-classes`).

    process-test-classes

Processes test `.class` files generated in the `test-compile` phase, e.g. to perform bytecode enhancements (Maven 2.0.5 and above).

    test

Runs tests using some suitable test framework. Note: These test cases are not considered for packaging and deploying.

    prepare-package

Performs final changes and validations before the package is finally created.

    package

Packages the successfully compiled and tested code into some distributable format like JAR, WAR, EAR into the target directory (`${basedir}/target`).

    pre-integration-test

Performs actions before integration tests are run if they require to apply some changes in the environment for the application.

    integration-test

Processes and possibly deploys the application to an environment where integration tests can be run.

    post-integration-test

Performs actions after the integration tests, like cleaning up the environment which has been created in the `pre-integration-test` phase.

    verify

Checks whether a package is valid and meets required quality criteria.

    install

Installs the artifact into the local repository. Any other local project can use this artifact as one of its dependencies after that (if your IDE doesn't support _workspace dependency resolution_ anyway).

    deploy

Copies the package to a remote repository to make it available for other developers.

