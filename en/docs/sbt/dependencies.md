---
title: "Dependencies"
slug: "dependencies"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Add a Managed Library Dependency
`libraryDependency` is the `SettingKey` that handles 'managed' library dependencies, which are dependencies that are automatically downloaded, matching the supplied versions. To add a single dependency:

    libraryDependencies += "com.typesafe.slick" %% "slick" % "3.2.0-M1"

The first part, `"com.typesafe.slick"`, indicates the library package. The second part, `"slick"`, is the library in question. The final part, `"3.2.0-M1"`, is the version. Because the library is joined by `%%` the version of Scala supplied by the `scalaVersion` setting key will be utilized.

You can add multiple libraries at once using `++=`:

    libraryDependencies ++= Seq(
      "com.typesafe.slick" %% "slick" % "3.2.0-M1" % "compile",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.2.0-M1",
      "mysql" % "mysql-connector-java" % "latest.release"
    )

Remember Scala's functional nature, allowing you to compute dependencies. Just remember to return a `Seq`:

    libraryDependencies ++= {
      lazy val liftVersion = "3.0-RC3" //Version of a library being used
      lazy val liftEdition = liftVersion.substring(0,3) //Compute a value
      Seq(
        "net.liftweb" %% "lift-webkit" % liftVersion % "compile",  // Use var in Seq
        "net.liftmodules" %% ("ng_" + liftEdition) % "0.9.2" % "compile",  // Use computed var in Seq
      )  // Because this is the last statement, the Seq is returned and appended to libraryDependencies
    }

## Add a Repository
A repository is a place that SBT looks for `libraryDependencies`. If the build complains about not finding a dependency, it can be lacking the correct repository. Within SBT, the repositories are listed in the `resolvers` SettingKey:

    resolvers += "Flyway" at "https://flywaydb.org/repo"

This follows the syntax of 'Repository name' at 'url location'.

## Pin Library to Project Version of Scala
If your project has this:

    scalaVersion := 2.11  // Replace '2.11' with the version of Scala your project is running on

Then you can use `%%` to automatically get the version of the library compiled against the version of Scala the project is using:

    libraryDependencies += "com.typesafe.slick" %% "slick" % "3.2.0-M1"

Note that having the above two lines is equivalent to having this one line:

    libraryDependencies += "com.typesafe.slick" % "slick_2.11" % "3.2.0-M1"

## Pin Library to Specific Version of Scala
A library can be 'pinned' to a specific version of Scala using the `%` operator between the `groupId` and the `artifactId` (the first two strings in a library dependency). In this example, we pin the library with the `artifactId` of `slick` to Scala version `2.10`:

     libraryDependencies += "com.typesafe.slick" % "slick_2.10" % "3.2.0-M1"


