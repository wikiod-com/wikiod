---
title: "Getting started with yarn"
slug: "getting-started-with-yarn"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation of Yarn with npm

    npm install --global yarn

If you didn't have npm installed before, check there website documentation for more details. https://yarnpkg.com/en/docs/install

## From NPM to Yarn
For the most of it, if you know NPM, you’re already set!

That adds a ‘package.json’ file in the root of your project

    npm init === yarn init
    
These are all the same:

    npm link === yarn link
    npm outdated === yarn outdated
    npm publish === yarn publish
    npm run === yarn run
    npm cache clean === yarn cache clean
    npm login === yarn login
    npm logout === yarn logout
    npm test === yarn test

‘Install’ is Yarn’s default behavior

    npm install === yarn
    The React Js library is saved in your package.json file:
    npm install react --save === yarn add react
    npm install -g @angular/cli === yarn global add @angular/cli

## Licensing with Yarn
Yarn can check the licenses of your dependencies and can also generate a license based on your package's dependencies.

    yarn licenses
    yarn licenses generate

## Checking Package Dependencies with Yarn
`yarn why package-name` will identify why a package is installed and which other packages depend upon it.

    yarn why react

## Using yarn with git repos
Using private repos working with yarn caveat: 

This works using `npm`:
```
"common-js": "git@bitbucket.org:<user-name>/<repo-name>.git#<identifier>"
```

but will not work using `yarn`.  This change is required:
```
"common-js": "git+ssh://git@bitbucket.org:<user-name>/<repo-name>.git#<identifier>"
```

Example uses `Bitbucket`, but `github` is the same.


_The ssh key is assumed to be saved on local machine_

## Installation or Setup
Detailed instructions on getting yarn set up or installed.

If you have **`npm`** installed on your system:

`npm install --global yarn`

On **macOS**:

- via Homebrew: `brew install yarn`
- via MacPorts: `sudo port install yarn` (**node** will be installed if not present)

On **Windows**:

- via Chocolatey: `choco install yarn`
- via Scoop: `scoop install yarn`
- via installer: [download installer](https://yarnpkg.com/en/docs/install#windows-tab)

On **Linux**:

- [look for instructions for your specific distribution](https://yarnpkg.com/en/docs/install#linux-tab)



