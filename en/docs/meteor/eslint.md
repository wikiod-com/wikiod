---
title: "ESLint"
slug: "eslint"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Adding eslint to your Meteor project
We'll use the popular `eslint-config-airbnb` as a starter as well as Meteor specific rules using `eslint-import-resolver-meteor`.

We also need to install `babel-parser` to lint Meteor enabled ES7 features such as async/await.

    cd my-project
    npm install --save-dev eslint-config-airbnb eslint-plugin-import eslint-plugin-react eslint-plugin-jsx-a11y eslint babel-eslint eslint-import-resolver-meteor
    touch .eslintrc.json

Then simply use this boilerplate `.eslintrc.json` to get started, you can override the rules as you wish.

    {
      "parser": "babel-eslint",
      "settings": {
        "import/resolver": "meteor"
      },
      "extends": "airbnb",
      "rules": {}
    }



## Using an npm script to lint your code
Edit your `package.json` to add the following script :

    {
      "scripts": {
        "lint": "eslint .;exit 0"
      }
    }

Then run it using `npm run lint`

We use `exit 0` as a trick to gracefully terminate the script when linting fails, otherwise `npm` will use `eslint` return code and crash.


