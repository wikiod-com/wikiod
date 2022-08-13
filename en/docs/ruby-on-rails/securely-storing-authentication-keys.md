---
title: "Securely storing authentication keys"
slug: "securely-storing-authentication-keys"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Many third-party APIs require a key, allowing them to prevent abuse. If they issue you a key, it's very important that you not commit the key into a public repository, as this will allow others to steal your key.

## Storing authentication keys with Figaro
Add `gem 'figaro'` to your Gemfile and run `bundle install`. Then run `bundle exec figaro install`; this will create config/application.yml and add it to your .gitignore file, preventing it from being added to version control.

You can store your keys in application.yml in this format:

```
SECRET_NAME: secret_value
```

where SECRET_NAME and secret_value are the name and value of your API key.

You also need to name these secrets in config/secrets.yml. You can have different secrets in each environment. The file should look like this:

```
development:
  secret_name: <%= ENV["SECRET_NAME"] %>
test:
  secret_name: <%= ENV["SECRET_NAME"] %>
production:
  secret_name: <%= ENV["SECRET_NAME"] %>
```

How you use these keys varies, but say for example `some_component` in the development environment needs access to `secret_name`. In config/environments/development.rb, you'd put:

```
Rails.application.configure do
  config.some_component.configuration_hash = {
    :secret => Rails.application.secrets.secret_name
  }
end
```

Finally, let's say you want to spin up a production environment on Heroku. This command will upload the values in config/environments/production.rb to Heroku:

```
$ figaro heroku:set -e production
```

