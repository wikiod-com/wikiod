---
title: "Rails 5 API Authetication"
slug: "rails-5-api-authetication"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Authentication with Rails authenticate_with_http_token
    authenticate_with_http_token do |token, options|
      @user = User.find_by(auth_token: token)
    end

You can test this endpoint with `curl` by making a request like

    curl -IH "Authorization: Token token=my-token" http://localhost:3000

