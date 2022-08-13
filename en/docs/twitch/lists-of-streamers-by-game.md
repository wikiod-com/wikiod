---
title: "Lists of Streamers by Game"
slug: "lists-of-streamers-by-game"
draft: false
images: []
weight: 9875
type: docs
toc: true
---

## Getting the First Page in Ruby
This Ruby example uses [Mechanize][mechanize], a library to automate web interactions.

`client_id` is an OAuth client_id.

`game` is the game directory to list.

[mechanize]: https://github.com/sparklemotion/mechanize

```ruby
require 'mechanize'
master_agent = Mechanize.new

client_id = "123"
game = "Minecraft"

url = "https://api.twitch.tv/kraken/streams?game=#{game}&client_id=#{client_id}"
final_list = []
master_agent.get(url) do |page|
    master_list = JSON.parse(page.body)
    master_list["streams"].each do |stream|
        final_list << stream["channel"]["name"]
    end
end
```

