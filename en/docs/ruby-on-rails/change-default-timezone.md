---
title: "Change default timezone"
slug: "change-default-timezone"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

> **config.active_record.default_timezone** determines whether to use Time.local (if set to :local) or Time.utc (if set to :utc) when pulling dates and times from the database. The default is :utc.
> http://guides.rubyonrails.org/configuring.html

---
If you want to change **Rails** timezone, but continue to have **Active Record** save in the database in **UTC**, use

    # application.rb
    config.time_zone = 'Eastern Time (US & Canada)'

---
If you want to change **Rails** timezone **AND** have **Active Record** store times in this timezone, use

    # application.rb
    config.time_zone = 'Eastern Time (US & Canada)'
    config.active_record.default_timezone = :local

**Warning**: you really should think twice, even thrice, before saving times in the database in a non-UTC format.

> **Note**  
> Do not forget to restart your Rails server after modifying `application.rb`.

---

Remember that `config.active_record.default_timezone` can take only two values

- **:local** (converts to the timezone defined in `config.time_zone`)
- **:utc** (converts to UTC)

---
Here's how you can find all available timezones

    rake time:zones:all

## Change Rails timezone AND have Active Record store times in this timezone
    # application.rb
    config.time_zone = 'Eastern Time (US & Canada)'
    config.active_record.default_timezone = :local

## Change Rails timezone, but continue to have Active Record save in the database in UTC
    # application.rb
    config.time_zone = 'Eastern Time (US & Canada)'

