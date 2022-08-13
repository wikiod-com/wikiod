---
title: "Heroku Limits"
slug: "heroku-limits"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## List of all limitations in Heroku platform
**1. Logs:**
By default, Heroku allows only 1500 lines of consolidated logs. When more than 1500 lines of logs are required, one has to use [addons][1] provided Heroku.

**2. Router:**
HTTP request have 30s timeout for initial response and 55s timeout thereafter. Maximum of 1MB buffer allowed for response.

**3. Dynos:**
Dyno *memory limits* based on the [type][2] chosen. For free dynos, [sleep hours][3] are imposed where it sleeps after 30 minutes of inactivity.  In addition, verified accounts come with a monthly pool of 1000 Free dyno hours, and unverified accounts receive 550. An application can have upto 100 dynos and a *process type* can't be scaled to more than 10 dynos. Free dyno type can have a maximum of two concurrent running dynos.

**4. Config Vars:**
[Config key and value pair][4] is limited to 32kb for an app.

**5. Build:**
Users are limited to 75 requests to Heroku Git repos per hour, per app, per user. Uncompressed size during checkout can't reach more than 1GB. Slug size is limited to 300 MB and length of compilation can't exceed 15 minutes.

**6. Data Clips:**
Every query can run to a maximum of 10 minutes and can return a maximum of 100,000 rows.

**7. Heroku Postgres:**
Downtime varies with different [tiers][5] from less than 4 hours to 15 minutes per month.

**8. API Limits:**
Maximum calls to Heroku API is restricted to 2400/hour.

**9. Membership Limits:**
For an enterprise account, maximum of 500 members and for others, 25 members are allowed.

**10. Application count:**
A maximum of 100 apps can be created by a verified user. Unverified users are restricted to 5 applications.


  [1]: https://elements.heroku.com/addons/#logging
  [2]: https://devcenter.heroku.com/articles/dyno-types#available-dyno-types
  [3]: https://devcenter.heroku.com/articles/free-dyno-hours#dyno-sleeping
  [4]: https://devcenter.heroku.com/articles/config-vars
  [5]: https://devcenter.heroku.com/articles/heroku-postgres-plans#plan-tiers


