---
title: "Environment Variables"
slug: "environment-variables"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ----- | ----- |
| PORT | Port that the Meteor app will be available on. |
| MONGO_URL | URL to connect to the Mongo instance. |
| ROOT_URL | ... |
| OPLOG_URL| ... |
| MONGO_OPLOG_URL| ... |
| METEOR_ENV | ... |
| NODE_ENV | ... |
| NODE_OPTIONS | ... |
| DISABLE_WEBSOCKETS | ... |
| MAIL_URL | ... |
| DDP_DEFAULT_CONNECTION_URL | ... |
| HTTP_PROXY | ... |
| HTTPS_PROXY | ... |
| METEOR_OFFLINE_CATALOG | ... |
| METEOR_PROFILE | ... |
| METEOR_DEBUG_BUILD | ... |
| TINYTEST_FILTER | ... |
| MOBILE_ROOT_URL | ... |
| NODE_DEBUG | ... |
| BIND_IP | ... |
| PACKAGE_DIRS | ... |
| DEBUG | ... |
| METEOR_PRINT_CONSTRAINT_SOLVER_INPUT | ... |
| METEOR_CATALOG_COMPRESS_RPCS | ... |
| METEOR_MINIFY_LEGACY | ... |
| METEOR_DEBUG_SQL | ... |
| METEOR_WAREHOUSE_DIR | ... |
| AUTOUPDATE_VERSION | ... |
| USE_GLOBAL_ADK | ... |
| METEOR_AVD | ... |
| DEFAULT_AVD_NAME | ... |
| METEOR_BUILD_FARM_URL | ... |
| METEOR_PACKAGE_SERVER_URL | ... |
| METEOR_PACKAGE_STATS_SERVER_URL | ... |
| DEPLOY_HOSTNAME | ... |
| METEOR_SESSION_FILE | ... |
| METEOR_PROGRESS_DEBUG | ... |
| METEOR_PRETTY_OUTPUT | ... |
| APP_ID | ... |
| AUTOUPDATE_VERSION | ... |
| CONSTRAINT_SOLVER_BENCHMARK | ... |
| DDP_DEFAULT_CONNECTION_URL | ... |
| SERVER_WEBSOCKET_COMPRESSION | ... |
| USE_JSESSIONID | ... |
| METEOR_PKG_SPIDERABLE_PHANTOMJS_ARGS | ... |
| WRITE_RUNNER_JS | ... |
| TINYTEST_FILTER | ... |
| METEOR_PARENT_PID | ... |
| METEOR_TOOL_PATH | ... |
| RUN_ONCE_OUTCOME | ... |
| TREE_HASH_DEBUG | ... |
| METEOR_DEBUG_SPRINGBOARD | ... |
| METEOR_TEST_FAIL_RELEASE_DOWNLOAD | ... |
| METEOR_CATALOG_COMPRESS_RPCS | ... |
| METEOR_TEST_LATEST_RELEASE | ... |
| METEOR_WATCH_POLLING_INTERVAL_MS | ... |
| EMACS | ... |
| METEOR_PACKAGE_STATS_TEST_OUTPUT | ... |
| METEOR_TEST_TMP | ... |


## Using Environment Variables with Meteor
Environment variables can be defined before the meteor call, like so:

```
PORT=4000 meteor
NODE_ENV="staging" meteor
```

## Setting Meteor SMTP server
**Gmail Example**

    MAIL_URL=smtp://username%40gmail.com:password@smtp.gmail.com:465/

Note: This setup only allows 2000 emails to be sent per day. Please see https://support.google.com/a/answer/176600?hl=en for alternative configurations.

