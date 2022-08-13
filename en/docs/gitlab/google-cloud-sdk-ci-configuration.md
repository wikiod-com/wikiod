---
title: "Google Cloud SDK CI Configuration"
slug: "google-cloud-sdk-ci-configuration"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Open JDK 8 Docker Example
      image: openjdk:8-jdk
      
      before_script:
           - curl https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.tar.gz > /tmp/google-cloud-sdk.tar.gz
           - mkdir -p /usr/local/gcloud
           - tar -C /usr/local/gcloud -xvf /tmp/google-cloud-sdk.tar.gz
           - echo y |/usr/local/gcloud/google-cloud-sdk/install.sh
           - chmod +x ./gradlew
      
        build:
          script:
            - ./gradlew build
          artifacts:
            paths:
            - app/build/outputs/

