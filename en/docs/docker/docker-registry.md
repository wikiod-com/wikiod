---
title: "Docker Registry"
slug: "docker-registry"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Running the registry
**Do not use `registry:latest`!** This image points to the old v1 registry. That Python project is no longer being developed. The new v2 registry is written in Go and is actively maintained. When people refer to a "private registry" they are referring to the v2 registry, *not* the v1 registry!

    docker run -d -p 5000:5000 --name="registry" registry:2

The above command runs the newest version of the registry, which can be found in the [Docker Distribution project](https://github.com/docker/distribution).

For more examples of image management features, such as tagging, pulling, or pushing, see the section on managing images.

## Configure the registry with AWS S3 storage backend
Configuring a private registry to use an [AWS S3](https://aws.amazon.com/s3/) backend is easy. The registry can do this automatically with the right configuration. Here is an example of what should be in your `config.yml` file:

    storage:
        s3:
            accesskey: AKAAAAAACCCCCCCBBBDA
            secretkey: rn9rjnNuX44iK+26qpM4cDEoOnonbBW98FYaiDtS
            region: us-east-1
            bucket: registry.example.com
            encrypt: false
            secure: true
            v4auth: true
            chunksize: 5242880
            rootdirectory: /registry

The `accesskey` and `secretkey` fields are IAM credentials with specific S3 permissions (see [the documentation](https://github.com/docker/distribution/blob/master/docs/storage-drivers/s3.md) for more information). It can just as easily use credentials with the [`AmazonS3FullAccess` policy](https://docs.aws.amazon.com/directoryservice/latest/admin-guide/role_s3_full_access.html) attached. The `region` is the region of your S3 bucket. The `bucket` is the bucket name. You may elect to store your images encrypted with `encrypt`. The `secure` field is to indicate the use of HTTPS. You should generally set `v4auth` to true, even though its default value is false. The `chunksize` field allows you to abide by the S3 API requirement that chunked uploads are at least five megabytes in size. Finally, `rootdirectory` specifies a directory underneath your S3 bucket to use.

There are [other storage backends](https://github.com/docker/distribution/blob/master/docs/storage-drivers/index.md) that can be configured just as easily.

