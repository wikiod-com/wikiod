---
title: "Continuous integration"
slug: "continuous-integration"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

The GitLab CI runs build jobs based on a checked in `.gitlab-ci.yml`. Jobs are run on a remote server in it's own docker container.

The CI server itself is configured with a `config.toml`.

- A build will fail if any lines in a job return an exit code != 0.

## Runner installation
# **Debian, Ubuntu and CentOS**

1. Add the official repository

Debian/Ubuntu

    curl -L https://packages.gitlab.com/install/repositories/runner/gitlab-ci-multi-runner/script.deb.sh | sudo bash

CentOS

    curl -L https://packages.gitlab.com/install/repositories/runner/gitlab-ci-multi-runner/script.rpm.sh | sudo bash

2. Install the `gitlab-ci-multi-runner` package

Debian/Ubuntu

    sudo apt-get install gitlab-ci-multi-runner

CentOS

    sudo yum install gitlab-ci-multi-runner

3. Register the runner


    sudo gitlab-ci-multi-runner register

+ Enter the URL to your GitLab CI. It should look like this `http://example.com/ci`

+ Enter the registration token. If this is a project specfic runner you can find the token in `Project settings -> Runners`. If it is a shared runner go to `Admin area -> Runners` and find the registration token there.

+ Now give your runner a descriptive name.

+ Select the executor which you want to use. Valid executors are: `shell` (These can be later configured to use sh or bash),`docker`,`docker-ssh`,`ssh`,`parallels`,`virtualbox`,`docker+machine` or `docker-ssh+machine`. For more detail information on executors check the [official documentation][1].

# **Windows**

1. Download the runner binary and place it somewhere appropriate on your system.
2. Open a command prompt as Administrator
3. Register the runner


    <runner-binary> register

+ Enter the URL to your GitLab CI. It should look like this `http://example.com/ci`

+ Enter the registration token. If this is a project specfic runner you can find the token in `Project settings -> Runners`. If it is a shared runner go to `Admin area -> Runners` and find the registration token there.

+ Now give your runner a descriptive name.

+ Select the executor which you want to use. Valid executors are: `shell`(Can be later configured to use cmd or powershell),`ssh`,`parallels` or `virtualbox`. For more detail information on executors check the [official documentation][1].

4. (Optional) Register runner as service


    <runner-binary> install --user <username> --password <password>

5. Start the runner


    <runner-binary> start

  [1]: https://gitlab.com/gitlab-org/gitlab-ci-multi-runner/blob/master/docs/configuration/advanced-configuration.md#the-executors

## Runner configuration
The config location for your runner is:

Debian/Ubuntu/CentOS

`/etc/gitlab-runner/config.toml` if run as root

`~/.gitlab-runner/config.toml` if run as non-root

Windows

`config.toml` where your binary is located


----------

A minimal `config.toml` can look like this:

    concurrent = 1
    [[runners]]
      name = "ExampleRunner"
      url = "https://example.com/ci"
      token = "f3058595ca4b2d217726466b1feed9"
      executor = "shell"
      shell = "bash"

For advanced configuration please check the [official documentation][1].


  [1]: https://gitlab.com/gitlab-org/gitlab-ci-multi-runner/blob/master/docs/configuration/advanced-configuration.md#the-global-section

## Setup Gitlab CI to allow cloning other private repositories
Some projects like GoLang might need to clone other dependent GitLab repositories during build. To get this working you can add a Deploy Key to dependent repositories and put the private key (without password) into the origin repository.

Create and check-in a SSH key inside the Git Repository that depends on some other repository during build time:

```
ssh-keygen -t rsa -b 4096 -C "My CI Deploykey"

# In the following promt name the key "deploykey" and leave the passphrase empty
Generating public/private rsa key pair.
Enter file in which to save the key (/home/user/.ssh/id_rsa): deploykey
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in deploykey.
Your public key has been saved in deploykey.pub.

# check-in both files

```

Use the `deploykey.pub` to configure a deploykey in the dependent repository. You can find an Deploykey page in the GitLab Project Settings.

Now add the following to you `.gitlab-ci.yml`

```
before_script:
  # Git and SSH setup to clone private repos
  # Needs the deploykey file to be installed in all dependent repositories
  - git config --global url."git@gitlab.com:".insteadOf "https://gitlab.com/"
  # Add gitlab to known_hosts
  - mkdir -p ~/.ssh && chmod 700 ~/.ssh
  - ssh-keyscan -H gitlab.com >> ~/.ssh/known_hosts
  # Start the ssh agent and add the deploykey
  - chmod 400 deploykey
  - eval $(ssh-agent -s)
  - ssh-add deploykey
```

Now any call to `git clone` inside your build should work. Even if it's via some other tools like `go get`, `govendor sync`, or whatever you are using.

