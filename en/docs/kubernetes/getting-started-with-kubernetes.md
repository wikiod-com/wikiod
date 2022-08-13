---
title: "Getting started with kubernetes"
slug: "getting-started-with-kubernetes"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Install on Google Cloud
Kubernetes was originally developed by Google to power their [Container Engine](https://cloud.google.com/container-engine/). As such, Kubernetes clusters are a first class citizen at Google.

Creating a Kubernetes cluster in the container engine requires `gcloud` command from the [Google Cloud SDK][gcloudsdk]. To install this command locally, use one of the following options:

- use the interactive installer (the easiest way for the newcomers):
```
curl https://sdk.cloud.google.com | bash
exec -l $SHELL
gcloud init
```

- download the SDK from https://cloud.google.com/sdk/ and run the appropriate install file.

    For example, to install in Linux (x86_64):
```
curl -Lo gcloud-sdk.tar.gz https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-142.0.0-linux-x86_64.tar.gz
tar xvf ./gcloud-sdk.tar.gz
./google-cloud-sdk/install.sh
gcloud init
```

Once `gcloud` is installed, create a Kubernetes cluster with:

```
# Give our cluster a name
CLUSTER_NAME=example-cluster

# Number of machines in the cluster.
NUM_NODES=3
    
gcloud container clusters create $CLUSTER_NAME --num_nodes=$NUM_VMS
```

[gcloudsdk]: https://cloud.google.com/sdk/

## Kubectl in command line
After you have a running cluster you can manage it with the `kubectl` command. Most of the commands you can get with the `kubectl --help` command, but I show you the most common commands, for manage and getting info about your cluster, nodes, pods, services and labels. 

---
For getting information about the cluster you can user the following command
```
kubectl cluster-info
```
It will show you the running address and port. 

---
For getting short information about the nodes, pods, services, etc. or any resources which got a place on the cluster you can use the following command
```
kubectl get {nodes, pods, services, ...}
```
The output mostly one line per resource. 

---
For getting detailed description about the resources you can use the `describe` flag for the `kubectl`
```
kubectl describe {nodes, pods, ...}
```

---
The deployed apps are only visible inside the cluster, so if you want to get the output from outside the cluster you should create a route between the terminal and kubernetes cluster. 
```
kubectl proxy
```
It will open a API, where we can get everything from the cluster. If you want to get the name of the pods for getting information about, you should use the following command:
```
kubectl get pods -o go-template --template '{{range .items}}{{.metadata.name}}{{"\n"}}{{end}}'
```
It will list the pods for later usage.
```
curl http://localhost:8001/api/v1/proxy/namespaces/default/pods/{pod_name}/
```

---
Two other common command is the getting logs and the execute a command from/in the containerized app. 
```
kubectl logs {pod_name}
kubectl exec {pod_name} {command}
```

---
Configuring tab completion for your shell can be done with:
```
source <(kubectl completion zsh)   # if you're using zsh
source <(kubectl completion bash)  # if you're using bash
```
or more programatically:
```
source <(kubectl completion "${0/-/}")
```

## Installing Minikube
Minikube creates a local cluster of virtual machines to run Kubernetes on.It is the simplest method to get your hands dirty with Kubernetes on your local machine.

Documentation for Minikube can be found at 
http://kubernetes.io/docs/getting-started-guides/minikube/

# Requirements

* On macOS, [xhyve driver][1], [VirtualBox][2] or [VMware Fusion][3] hypervisors
* On Linux, [VirtualBox][2] or [KVM][4] hypervisors
* On Windows [VirtualBox][2] or [Hyper-V][5] hypervisors
* VT-x/AMD-v virtualization enabled

To check if virtualization support is enabled, run the appropriate command from below. The command will output something if virtualization is enabled.

    # On Linux
    cat /proc/cpuinfo | grep 'vmx\|svm'
    # On OSX
    sysctl -a | grep machdep.cpu.features | grep VMX

# Installation

Minikube is a single binary. Thus, installation is as easy as downloading the binary and placing it in your path.

    # Specify the version of minikube to download.
    # Latest version can be retrieved from 
    # https://github.com/kubernetes/minikube/releases
    VERSION=v0.16.0

    # If on Linux
    OS=linux  
    # If on OSX
    # OS=darwin

    # URL to download minikube binary from
    URL=https://storage.googleapis.com/minikube/releases/$VERSION/minikube-$OS-amd64
    
    # Download binary and place in path.
    curl -Lo minikube $URL 
    chmod +x minikube 
    sudo mv minikube /usr/local/bin/

# Usage

To start a new cluster:

    minikube start

This will create a new cluster of local virtual machines with Kubernetes already installed and configured.

You can access the Kubernetes dashboard with:

    minikube dashboard

Minikube creates a related context for `kubectl` which can be used with:

    kubectl config use-context minikube


Once ready the local Kubernetes can be used:

    kubectl run hello-minikube --image=gcr.io/google_containers/echoserver:1.4 --port=8080
    kubectl expose deployment hello-minikube --type=NodePort
    curl $(minikube service hello-minikube --url)


To stop the local cluster:

    minikube stop

To delete the local cluster, note new IP will be allocated after creation:

    minikube delete


  [1]: https://github.com/kubernetes/minikube/blob/master/docs/drivers.md#xhyve-driver
  [2]: https://www.virtualbox.org/wiki/Downloads
  [3]: https://www.vmware.com/products/fusion.html
  [4]: https://github.com/kubernetes/minikube/blob/master/docs/drivers.md#kvm-driver
  [5]: https://github.com/kubernetes/minikube/blob/master/docs/drivers.md#hyperV-driver

## Configure kubectl
A Kubernetes cluster is controlled using the `kubectl` command. The method of configuring `kubectl` depends on where Kubernetes is installed.

# Google Cloud (Container Engine)

To install kubectl using the Google Cloud SDK:

    gcloud components install kubectl

To configure kubectl to control an existing Kubernetes cluster in Container Engine:

    gcloud container clusters get-credentials $CLUSTER_NAME

# Minikube

When using minikube, the kubectl binary needs to be manually downloaded and placed in the path.

    # Version of Kubernetes.
    K8S_VERSION=$(curl -sS https://storage.googleapis.com/kubernetes-release/release/stable.txt)
    # Operating System. Can be one of {linux, darwin}
    GOOS=linux
    # Architecture. Can be one of {386, amd64, arm64, ppc641e}
    GOARCH=amd64

    # Download and place in path.
    curl -Lo kubectl http://storage.googleapis.com/kubernetes-release/release/${K8S_VERSION}/bin/${GOOS}/${GOARCH}/kubectl
    chmod +x kubectl
    sudo mv kubectl /usr/local/bin/

The minikube binary automatically configures kubectl when starting a cluster.

    minikube start
    # kubectl is now ready to use!

## Hello World
Once your Kubernetes cluster is running and `kubectl` is configured you could run your first application with a few steps. This can be done using the [imperative commands][1] which doesn't need configuration files.

In order to run an application you need to provide a deployment name (`bootcamp`), the container image location (`docker.io/jocatalin/kubernetes-bootcamp:v1`) and the port (`8080`)

    $ kubectl run bootcamp --image=docker.io/jocatalin/kubernetes-bootcamp:v1 --port=8080

Confirm that it worked with:

    $ kubectl get deployments
    NAME                  DESIRED   CURRENT   UP-TO-DATE   AVAILABLE   AGE
    bootcamp               1         1         1            1         6s


To expose your application and make it accessible from the outside run:

    $ kubectl expose deployment/bootcamp --type="LoadBalancer" --port 8080
Confirm that it worked with:

    $ kubectl get services
    NAME                  CLUSTER-IP   EXTERNAL-IP         PORT(S)    AGE
    kubernetes            10.0.0.1     <none>              443/TCP    3m
    bootcamp              10.3.245.61  104.155.111.170     8080:32452/TCP   2m

To access the services, use the external IP and the application port e.g. like this:

    $ export EXTERNAL_IP=$(kubectl get service bootcamp --output=jsonpath='{.status.loadBalancer.ingress[0].ip}')
    $ export PORT=$(kubectl get services --output=jsonpath='{.items[0].spec.ports[0].port}')
    $ curl "$EXTERNAL_IP:$PORT"
    Hello Kubernetes bootcamp! | Running on: bootcamp-390780338-2fhnk | v=1

The same could be done manually with the data provided in:

    $ kubectl describe service bootcamp
    Name:                   bootcamp
    Namespace:              default
    Labels:                 run=bootcamp
    Selector:               run=bootcamp
    Type:                   LoadBalancer
    IP:                     10.3.245.61
    LoadBalancer Ingress:   104.155.111.170
    Port:                   <unset> 8080/TCP
    NodePort:               <unset> 32452/TCP
    Endpoints:              10.0.0.3:8080
    ... events and details left out ....
  
    $ export NODE=104.155.111.170
    $ export PORT=8080

Once this worked you can scale up your application with:

    $ kubectl scale deployments/bootcamp --replicas=4

And check the result with:

    $ kubectl get deployments
    NAME                  DESIRED   CURRENT   UP-TO-DATE   AVAILABLE   AGE
    bootcamp               4         4         4            4         30s

    $ curl "$EXTERNAL_IP:$PORT"
    Hello Kubernetes bootcamp! | Running on: bootcamp-390780338-2fhnk | v=1
    $ curl "$EXTERNAL_IP:$PORT"
    Hello Kubernetes bootcamp! | Running on: bootcamp-390780338-gmtv5 | v=1
    
Mind the changing pod id.

In order to push out a new application version run:

    kubectl set image deployments/bootcamp bootcamp=jocatalin/kubernetes-bootcamp:v2

And confirm it with:

    $ curl "$EXTERNAL_IP:$PORT"
    Hello Kubernetes bootcamp! | Running on: bootcamp-284539476-gafwev3 | v=2


Cleaning up is finally done with:

    $ kubectl delete deployment bootcamp
    $ kubectl delete service bootcamp


  [1]: https://kubernetes.io/docs/concepts/tools/kubectl/object-management-using-imperative-commands/

