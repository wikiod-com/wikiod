---
title: "Calling Kubernetes API"
slug: "calling-kubernetes-api"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Using Kubernetes Go Client - Outside of Cluster

    package main

    import (
      "fmt"

      "k8s.io/client-go/1.5/kubernetes"
      "k8s.io/client-go/1.5/pkg/api/v1"
      "k8s.io/client-go/1.5/tools/clientcmd"
    )

    func main()  {
        config, err := clientcmd.BuildConfigFromFlags("", <kube-config-path>)
        if err != nil {
          return nil, err
        }

        c, err := kubernetes.NewForConfig(config)
        if err != nil {
          return nil, err
        }

        // Get Pod by name
        pod, err := c.Pods(v1.NamespaceDefault).Get("my-pod")
        if err != nil {
            fmt.Println(err)
            return
        }
      
        // Print its creation time
        fmt.Println(pod.GetCreationTimestamp())
    }


## Using Kubernetes Go Client - Inside of Cluster

    package main

    import (
      "fmt"

      "k8s.io/client-go/1.5/kubernetes"
      "k8s.io/client-go/1.5/pkg/api/v1"
      "k8s.io/client-go/1.5/rest"
    )

    func main()  {
        config, err = rest.InClusterConfig()
        if err != nil {
          return nil, err
        }

        c, err := kubernetes.NewForConfig(config)
        if err != nil {
          return nil, err
        }

        // Get Pod by name
        pod, err := c.Pods(v1.NamespaceDefault).Get("my-pod")
        if err != nil {
            fmt.Println(err)
            return
        }
      
        // Print its creation time
        fmt.Println(pod.GetCreationTimestamp())
    }


## List replicasets by given deployment with kubernetes go client
```golang
package main

import (
    "k8s.io/kubernetes/pkg/api"
    unver "k8s.io/kubernetes/pkg/api/unversioned"
    "k8s.io/kubernetes/pkg/apis/extensions"
    "k8s.io/kubernetes/pkg/client/restclient"
    client "k8s.io/kubernetes/pkg/client/unversioned"
    "log"
    "os"
    "strings"
)

var logger *log.Logger

const (
    SERVER             string = "http://172.21.1.11:8080"
    RevisionAnnotation        = "deployment.kubernetes.io/revision"
)

func init() {
    logger = log.New(os.Stdout, "", 0)
}

func getReplicaSetsByDeployment(c *client.Client, deployment *extensions.Deployment) ([]extensions.ReplicaSet, error) {

    namespace := deployment.Namespace
    selector, err := unver.LabelSelectorAsSelector(deployment.Spec.Selector)
    if err != nil {
        return nil, err
    }
    options := api.ListOptions{LabelSelector: selector}
    rsList, err := c.Extensions().ReplicaSets(namespace).List(options)

    return rsList.Items, nil
}

func getDeploymentByReplicaSet(namespace string, c *client.Client, rs *extensions.ReplicaSet) ([]extensions.Deployment, error) {

    selector, err := unver.LabelSelectorAsSelector(rs.Spec.Selector)
    if err != nil {
        return nil, err
    }
    options := api.ListOptions{LabelSelector: selector}
    dps, err := c.Extensions().Deployments(namespace).List(options)
    if err != nil {
        return nil, err
    }

    return dps.Items, nil
}

func getDeploymentByReplicaSetName(namespace string, c *client.Client, rs *extensions.ReplicaSet) (*extensions.Deployment, error) {

    name := rs.Name
    index := strings.LastIndex(name, "-")
    deploymentName := name[:index]

    dp, err := c.Extensions().Deployments(namespace).Get(deploymentName)
    if err != nil {
        return nil, err
    }

    return dp, nil
}

func main() {

    config := &restclient.Config{
        Host: SERVER,
    }

    c, err := client.New(config)
    if err != nil {
        logger.Fatalf("Could not connect to k8s api: err=%s\n", err)
    }

    list, err := c.Extensions().Deployments(api.NamespaceDefault).List(api.ListOptions{})
    if err != nil {
        logger.Fatalf("Could not list deployments: err=%s\n", err)
    }

    logger.Printf("Deployment -------> ReplicaSet: ")

    for _, deployment := range list.Items {
        rses, err := getReplicaSetsByDeployment(c, &deployment)
        if err != nil {
            logger.Fatalf("GetReplicaSetsByDeployment Error: err=%s\n", err)
        }

        for _, rs := range rses {
            logger.Printf("ReplicaSet assioated with Deployment: rs-name=%s, revision=%s, dp-name=%s\n",
                rs.Name, rs.Annotations[RevisionAnnotation], deployment.Name)
        }
    }

    logger.Printf("\n\nReplicaSet -------> Deployment: ")
    rsList, err := c.Extensions().ReplicaSets(api.NamespaceDefault).List(api.ListOptions{})
    if err != nil {
        log.Fatalf("Could not list ReplicaSet")
    }

    for _, rs := range rsList.Items {
        dp, err := getDeploymentByReplicaSetName(api.NamespaceDefault, c, &rs)
        if err != nil {
            logger.Fatalf("GetDeploymentByReplicaSet Error: err=%s\n", err)
        }

        logger.Printf("Deployment assioated with ReplicaSet: rs-name=%s, revision=%s, dp-name=%s\n",
            rs.Name, rs.Annotations[RevisionAnnotation], dp.Name)
    }
}
```

## Rollback with the revision of replicasets using kubernetes go client
```golang
package main

import (
    //"k8s.io/kubernetes/pkg/api"
    "k8s.io/kubernetes/pkg/client/restclient"
    "log"
    "os"
    // unver "k8s.io/kubernetes/pkg/api/unversioned"
    "k8s.io/kubernetes/pkg/apis/extensions"
    client "k8s.io/kubernetes/pkg/client/unversioned"
)

var logger *log.Logger

var annotations = map[string]string{
    "Image":                      "nginx:1.7.9",
    "UserId":                     "2",
    "kubernetes.io/change-cause": "version mismatch",
}

const (
    DEPLOYMENT         string = "nginx-test"
    REVERSION          int64  = 4
    SERVER             string = "http://172.21.1.11:8080"
    RevisionAnnotation        = "deployment.kubernetes.io/revision"
)

func init() {
    logger = log.New(os.Stdout, "", 0)
}

func rollBack(c *client.Client, dp *extensions.Deployment, revision int64) error {

    dr := new(extensions.DeploymentRollback)
    dr.Name = dp.Name
    dr.UpdatedAnnotations = annotations
    dr.RollbackTo = extensions.RollbackConfig{Revision: revision}

    // Rollback
    err := c.Extensions().Deployments("ops").Rollback(dr)
    if err != nil {
        logger.Printf("Deployment Rollback Error: err=%s\n", err)
        return err
    }

    return nil
}

func main() {
    config := &restclient.Config{
        Host: SERVER,
    }

    c, err := client.New(config)
    if err != nil {
        logger.Fatalf("Could not connect to k8s api: err=%s\n", err)
    }

    dp, err := c.Extensions().Deployments("ops").Get(DEPLOYMENT)
    if err != nil {
        logger.Fatalf("Could not list deployments: err=%s\n", err)
    }

    rollBack(c, dp, REVERSION)

}
```

## Rolling update with repliasets using kubernetes go client
```golang
package main

import (
    //    "k8s.io/kubernetes/pkg/api"
    // unver "k8s.io/kubernetes/pkg/api/unversioned"
    "k8s.io/kubernetes/pkg/apis/extensions"
    "k8s.io/kubernetes/pkg/client/restclient"
    client "k8s.io/kubernetes/pkg/client/unversioned"
    "k8s.io/kubernetes/pkg/util/intstr"
    "log"
    "os"
)

var logger *log.Logger

const (
    //DEPLOYMENT           string = "nginx-deployment"
    DEPLOYMENT           string = "nginx-test"
    RevisionHistoryLimit int32  = 5
    SERVER               string = "http://172.21.1.11:8080"
    RevisionAnnotation   string = "deployment.kubernetes.io/revision"
)

func init() {
    logger = log.New(os.Stdout, "", 0)
}

func rollingUpdate(c *client.Client, dp *extensions.Deployment) error {

    // New a DeploymentStrategy
    ds := new(extensions.DeploymentStrategy)
    ds.Type = extensions.RollingUpdateDeploymentStrategyType
    ds.RollingUpdate = new(extensions.RollingUpdateDeployment)
    ds.RollingUpdate.MaxUnavailable = intstr.FromInt(int(dp.Spec.Replicas))

    dp.Spec.Strategy = *ds

    // Image
    //dp.Spec.Template.Spec.Containers[0].Image = "nginx:1.9.7"
    dp.Spec.Template.Spec.Containers[0].Image = "nginx:1.9"

    // Update
    //_, err := c.Extensions().Deployments(api.NamespaceDefault).Update(dp)
    _, err := c.Extensions().Deployments("ops").Update(dp)
    if err != nil {
        logger.Printf("Update Deployment Error: err=%s\n", err)
        return err
    }
    return nil
}

func main() {
    config := &restclient.Config{
        Host: SERVER,
    }

    c, err := client.New(config)
    if err != nil {
        logger.Fatalf("Could not connect to k8s api: err=%s\n", err)
    }

    //dp, err := c.Extensions().Deployments(api.NamespaceDefault).Get(DEPLOYMENT)
    dp, err := c.Extensions().Deployments("ops").Get(DEPLOYMENT)
    if err != nil {
        logger.Fatalf("Could not list deployments: err=%s\n", err)
    }

    rollingUpdate(c, dp)

}
```

