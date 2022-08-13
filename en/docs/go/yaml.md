---
title: "YAML"
slug: "yaml"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Creating a config file in YAML format
    import (
        "io/ioutil"
        "path/filepath"

        "gopkg.in/yaml.v2"
    )
    
    func main() {
        filename, _ := filepath.Abs("config/config.yml")
        yamlFile, err := ioutil.ReadFile(filename)
        var config Config
        err = yaml.Unmarshal(yamlFile, &config)
        if err != nil {
            panic(err)
        }
        //env can be accessed from config.Env
    }
    
    type Config struct {
        Env                 string `yaml:"env"`
    }
    
    //config.yml should be placed in config/config.yml for example, and needs to have the following line for the above example:
    //env: test

