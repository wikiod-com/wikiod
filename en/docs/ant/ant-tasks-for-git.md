---
title: "Ant Tasks for Git"
slug: "ant-tasks-for-git"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Get started:
    <macrodef name = "git">
        <attribute name = "command" />
        <attribute name = "dir" default = "" />
        <element name = "args" optional = "true" />
        <sequential>
            <echo message = "git @{command}" />
            <exec executable = "git" dir = "@{dir}">
                <arg value = "@{command}" />
                <args/>
            </exec>
        </sequential>
    </macrodef>
    <macrodef name = "git-clone-pull">
        <attribute name = "repository" />
        <attribute name = "dest" />
        <sequential>
            <git command = "clone">
                <args>
                    <arg value = "@{repository}" />
                    <arg value = "@{dest}" />
                </args>
            </git>
            <git command = "pull" dir = "@{dest}" />
        </sequential>
    </macrodef>

## Clone
    <git command="clone">
        <args>
            <arg value = "-v" />
            <arg value = "git@YOURGITURL:GITUSER/GITREPO" />
            <arg value = "repo" />
        </args>
    </git>

## Pull
    <git command = "pull" dir = "repository_path" />

## Add / Commit / Push
    <input message="Commit message" addproperty="commit-message" />
    <git command="add">
        <args>
                        <arg value="." />
        </args>
    </git>
    <git command="commit">
        <args>
                        <arg value="-am ${commit-message}" />
        </args>
    </git>
    <git command="push" />

