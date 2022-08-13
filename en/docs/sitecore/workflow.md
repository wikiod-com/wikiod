---
title: "Workflow"
slug: "workflow"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Workflows provide a flexible and controllable way of content creation, maintenance, and review. Workflow contains a list of states and commands.

## Assign workflow to an item
Information about the item workflow is stored in the "Workflow" field. 

    var workflow = Factory.GetDatabase("master").WorkflowProvider.GetWorkflow(workflowId);
    workflow.Start(item);

    var workflowId = item.Fields["__Default workflow"].Value;
    var workflow = Factory.GetDatabase("master").WorkflowProvider.GetWorkflow(workflowId);
    workflow.Start(item);

