---
title: "Pre-planning before you create your forest or domain."
slug: "pre-planning-before-you-create-your-forest-or-domain"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

One thing you might consider with any domains in your forest is how many physical vs virtual machines you want to have.  Personally I believe that there should be one physical machine per domain.  One of the reasons I believe this is because of how the clocks are handled on, specifically in my case, Hyper-V machines.  I cannot speak to VMWare.  On a Hyper-V host it installs a clock sync service on the guest operating systems.  In a domain all member machines are synced up to the time service from the DCs.  Each domain syncs up to the forest.  But on a Hyper-V guest the clock is synced up to the physical machine's clock.  And if the host is a member machine the clock is then synced up to the domain.  This creates a feedback loop that allows for the clock to drift I have found.  After a couple months the time drifts to the point that there is a noticeable difference in time and in Active Directory that is a major issue.  To solve this I set my Hyper-V hosts to sync time at a very low interval from a physical DC that holds the Flexible Single Master Operation (FSMO) role of Primary Domain Controller (PDC) in the forest root domain.    

## Considerations
You need to decide what your forest structure is going to look like before you install Active Directory for the first time.

Are you to have just one domain in your forest, or are you going to have multiple domains in your forest?

Active Directory can support multiple sites. Generally sites are split up across slow links (slow network connections) or large distances between geographic areas, but they do not have to be. Sites are setup via IP Address subnets. You specify these IP Addresses belong to site A and the others belong to site B. Intersite replication (between site A and site B) usually occurs at a longer frequency than intrasite replication. For example replication between site A and site B could be set at the default 180 minutes (3 hours) and replication between Domain Controllers within the same site is by default set to 15 seconds.

You also need to consider the number of "machines" you will need to have in order to build out your Active Directory forest. Best practice would be have a minimum of two Domain Controllers (DC) per each domain in your forest for redundancy.  DNS is a requirement for Active Directory.  Active Directory clients such as users computers) use DNS to find each other and locate services advertised in Active Directory by the Active Directory domain controllers. You must decide whether DNS will be integrated with Active Directory or not.  It is easier to get Active Directory up and running off the ground quickly if you decide to integrate DNS to AD.  It is also worth noting that you should backup your Active Directory because having bad data replicate to another DC provides redundancy of bad data, and not a path to roll back to. 

