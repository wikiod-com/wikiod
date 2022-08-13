---
title: "Iptables with Docker"
slug: "iptables-with-docker"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

This topic is about how to limit access to your docker containers from outside world using iptables.

For impatient people, you can check the examples. For the others, please read the remark section to understand how to build new rules.

## Syntax
- iptables -I DOCKER [RULE ...] [ACCEPT|DROP] // To add a rule a the top of the DOCKER table
- iptables -D DOCKER [RULE ...] [ACCEPT|DROP] // To remove a rule from the DOCKER table
- ipset restore < /etc/ipfriends.conf // To reconfigure your ipset *ipfriends*

## Parameters
| Parameters | Details |
| ---------- | ------- |
| ext_if | Your external interface on Docker host. |
| XXX.XXX.XXX.XXX | A particular IP on which Docker containers access should be given. |
| YYY.YYY.YYY.YYY | Another IP on which Docker containers access should be given. |
| ipfriends | The ipset name defining the IPs allowed to access your Docker containers.

# The problem

Configuring iptables rules for Docker containers is a bit tricky. At first, you would think that "classic" firewall rules should do the trick.

For example, let's assume that you have configured a nginx-proxy container + several service containers to expose via HTTPS some personal web services. Then a rule like this should give access to your web services only for IP XXX.XXX.XXX.XXX.

    $ iptables -A INPUT -i eth0 -p tcp -s XXX.XXX.XXX.XXX -j ACCEPT
    $ iptables -P INPUT DROP

It won't work, your containers are still accessible for everyone.

Indeed, Docker containers are not host services. They rely on a virtual network in your host, and the host acts as a gateway for this network. And concerning gateways, routed traffic is not handled by the INPUT table, but by the FORWARD table, which makes the rule posted before uneffective.

But it's not all. In fact, Docker daemon creates a lot of iptables rules when it starts to do its magic concerning containers network connectivity. In particular, a DOCKER table is created to handle rules concerning containers by forwarding traffic from the FORWARD table to this new table.

    $ iptables -L
    Chain INPUT (policy ACCEPT)
    target     prot opt source               destination
    
    Chain FORWARD (policy DROP)
    target     prot opt source               destination
    DOCKER-ISOLATION  all  --  anywhere             anywhere
    DOCKER     all  --  anywhere             anywhere
    ACCEPT     all  --  anywhere             anywhere             ctstate RELATED,ESTABLISHED
    ACCEPT     all  --  anywhere             anywhere
    ACCEPT     all  --  anywhere             anywhere
    DOCKER     all  --  anywhere             anywhere
    ACCEPT     all  --  anywhere             anywhere             ctstate RELATED,ESTABLISHED
    ACCEPT     all  --  anywhere             anywhere
    ACCEPT     all  --  anywhere             anywhere
    
    Chain OUTPUT (policy ACCEPT)
    target     prot opt source               destination
    
    Chain DOCKER (2 references)
    target     prot opt source               destination
    ACCEPT     tcp  --  anywhere             172.18.0.4           tcp dpt:https
    ACCEPT     tcp  --  anywhere             172.18.0.4           tcp dpt:http
    
    Chain DOCKER-ISOLATION (1 references)
    target     prot opt source               destination
    DROP       all  --  anywhere             anywhere
    DROP       all  --  anywhere             anywhere
    RETURN     all  --  anywhere             anywhere

# The solution

If you check the official documentation (https://docs.docker.com/v1.5/articles/networking/), a first solution is given to limit Docker container access to one particular IP.

    $ iptables -I DOCKER -i ext_if ! -s 8.8.8.8 -j DROP

Indeed, adding a rule at the top of the DOCKER table is a good idea. It does not interfere with the rules automatically configured by Docker, and it is simple. But two major lacks :
- First, what if you need to access from two IP instead of one ? Here only one src IP can be accepted, other will be dropped without any way to prevent that.
- Second, what if your docker need access to Internet ? Pratically no request will succeed, as only the server 8.8.8.8 could respond to them.
- Finally, what if you want to add other logics ? For example, give access to any user to your webserver serving on HTTP protocol, but limit everything else to particular IP.

For the first observation, we can use *ipset*. Instead of allowing one IP in the rule above, we allow all IPs from the predefined ipset. As a bonus, the ipset can be updated without the necessity to redefine the iptable rule.

    $ iptables -I DOCKER -i ext_if -m set ! --match-set my-ipset src -j DROP

For the second observation, this is a canonical problem for firewalls : if you are allowed to contact a server through a firewall, then the firewall should authorize the server to respond to your request. This can be done by authorizing packets which are related to an established connection. For the docker logic, it gives :

    $ iptables -I DOCKER -i ext_if -m state --state ESTABLISHED,RELATED -j ACCEPT

The last observation focuses on one point : iptables rules is essential. Indeed, additional logic to ACCEPT some connections (including the one concerning ESTABLISHED connections) must be put at the top of the DOCKER table, before the DROP rule which deny all remaining connections not matching the ipset.

As we use the -I option of iptable, which inserts rules at the top of the table, previous iptables rules must be inserted by reverse order :

    // Drop rule for non matching IPs
    $ iptables -I DOCKER -i ext_if -m set ! --match-set my-ipset src -j DROP
    // Then Accept rules for established connections
    $ iptables -I DOCKER -i ext_if -m state --state ESTABLISHED,RELATED -j ACCEPT 
    $ iptables -I DOCKER -i ext_if ... ACCEPT // Then 3rd custom accept rule
    $ iptables -I DOCKER -i ext_if ... ACCEPT // Then 2nd custom accept rule
    $ iptables -I DOCKER -i ext_if ... ACCEPT // Then 1st custom accept rule

With all of this in mind, you can now check the examples which illustrate this configuration.

## Limit access on Docker containers to a set of IPs
First, install *ipset* if needed. Please refer to your distribution to know how to do it. As an example, here is the command for Debian-like distributions.

    $ apt-get update
    $ apt-get install ipset

Then create a configuration file to define an ipset containing the IPs for which you want to open access to your Docker containers.

    $ vi /etc/ipfriends.conf
    # Recreate the ipset if needed, and flush all entries
    create -exist ipfriends hash:ip family inet hashsize 1024 maxelem 65536
    flush
    # Give access to specific ips
    add ipfriends XXX.XXX.XXX.XXX
    add ipfriends YYY.YYY.YYY.YYY
    
Load this ipset.

    $ ipset restore < /etc/ipfriends.conf

Be sure that your Docker daemon is running : no error should be shown after entering the following command.

    $ docker ps

You are ready to insert your iptables rules. You **must** respect the order.

    // All requests of src ips not matching the ones from ipset ipfriends will be dropped.
    $ iptables -I DOCKER -i ext_if -m set ! --match-set ipfriends src -j DROP
    // Except for requests coming from a connection already established.
    $ iptables -I DOCKER -i ext_if -m state --state ESTABLISHED,RELATED -j ACCEPT

If you want to create new rules, you will need to remove all custom rules you've added before inserting the new ones.

    $ iptables -D DOCKER -i ext_if -m set ! --match-set ipfriends src -j DROP
    $ iptables -D DOCKER -i ext_if -m state --state ESTABLISHED,RELATED -j ACCEPT

## Configure restriction access when Docker daemon starts
Work in progress

## Some custom iptables rules
Work in progress

