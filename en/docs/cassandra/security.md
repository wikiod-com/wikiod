---
title: "Security"
slug: "security"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

# Cassandra security resources
- [CQL: Database roles syntax definition](https://cassandra.apache.org/doc/cql3/CQL.html#databaseRoles)
- [CQL: List of object permissions](https://cassandra.apache.org/doc/cql3/CQL.html#dataControl)
- [DataStax Documentation: Internal authentication](http://docs.datastax.com/en/cassandra/2.2/cassandra/configuration/secureInternalAuthenticationTOC.html)
- [DataStax Documentation: Internal authorization](http://docs.datastax.com/en/cassandra/2.2/cassandra/configuration/secureInternalAuthorizationTOC.html)

## Configuring internal authentication
Cassandra will not require users to login using the default configuration. Instead password-less, anonymous logins are permitted for anyone able to connect to the `native_transport_port`. This behaviour can be changed by editing the `cassandra.yaml` config to use a different authenticator:

```yaml
# Allow anonymous logins without authentication
# authenticator: AllowAllAuthenticator

# Use username/password based logins
authenticator: PasswordAuthenticator
```

The login credentials validated by `PasswordAuthenticator` will be stored in the internal `system_auth` keyspace. By default, the keyspace will not be replicated accross all nodes. You'll have to change the replication settings to make sure that Cassandra will still be able to read user credentials from local storage in case other nodes in the cluster cannot be reached, or else you might not be able to login!

For `SimpleStrategy` (where `N` is the number of nodes in your cluster):

```sql
ALTER KEYSPACE system_auth WITH replication = {'class': 'SimpleStrategy', 'replication_factor': N};
```

For `NetworkTopologyStrategy` (where `N` is the number of nodes in the corresponding data center):

```sql
ALTER KEYSPACE system_auth WITH replication =  { 'class' : 'NetworkTopologyStrategy', 'datacenter1' : N };
```

Restart each node after the changes described above. You'll now only be able to login using the default superuser:

`cqlsh -u cassandra -p cassandra`

## (Optional) Replace default superuser with custom user

Using a default superuser with a standard password isn't much safer than using no user at all. You should create your own user instead using a safe and unique password:

```sql
CREATE ROLE myadminuser WITH PASSWORD = 'admin123' AND LOGIN = true AND SUPERUSER = true;
```

Log in using your new user: `cqlsh -u myadminuser -p admin123`

Now disable login for the standard cassandra user and remove the superuser status:

`ALTER ROLE cassandra WITH LOGIN = false AND SUPERUSER = false;`


## Configuring internal authorization
By default each user will be able to access all data in Cassandra. You'll have to configuring a different authorizer in your `cassandra.yaml` to grant individual object permissions to your users.

```yaml
# Grant all permissions to all users
# authorizer: AllowAllAuthorizer

# Use object permissions managed internally by Cassandra
authorizer: CassandraAuthorizer
```

Permissions for individual users will be store in the internal `system_auth` keyspace. You should change the replication settings in case you haven't already done so while enabling password based authentication. 


For `SimpleStrategy` (where `N` is the number of nodes in your cluster):

```sql
ALTER KEYSPACE system_auth WITH replication = {'class': 'SimpleStrategy', 'replication_factor': N};
```

For `NetworkTopologyStrategy` (where `N` is the number of nodes in the corresponding data center):

```sql
ALTER KEYSPACE system_auth WITH replication =  { 'class' : 'NetworkTopologyStrategy', 'datacenter1' : N };
```

Restart each node after the changes described above. You'll now be able to set permissions using e.g. the following commands.

Grants all permissions for specified keyspace and role:

`GRANT ALL ON KEYSPACE keyspace_name TO role_name;`

Grant read permissions on all keyspaces:

`GRANT SELECT ON ALL KEYSPACES TO role_name;`

Allow execution of INSERT, UPDATE, DELETE and TRUNCATE statements on a certain keyspace:

`GRANT MODIFY ON KEYSPACE keyspace_name TO role_name;`

Allow changing keyspaces, tables and indices for certain keyspace:

`GRANT ALTER ON KEYSPACE keyspace_name TO role_name;`


Please note that permissions will be cached for `permissions_validity_in_ms` (`cassandra.yaml`) and changes might not be effective instantly. 


