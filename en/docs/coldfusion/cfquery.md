---
title: "cfquery"
slug: "cfquery"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Parameters
| **Parameter**  | **Details** |
| -------------- | ----------- |
| name       | Value: *string*, Default: yes |
| dbtype     | Value: query/hql, Default: no, Remarks: when left blank, it's a normal query |
| datasource | Default: no, Remarks: database |
| params     | Value: *structure*, Default: no, Remarks: cfscript syntax only! In cfml they are written inside SLQ stament using `<cfqueryparam />` |


## Query of Query
Function Calls
-------------

    <!--- Load the user object based on the component path. --->
    <cfset local.user = new com.User() />
    <cfset local.allUsers = user.getAllUsers()>
    <cfset local.specificUser = user.getUserIdFromQry(qry = local.allUsers, userId = 1)>   

User.cfc
--------

    <cfcomponent>
        <cffunction name="getAllUsers" access="public" returntype="query">
            <cfquery name="local.qryGetAllUsers" datasource="DATABASE_NAME">
                SELECT  id,
                        name
                FROM    user
            </cfquery>
            
            <cfreturn local.qryGetAllUsers>     
        </cffunction>
    
        <cffunction name="getUserIdFromQry" access="public" returntype="query">
            <cfargument name="qry" type="query" required="Yes" hint="Query to fetch from">
            <cfargument name="userId" type="numeric" required="Yes" hint="The ID of the user">
            
            <cfquery name="local.qryGetUserIdFromQry" dbtype="query">
                SELECT  id,
                        name
                FROM    arguments.qry
                WHERE   id = <cfqueryparam value="#arguments.userId#" cfsqltype="cf_sql_integer">
            </cfquery>
            
            <cfreturn local.qryGetUserIdFromQry>     
        </cffunction>
    </component>

## Using cfquery within a Function
    <cffunction name="getUserById" access="public" returntype="query">
        <cfargument name="userId" type="numeric" required="yes" hint="The ID of the user">
    
        <cfquery name="local.qryGetUser" datasource="DATABASE_NAME">
             SELECT  id,
                     name
             FROM    user
             WHERE   id = <cfqueryparam value="#arguments.userId#" cfsqltype="cf_sql_integer">
        </cfquery>

        <cfreturn local.qryGetUser>
    </cffunction>


