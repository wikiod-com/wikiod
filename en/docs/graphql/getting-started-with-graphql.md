---
title: "Getting started with graphql"
slug: "getting-started-with-graphql"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## GraphQL Query Language (GQL)
Instead of defining URL endpoints for each data resource, in GraphQL you define a single endpoint that accepts GraphQL queries. Unlike traditional database query languages, GraphQL Query Language (GQL) is a projection of data returned by a root level query. The GraphQL schema will define the data model and root level queries and mutations. GQL executes those queries and defines what data to return.

## Query for All Film Titles from Star Wars Schema

[![Star Wars data query for allFilms with a projection of only the title][2]][2]
*[GraphiQL Query][3]*


## Query for Planet Residents to Star Wars film "A New Hope"
[![enter image description here][4]][4]
*[GraphiQL Query][5]*

Notice how the root query `film()` takes in the id parameter of the film "A New Hope" and the projection from GQL returns the title, planets connected to the film, and then resident names of the planets. Check out the [Star Wars GraphiQL][6] site to experiment with querying in GraphQL.


  [1]: http://graphql.org/
  [2]: https://i.stack.imgur.com/yurqO.png
  [3]: http://graphql.org/swapi-graphql/?query=query%7B%0A%20%20allFilms%7B%0A%20%20%20%20films%7B%0A%20%20%20%20%20%20title%0A%20%20%20%20%7D%0A%20%20%7D%0A%7D
  [4]: https://i.stack.imgur.com/bbCAW.png
  [5]: http://graphql.org/swapi-graphql/?query=query%7B%0A%20%20film(id%3A%20%22ZmlsbXM6MQ%3D%3D%22)%7B%0A%20%20%20%20id%0A%20%20%20%20title%2C%0A%20%20%20%20planetConnection%7B%0A%20%20%20%20%20%20planets%7B%0A%20%20%20%20%20%20%20%20name%0A%20%20%20%20%20%20%20%20residentConnection%7B%0A%20%20%20%20%20%20%20%20%20%20residents%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20name%0A%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%7D%0A%20%20%20%20%7D%0A%20%20%7D%0A%7D
  [6]: http://graphql.org/swapi-graphql/

## Schema Definition
In [GraphQL][1] the Schema defines the root execution [queries and mutations][2] as well as the [types][3] for your data.

## Schema Object Type

The `Person` type has two fields, one is a standard Scalar type and the other represents a relationship to a list of other Person types that are 'friends'. Linking other types is what makes GraphQL so powerful. Now in GraphQL Query Language (GQL), the client can traverse the friends graph without any additional code or advanced querying.

    type Person {
      id: ID
      name: String
      friends: [Person]
    }

## Schema Query

The `person` query looks up a single person by it's id. This is the entry point to your data for clients using GQL. 

    type Query {   
      person(id: ID!): Person
    }

## Query Nick's Friend's Friend's Friends

Now that we have a Person type and a person root query we can look up a person and as many degrees of separation we want of the person's friends network.

    query {
      person(id: 'nick'){
        id
        name
        friends{
          id
          name
          friends{
            id
            name
            friends{
              id
              name
            }
          }
        }
      }
    }

  [1]: http://graphql.org/
  [2]: http://graphql.org/learn/schema/#the-query-and-mutation-types
  [3]: http://graphql.org/learn/schema/#type-system

## Server Installation & Implementation
## GraphQL.js ##

[GraphQL.js][1] is a JavaScript reference implementation for GraphQL. You can install it via npm:

 - Initialize npm in your project if you have not done so already: `npm init`
 - Install GraphQL.js from npm: `npm install --save graphql`

### Example Server ###

    var { graphql, buildSchema } = require('graphql');
    
    // Construct a schema, using GraphQL schema language
    var schema = buildSchema(`
      type Query {
        hello: String
      }
    `);
    
    // The root provides a resolver function for each API endpoint
    var root = {
      hello: () => {
        return 'Hello world!';
      },
    };
    
    // Run the GraphQL query '{ hello }' and print out the response
    graphql(schema, '{ hello }', root).then((response) => {
      console.log(response);
    });

### Server Middleware Alternatives ###

 - [Express GraphQL Middleware][2]
 - [Hapi GraphQL Middleware][3]
 - [Koa GraphQL Middleware][4]


  [1]: http://graphql.org/graphql-js/
  [2]: http://graphql.org/graphql-js/running-an-express-graphql-server/
  [3]: https://github.com/SimonDegraeve/hapi-graphql
  [4]: https://github.com/chentsulin/koa-graphql

