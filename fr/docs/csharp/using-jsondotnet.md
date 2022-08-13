---
title: "Utiliser json.net"
slug: "utiliser-jsonnet"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Utilisation de la classe [JSON.net](http://www.newtonsoft.com/json) [JsonConverter](http://www.newtonsoft.com/json/help/html/T_Newtonsoft_Json_JsonConverter.htm).

## Utilisation de JsonConverter sur des valeurs simples
Exemple d'utilisation de JsonCoverter pour désérialiser la propriété d'exécution de la réponse API dans un objet [Timespan](https://msdn.microsoft.com/en-us/library/system.timespan(v=vs.110).aspx) dans le Modèle de films

# JSON (http://www.omdbapi.com/?i=tt1663662)

    {
        Title: "Pacific Rim",
        Year: "2013",
        Rated: "PG-13",
        Released: "12 Jul 2013",
        Runtime: "131 min",
        Genre: "Action, Adventure, Sci-Fi",
        Director: "Guillermo del Toro",
        Writer: "Travis Beacham (screenplay), Guillermo del Toro (screenplay), Travis Beacham (story)",
        Actors: "Charlie Hunnam, Diego Klattenhoff, Idris Elba, Rinko Kikuchi",
        Plot: "As a war between humankind and monstrous sea creatures wages on, a former pilot and a trainee are paired up to drive a seemingly obsolete special weapon in a desperate effort to save the world from the apocalypse.",
        Language: "English, Japanese, Cantonese, Mandarin",
        Country: "USA",
        Awards: "Nominated for 1 BAFTA Film Award. Another 6 wins & 46 nominations.",
        Poster: "https://images-na.ssl-images-amazon.com/images/M/MV5BMTY3MTI5NjQ4Nl5BMl5BanBnXkFtZTcwOTU1OTU0OQ@@._V1_SX300.jpg",
        Ratings: [{
                Source: "Internet Movie Database",
                Value: "7.0/10"
            },
            {
                Source: "Rotten Tomatoes",
                Value: "71%"
            },
            {
                Source: "Metacritic",
                Value: "64/100"
            }
        ],
        Metascore: "64",
        imdbRating: "7.0",
        imdbVotes: "398,198",
        imdbID: "tt1663662",
        Type: "movie",
        DVD: "15 Oct 2013",
        BoxOffice: "$101,785,482.00",
        Production: "Warner Bros. Pictures",
        Website: "http://pacificrimmovie.com",
        Response: "True"
    }

# Modèle de film

    using Project.Serializers;
    using Newtonsoft.Json;
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Runtime.Serialization;
    using System.Threading.Tasks;
    
    namespace Project.Models
    {
        [DataContract]
        public class Movie
        {
            public Movie() { }
    
            [DataMember]
            public int Id { get; set; }
    
            [DataMember]
            public string ImdbId { get; set; }
    
            [DataMember]
            public string Title { get; set; }
    
            [DataMember]
            public DateTime Released { get; set; }
    
            [DataMember]
            [JsonConverter(typeof(RuntimeSerializer))]
            public TimeSpan Runtime { get; set; }
    
        }
    }

# RuntimeSerializer

    using Newtonsoft.Json;
    using Newtonsoft.Json.Linq;
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text.RegularExpressions;
    using System.Threading.Tasks;
    
    namespace Project.Serializers
    {
        public class RuntimeSerializer : JsonConverter
        {
            public override bool CanConvert(Type objectType)
            {
                return objectType == typeof(TimeSpan);
            }
    
            public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
            {
                if (reader.TokenType == JsonToken.Null)
                    return null;
    
                JToken jt = JToken.Load(reader);
                String value = jt.Value<String>();
    
                Regex rx = new Regex("(\\s*)min$");
                value = rx.Replace(value, (m) => "");
    
                int timespanMin; 
                if(!Int32.TryParse(value, out timespanMin))
                {
                    throw new NotSupportedException();
                }
    
                return new TimeSpan(0, timespanMin, 0);
            }
    
            public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer)
            {
                serializer.Serialize(writer, value);
            }
        }
    }

# L'appeler

    Movie m = JsonConvert.DeserializeObject<Movie>(apiResponse));



## Collecter tous les champs de l'objet JSON
    using Newtonsoft.Json.Linq;
    using System.Collections.Generic;

    public class JsonFieldsCollector
    {
        private readonly Dictionary<string, JValue> fields;

        public JsonFieldsCollector(JToken token)
        {
            fields = new Dictionary<string, JValue>();
            CollectFields(token);
        }

        private void CollectFields(JToken jToken)
        {
            switch (jToken.Type)
            {
                case JTokenType.Object:
                    foreach (var child in jToken.Children<JProperty>())
                        CollectFields(child);
                    break;
                case JTokenType.Array:
                    foreach (var child in jToken.Children())
                        CollectFields(child);
                    break;
                case JTokenType.Property:
                    CollectFields(((JProperty) jToken).Value);
                    break;
                default:
                    fields.Add(jToken.Path, (JValue)jToken);
                    break;
            }
        }

        public IEnumerable<KeyValuePair<string, JValue>> GetAllFields() => fields;
    }

**Usage:**

    var json = JToken.Parse(/* JSON string */);
    var fieldsCollector = new JsonFieldsCollector(json);
    var fields = fieldsCollector.GetAllFields();

    foreach (var field in fields)
        Console.WriteLine($"{field.Key}: '{field.Value}'");


**Démo**<br/>
Pour cet objet JSON

    {
      "User": "John",
      "Workdays": {
        "Monday": true,
        "Tuesday": true,
        "Friday": false
      },
      "Age": 42
    }

la sortie attendue sera :

    User: 'John'
    Workdays.Monday: 'True'
    Workdays.Tuesday: 'True'
    Workdays.Friday: 'False'
    Age: '42'

