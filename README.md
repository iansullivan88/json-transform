# json-transform

A command line tool and Haskell library for transforming json values, for example:

input:
```
{  
    "school":"Bear Academy",
    "courses":[  
        {  
            "id":2,
            "name":"Honey Stealing",
            "startDate":{  
                "day":"02",
                "month":"12",
                "year":"2017"
            },
            "enrolled":12
        },
        {  
            "id":2,
            "name":"Salmon Fishing",
            "startDate":{  
                "day":"27",
                "month":"01",
                "year":"2018"
            },
            "enrolled":10
        }
    ],
    "address":"The Forrest",
    "motto":"Ursus docere"
}
```

transform:
```
{  
    "mySchool":"$(school)",
    "myName":"Paddington",
    "myCourses[$c <- $(courses)]":{  
        "name":"$c(name)",
        "startDate":"$c(startDate.year)-$c(startDate.month)-$c(startDate.day)",
        "enrolled":"$c(enrolled)"
    }
}
```

output:
```
{  
    "mySchool":"Bear Academy",
    "myName":"Paddington",
    "myCourses":[  
        {  
            "name":"Honey Stealing",
            "startDate":"2017-12-02",
            "enrolled":12
        },
        {  
            "name":"Salmon Fishing",
            "startDate":"2018-01-27",
            "enrolled":10
        }
    ]
}
```

## CLI

```
Usage: json-transform (-i|--input FILENAME) (-t|--transform FILENAME)

Available options:
  -i,--input FILENAME      a path to the input file or - to read from stdin
  -t,--transform FILENAME  a path to the transform file or - to read from stdin
```

## Why

Just for fun
