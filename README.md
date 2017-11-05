# json-transform

A Haskell library for transforming json values.

input:
 {
     "orderId": 12345,
     "date": {
        "day":"03",
        "month":"04",
        "year":"2005"
     },
     "extra-info": {
        "foo":"bar",
        "fizz":"buzz"
     }
 }

transform:
 {
    "id": "$(orderId)",
    "dateString": "$(date.year)-$(date.month)-$(date.day)",
    "type": "order"
 }

output:
 {
    "id": 12345,
    "dateString": "2005-04-03",
    "type": "order"
 }
