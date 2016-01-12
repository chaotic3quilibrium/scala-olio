import org.json4s.native.JsonMethods._

val json = """
 {
  "transaction": {
    "endPoint": "v20150317.byAliLocator",
    "uuid": "5bfcc724-6640-41da-b97b-07423bd629b0",
    "utcDateTimeStamp": "20150417T225646Z",
    "status": {
      "code": "Success",
      "description": "Completed without error"
    },
    "client": {
      "trackingValue": "jim.oflaherty.jr@gmail.com",
      "ip": "127.0.0.1"
    },
    "generatedInMillis": 364
  },
  "request": {
    "locations": [
      {
        "trackingLabel": "fc75591e-84a0-4b16-baa2-5022d721e660",
        "aliLocatorValue": "B7LJ0C6DYAYW"
      }
    ]
  },
  "response": {
    "locations": [
      {
        "trackingLabel": "fc75591e-84a0-4b16-baa2-5022d721e660",
        "status": {
          "code": "Success",
          "description": "Completed without error"
        },
        "detail": {
          "aliLocator": {
            "value": "B7LJ0C6DYAYW",
            "input": {
              "isInputDifferent": false,
              "isInputDeprecated": false
            }
          },
          "streetAddress": {
            "country": "USA",
            "postalCode": {
              "primary": "75081",
              "secondary": "5103"
            },
            "stateProvinceRegionCode": "TX",
            "city": "Richardson",
            "street": "520 E Spring Valley Rd",
            "singleLine": "520 E Spring Valley Rd, Richardson, Texas, 75081-5103, USA"
          },
          "geoCoordinate": {
            "longitude": "-96.722763",
            "latitude": "32.939667"
          }
        }
      }
    ]
  }
}
"""
val jValue =
  parse(json)
val listTuple2 =
  jValue.toKeyValues()
