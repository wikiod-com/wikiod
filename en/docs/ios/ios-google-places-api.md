---
title: "iOS Google Places API"
slug: "ios-google-places-api"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Getting Nearby Places from Current Location
Prerequisites 
1) Install pods in your project
2) Install the GooglePlaces SDK
3) Enable location services

First we need to get the users location by getting their current longitude and latitude.

1) Import GooglePlaces and GooglePlacePicker

   

    import GooglePlaces
    import GooglePlacePicker

2) Add the `CLLOcationManagerDelegate` protocol


    class ViewController: UIViewController, CLLocationManagerDelegate {
        
    }

3) create your CLLocationManager()


    var currentLocation = CLLocationManager()

4) Request authorization


    
    currentLocation = CLLocationManager()
    currentLocation.requetAlwayAuthorization()


5) Create a button to call the GooglePlacePicker method

@IBAction func placePickerAction(sender: AnyObject) {

    if CLLOcationManager.authorizationStatues() == .AuthorizedAlways {

            let center = CLLocationCoordinate2DMake((currentLocation.location?.coordinate.latitude)!, (currentLocation.location?.coordinate.longitude)!)
            let northEast = CLLocationCoordinate2DMake(center.latitude + 0.001, center.longitude + 0.001)
            let southWest = CLLocationCoordinate2DMake(center.latitude - 0.001, center.longitude - 0.001)
            let viewport = GMSCoordinateBounds(coordinate: northEast, coordinate: southWest)
            let config = GMSPlacePickerConfig(viewport: viewport)
            placePicker = GMSPlacePicker(config: config)
            
            placePicker?.pickPlaceWithCallback({ (place: GMSPlace?, error: NSError?) -> Void in
                if let error = error {
                    print("Pick Place error: \(error.localizedDescription)")
                    return
                }
                
                if let place = place {
                   print("Place name: \(place.name)")
                    print("Address: \(place.formattedAddress)")
                    
                } else {
                   print("Place name: nil")
                    print("Address: nil")
                }
            })
        }        
    }

