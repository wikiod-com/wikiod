---
title: "WCSessionDelegate"
slug: "wcsessiondelegate"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

`WCSessionDelegate` works with watch OS2 + using WatchConnectivity.

 

      var watchSession : WCSession?

     func startWatchSession(){
            
            if(WCSession.isSupported()){
                watchSession = WCSession.default()
                watchSession!.delegate = self
                watchSession!.activate()
            }
        }

Implement the required method:- `didReceiveApplicationContext`

## Watch kit controller (WKInterfaceController)
    import WatchConnectivity
    
    var watchSession : WCSession?
    
        override func awake(withContext context: Any?) {
            super.awake(withContext: context)
            // Configure interface objects here.
            startWatchSession()
        }
    
    func startWatchSession(){
            
            if(WCSession.isSupported()){
                watchSession = WCSession.default()
                watchSession!.delegate = self
                watchSession!.activate()
            }
        }
        
    //Callback in below delegate method when iOS app triggers event
    func session(_ session: WCSession, didReceiveApplicationContext applicationContext: [String : Any]) {
            print("did ReceiveApplicationContext at watch")
        }

