---
title: "Getting started with avfoundation"
slug: "getting-started-with-avfoundation"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
A minimal setup for camera output preview (Swift 2, Swift 3) 


    import UIKit
    import AVFoundation

    class ViewController: UIViewController {
        var session: AVCaptureSession?
        var cameraPreviewLayer: AVCaptureVideoPreviewLayer?

        override func viewDidLoad() {
            super.viewDidLoad()
            setupSession()
            if let cameraPreviewLayer = AVCaptureVideoPreviewLayer(session: session) {
                view.layer.addSublayer(cameraPreviewLayer)
                self.cameraPreviewLayer = cameraPreviewLayer
                session?.startRunning()
            }
        }

        func setupSession() {
            session = AVCaptureSession()
            //setup input
            let device =  AVCaptureDevice.defaultDevice(withMediaType: AVMediaTypeVideo)
            do {
                let input = try AVCaptureDeviceInput(device: device)
                if session?.canAddInput(input) == true {
                    session?.addInput(input)
                }
            } catch {
                print("An error occured: \(error.localizedDescription)")
            }


            //setup output
            let output = AVCaptureVideoDataOutput()
            output.videoSettings = [kCVPixelBufferPixelFormatTypeKey as AnyHashable: kCVPixelFormatType_32BGRA]
            if session?.canAddOutput(output) == true {
                session?.addOutput(output)
            }
        
        }

        override func viewDidLayoutSubviews() {
            super.viewDidLayoutSubviews()
            self.cameraPreviewLayer?.frame = self.view.layer.bounds
        }
    }




