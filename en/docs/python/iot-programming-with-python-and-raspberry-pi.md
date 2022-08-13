---
title: "IoT Programming with Python and Raspberry PI"
slug: "iot-programming-with-python-and-raspberry-pi"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Example - Temperature sensor
Interfacing of DS18B20 with Raspberry pi

**Connection of DS18B20 with Raspberry pi**

[![enter image description here][1]][1]

You can see there are three terminal
 1. Vcc
 2. Gnd
 3. Data (One wire protocol)

[![enter image description here][2]][2]

**R1 is 4.7k ohm resistance for pulling up the voltage level**

 1. **Vcc** should be connected to any of the 5v or 3.3v pins of Raspberry pi (PIN : 01, 02, 04, 17).
 2. **Gnd** should be connected to any of the Gnd pins of Raspberry pi (PIN : 06, 09, 14, 20, 25).
 3. **DATA** should be connected to (PIN : 07)

**Enabling the one-wire interface from the RPi side**

 4. Login to Raspberry pi using putty or any other linux/unix terminal.
 5. After login, open the /boot/config.txt file in your favourite browser.

    nano /boot/config.txt

 6. Now add the this line `dtoverlay=w1–gpio` to the end of the file.

 7. Now reboot the Raspberry pi `sudo reboot`.

 8. Log in to Raspberry pi, and run `sudo modprobe g1-gpio`

 9. Then run ` sudo modprobe w1-therm`

 10. Now go to the directory /sys/bus/w1/devices `cd /sys/bus/w1/devices`

 11. Now you will found out a virtual directory created of your temperature sensor starting from 28-********.

 12. Go to this directory `cd 28-********`

 13. Now there is a file name **w1-slave**, This file contains the temperature and other information like CRC. `cat w1-slave`.

**Now write a module in python to read the temperature**

    import glob
    import time
    
    RATE = 30
    sensor_dirs = glob.glob("/sys/bus/w1/devices/28*")

    if len(sensor_dirs) != 0:
        while True:
            time.sleep(RATE)
            for directories in sensor_dirs:
                temperature_file = open(directories + "/w1_slave")
                # Reading the files
                text = temperature_file.read()
                temperature_file.close()
                # Split the text with new lines (\n) and select the second line.
                second_line = text.split("\n")[1]
                # Split the line into words, and select the 10th word
                temperature_data = second_line.split(" ")[9]
                # We will read after ignoring first two character.
                temperature = float(temperature_data[2:])
                # Now normalise the temperature by dividing 1000.
                temperature = temperature / 1000
                print 'Address : '+str(directories.split('/')[-1])+', Temperature : '+str(temperature)

Above python module will print the temperature vs address for infinite time. RATE parameter is defined to change or adjust the frequency of temperature query from the sensor.

GPIO pin diagram 

 1. [https://www.element14.com/community/servlet/JiveServlet/previewBody/73950-102-11-339300/pi3_gpio.png][3]


  [1]: https://i.stack.imgur.com/OBA2X.png
  [2]: https://i.stack.imgur.com/dSFFQ.png
  [3]: https://www.element14.com/community/servlet/JiveServlet/previewBody/73950-102-11-339300/pi3_gpio.png

