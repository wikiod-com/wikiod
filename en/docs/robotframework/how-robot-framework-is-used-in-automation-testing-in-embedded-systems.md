---
title: "How robot framework is used in Automation testing in Embedded Systems?"
slug: "how-robot-framework-is-used-in-automation-testing-in-embedded-systems"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Robot framework is widely used in Automation testing of Embedded products.
We are going to take an Embedded product as an example and see how to automate the test cases using Robot Framework.



Abbreviation:
* RPS - Remote power supply
* RF - Robot frame work

## Remote Power Supply Testing
## Remote Power supply simulation
Since we don't have a real remote power supply hardware, we are going to simulate it
using python program.

### Basic idea about RPS
- Actually remote power supply has a http server. 
- User can send commands to turn ON/OFF power supply using http request.

We are going to simulate remote power supply using following program rps-server.py.

    from flask import Flask, request
    from flask_httpauth import HTTPBasicAuth


    app = Flask(__name__)
    auth = HTTPBasicAuth()

    users = {
            'admin': '12345678'
    }
    app.url_map.strict_slashes = False

    PINS = ['P60', 'P61', 'P62', 'P63']

    PINS_STATUS = {'P60':'0', 'P61': '0', 'P62':'0', 'P63':'0'}

    @auth.get_password
    def get_pw(username):
            if username in users:
                    return users.get(username)
            return None

    @app.route('/')
    @auth.login_required
    def index():
            return "Hello, %s!" % auth.username()

    def get_html_string():
            html_str = '<html>P60={}P61={}P62={}P63={}</html>'.format(PINS_STATUS['P60'],
                                                    PINS_STATUS['P61'],
                                                PINS_STATUS['P62'],
                                                PINS_STATUS['P63'])
            return html_str

    def parse_cmd_args(args):
            global current_status
            if str(args['CMD']) == 'SetPower':
                    for key in args:
                            if key in PINS:
                                    PINS_STATUS[key] = str(args[key])
                                
                    return get_html_string()
        
            if str(args['CMD']) == 'GetPower':
                    return get_html_string()
       

        @app.route('/SetCmd', methods=['GET','POST'])
        def rps():
            if request.method=="GET":
                    args=request.args.to_dict()
                    ret = parse_cmd_args(args)
                    return ret

The above code actually simulates http server to control the remote power supply.

## How to Run RPS server ?
    $ export FLASK_APP=rps-server.py
    $ flask run

## How to send commands to rps server ?
Following are the two commands used to control the RPS
1. SetPower
2. GetPower

By default the server will be listening at the port 5000.

The power supply ports are,

1. P60
2. P61
3. P62
4. P64

The states of the ports are, 
1. ON - 1
2. OFF - 0

## Requirements

Requirements for building a remote power supply are 
1. Remote power supply should be able to turn ON/OFF remotely
2. Remote power supply status can be accessed remotely.

## Deriving test cases

Test cases derived from requirement
1. Turn on Power supply 2 remotely.
2. Verify power supply 2 is on.
3. Turn off Power supply 2 remotely.
4. Verify power supply 2 is off.

## Manual Testing

* Run the rps server.
* To turn on Port 3, open a browser and give following URI

`http://admin:12345678@localhost:5000/SetCmd?CMD=SetPower&P62=1`
* To get the status of all the ports

`http://admin:12345678@localhost:5000/SetCmd?CMD=GetPower`


# Writing test library

We need to write a test library in python for sending http commands using http request. Later we will be using this library as keywords in robot frame work.

## commands.py

We are going to use library from commands.py to send SetPower and GetPower.


    import requests
    import re
    

    class commands(object):
    
        ROBOT_LIBRARY_SCOPE = 'GLOBAL'
        def __init__(self, ip='localhost:5000'):
            self.ip_address = ip
            self.query = {}
            self.user = 'admin'
            self.passw = '12345678'

        def form_query(self, state, cmd, port):
            port = self.get_port_no(port)
            self.query = {port: state}
            return self.query

        def get_port_no(self, port_no):
            port = 'P6' + str(port_no)
            return port

        def clean_html(self, data):
            exp = re.compile('<.*?>')
            text = re.sub(exp, "", data)
            return text.rstrip()

        def send_cmds(self, cmd, port=None, state=None):
            url = 'http://{}:{}@{}/SetCmd?CMD={}'\
                  .format(self.user,
                          self.passw,
                          self.ip_address,
                          cmd)
            print url
            if cmd == 'SetPower':
                self.form_query(state, cmd, port)
                self.req = requests.get(url, params=self.query)
                return True
            elif cmd == 'GetPower':
                self.req = requests.get(url)
                data = self.clean_html(self.req.text)
                return data
            else:
                return False

            return self.req.text


    # c = commands('localhost:5000')

    # c.send_cmds('SetPower', 2, 1)
    # c.send_cmds('SetPower', 3, 1)
    # print c.send_cmds('GetPower')

## Python key word documentation

1. `send_cmds(cmd, port=None, state=None)` is the function we are going to use.
2.  While using this function in Robot key word, no need to bother about `_`, or `Lowercaser` or `Uppercase` in function name.

Python function will look like this while using as keyword,

    Send Cmds       cmd   port  state
    
 


## Writing test Keywords

We are going to use `Send Cmds` as python keyword in test suite.

* RPS send commands uses following four arguments to set power
    - command = SetPower
    - port  = 2
    - state = 1 for ON / 0 for off
  When we call that command it will turn ON/OFF the power supply

* RPS get power will return the status of all the Power supply ports


    *** Keywords ***
    RPS send commands
        [Arguments]    ${command}    ${port}    ${state}
        ${output}=    Send cmds    ${command}  ${port}  ${state}
        [return]    ${output}

    RPS get Power
        [Arguments]    ${command}
        ${output}=    Send cmds    ${command}
        [return]    ${output}}

### Algorithm to test power supply

1. Set power to a port
2. Check the status of cmd 
3. Get the status of the port and check whether it is ON/OFF



## Writing test cases using the above key words
    
Now we are ready to write test case using following two keywords

 * RPS send commands  - To set and unset a power of port
 * RPS get power      - To get the status of all the port


    *** Settings ***
    Library      commands.py
    
    *** Test Cases ***
    Turn on Power supply 2 remotely
         ${out}=    RPS send commands     SetPower  2  1
         Should be equal    ${out}  ${True}

    Verify power supply 2 is on
        ${out}=     RPS get power    GetPower
        should contain    ${out}  P62=1

    Turn off Power supply 2 remotely
         ${out}=    RPS send commands     SetPower  2  0
         Should be equal    ${out}  ${True}

    Verify power supply 2 is off
        ${out}=     RPS get power    GetPower
        should contain    ${out}  P62=0


Create a file name `remote-power-supply.robot`

Copy above key words and test case in to the file.


## How to execute RPS server and remote-power-supply.robot ?

* Run remote power supply first
* Run the test suite remote-power-supply.robot

  
    $ export FLASK_APP=rps-server.py
    $ flask run
    $ pybot remote-power-supply.robot

## Output

    $ pybot remote-pwer-supply.robot 
    ==============================================================================
    Remote-Pwer-Supply                                                            
    ==============================================================================
    Turn on Power supply 2 remotely                                       | PASS |
    ------------------------------------------------------------------------------
    Verify power supply 2 is on                                           | PASS |
    ------------------------------------------------------------------------------
    Turn off Power supply 2 remotely                                      | PASS |
    ------------------------------------------------------------------------------
    Verify power supply 2 is off                                          | PASS |
    ------------------------------------------------------------------------------    
    Remote-Pwer-Supply                                                    | PASS |
    4 critical tests, 4 passed, 0 failed
    4 tests total, 4 passed, 0 failed
    ==============================================================================
    Output:  /tmp/talks/robot-framework-intro/test-cases/output.xml
    Log:     /tmp/talks/robot-framework-intro/test-cases/log.html
    Report:  /tmp/talks/robot-framework-intro/test-cases/report.html


# Following two diagrams explains about test architecture between  RPS and RF
## Remote Power supply test architecture
[![Remote power supply test architecture][1]][1]
 

## Robot frame work architecture
[![enter image description here][2]][2]


## Credits

Thanks to robot framework for architecture diagram.

# The complete code is available here

[Remote power supply][3]

[commands.py][4]

[remote-power-supply.robot][5]

  [1]: https://i.stack.imgur.com/mueKw.png
  [2]: https://i.stack.imgur.com/fsvRa.png
  [3]: https://github.com/gnurenga/talks/tree/master/remote-power-supply
  [4]: https://github.com/gnurenga/talks/tree/master/robot-framework-intro/library/rps
  [5]: https://github.com/gnurenga/talks/tree/master/robot-framework-intro

