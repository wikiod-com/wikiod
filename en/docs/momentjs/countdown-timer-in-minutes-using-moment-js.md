---
title: "countdown timer in minutes using moment.js"
slug: "countdown-timer-in-minutes-using-momentjs"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## countdown timer using moment.js
   

 **HTML**  

    <div class="countdown"></div>

**JS**

    var duration = moment.duration({
          'minutes': 5,
          'seconds': 00
        
        });
    
    var timestamp = new Date(0, 0, 0, 2, 10, 30);
    var interval = 1;
    var timer = setInterval(function() {
      timestamp = new Date(timestamp.getTime() + interval * 1000);
    
      duration = moment.duration(duration.asSeconds() - interval, 'seconds');
      var min = duration.minutes();
      var sec = duration.seconds();
    
      sec -= 1;
      if (min < 0) return clearInterval(timer);
      if (min < 10 && min.length != 2) min = '0' + min;
      if (sec < 0 && min != 0) {
        min -= 1;
        sec = 59;
      } else if (sec < 10 && sec.length != 2) sec = '0' + sec;
    
      $('.countdown').text(min + ':' + sec);
      if (min == 0 && sec == 0)
        clearInterval(timer);
    
    
    }, 1000);

   

 

**Putting it all together**

  

      <!DOCTYPE html>
    <html>
    <head>
        <title>FINAL COUNTDOWN</title>
        <style type="text/css">
            /* Styling it all now! */
    
            @font-face {
                font-family: 'digi';
                src: url('http://cssdeck.com/uploads/resources/fonts/digii/DS-DIGII.TTF');
            }
    
            * {
                box-sizing: border-box;
            }
    
            html, body {
                width: 100%; height: 100%;
            }
    
            body {
                font-family: 'digi';
                font-size: 76%;
                background: #000;
                background-image: radial-gradient(
                    center 0,
                    white,
                    #B9B7B1
                );
            }
    
            .countdown {
                width: 250px; height: 100px;
                background: white;
                font-size: 4em;
                color: #707070;
                
                /* Centering everything */
                position: absolute;
                left: 50%; top: 50%;
                margin-left: -125px; margin-top: -50px;
                
                text-align: center;
                line-height: 100px;
                
                border-top: 5px solid #E54B6B;
              border-bottom: 5px solid #E54B6B;
                background-image: linear-gradient(#f0f0f0, white);
            }
        </style>
        <script
      src="https://code.jquery.com/jquery-3.2.1.js"
      integrity="sha256-DZAnKJ/6XZ9si04Hgrsxu/8s717jcIzLy3oi35EouyE="
      crossorigin="anonymous"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.5.0/moment.min.js"></script>
    </head>
    <body>
    <div class="countdown"></div>
    </body>
    </html>
    <script type="text/javascript">
        var duration = moment.duration({
          'minutes': 5,
          'seconds': 00
    
        });
    
        var timestamp = new Date(0, 0, 0, 2, 10, 30);
        var interval = 1;
        var timer = setInterval(function() {
          timestamp = new Date(timestamp.getTime() + interval * 1000);
    
          duration = moment.duration(duration.asSeconds() - interval, 'seconds');
          var min = duration.minutes();
          var sec = duration.seconds();
    
          sec -= 1;
          if (min < 0) return clearInterval(timer);
          if (min < 10 && min.length != 2) min = '0' + min;
          if (sec < 0 && min != 0) {
            min -= 1;
            sec = 59;
          } else if (sec < 10 && sec.length != 2) sec = '0' + sec;
    
          $('.countdown').text(min + ':' + sec);
          if (min == 0 && sec == 0)
            clearInterval(timer);
    
    
        }, 1000);
    
    </script>

**Fiddle :** https://jsfiddle.net/pahday/fyxu66hk/6/


  [1]: https://jsfiddle.net/pahday/fyxu66hk/6/

