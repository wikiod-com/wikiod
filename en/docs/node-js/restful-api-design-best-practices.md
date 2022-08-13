---
title: "Restful API Design Best Practices"
slug: "restful-api-design-best-practices"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Error Handling: GET all resources
How do you handle errors, rather then log them to the console?

Bad way:

    Router.route('/')
      .get((req, res) => {
        Request.find((err, r) => {
          if(err){
            console.log(err)
          } else {
            res.json(r)
          }
        })
      })
      .post((req, res) => {
        const request = new Request({
          type: req.body.type,
          info: req.body.info
        });
        request.info.user = req.user._id;
        console.log("ABOUT TO SAVE REQUEST", request);
        request.save((err, r) => {
          if (err) {
            res.json({ message: 'there was an error saving your r' });
          } else {
            res.json(r);
          }
        });
      });

Better way:


    Router.route('/')
     .get((req, res) => {
      Request.find((err, r) => {
       if(err){
        console.log(err)
      } else {
        return next(err)
      }
    })
    })
    .post((req, res) => {
    const request = new Request({
      type: req.body.type,
      info: req.body.info
    });
    request.info.user = req.user._id;
    console.log("ABOUT TO SAVE REQUEST", request);
    request.save((err, r) => {
      if (err) {
        return next(err)
      } else {
        res.json(r);
      }
    });
    });

