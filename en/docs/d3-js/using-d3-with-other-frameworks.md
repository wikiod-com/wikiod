---
title: "Using D3 with other frameworks"
slug: "using-d3-with-other-frameworks"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## D3js with Angular
Using D3js with Angular can open up new fronts of possibilities such as live updation of charts as soon as data is updated. We can encapsulate complete chart functionality within an Angular directive, which makes it easily reusable.

***index.html*** >>

    <!DOCTYPE html>
    <html ng-app="myApp">
    <head>
      <script src="https://d3js.org/d3.v4.min.js"></script>
      <script data-require="angular.js@1.4.1" data-semver="1.4.1" src="https://code.angularjs.org/1.4.1/angular.js"></script>
      <script src="app.js"></script>
      <script src="bar-chart.js"></script>
    </head>
    
    <body>
    
      <div ng-controller="MyCtrl">
        <!-- reusable d3js bar-chart directive, data is sent using isolated scope -->
        <bar-chart data="data"></bar-chart>
      </div>
    
    </body>
    </html>

We can pass the data to the chart using controller, and watch for any changes in the data to enable live updation of chart in the directive:

***app.js*** >>

    angular.module('myApp', [])
      .controller('MyCtrl', function($scope) {
        $scope.data = [50, 40, 30];
        $scope.$watch('data', function(newVal, oldVal) {
          $scope.data = newVal;
        }, true);
      });

Finally, the directive definition. The code we write to create and manipulate the chart will sit in the link function of the directive.

Note that we have put a scope.$watch in the directive too, to update as soon as the controller passes new data. We are re-assigning new data to our data variable if there is any data change and then calling the repaintChart() function, which performs the chart re-rendering.

***bar-chart.js*** >>

    angular.module('myApp').directive('barChart', function($window) {
      return {
        restrict: 'E',
        replace: true,
        scope: {
          data: '='
        },
        template: '<div id="bar-chart"></div>',
        link: function(scope, element, attrs, fn) {
    
          var data = scope.data;
          var d3 = $window.d3;
          var rawSvg = element;
    
          var colors = d3.scale.category10();
    
          var canvas = d3.select(rawSvg[0])
            .append('svg')
            .attr("width", 300)
            .attr("height", 150);
    
          // watching for any changes in the data
          // if new data is detected, the chart repaint code is run
          scope.$watch('data', function(newVal, oldVal) {
            data = newVal;
            repaintChart();
          }, true);
    
          var xscale = d3.scale.linear()
            .domain([0, 100])
            .range([0, 240]);
    
          var yscale = d3.scale.linear()
            .domain([0, data.length])
            .range([0, 120]);
    
          var bar = canvas.append('g')
            .attr("id", "bar-group")
            .attr("transform", "translate(10,20)")
            .selectAll('rect')
            .data(data)
            .enter()
            .append('rect')
            .attr("class", "bar")
            .attr("height", 15)
            .attr("x", 0)
            .attr("y", function(d, i) {
              return yscale(i);
            })
            .style("fill", function(d, i) {
              return colors(i);
            })
            .attr("width", function(d) {
              return xscale(d);
            });
    
          // changing the bar widths according to the changes in data
          function repaintChart() {
            canvas.selectAll('rect')
              .data(data)
              .transition()
              .duration(800)
              .attr("width", function(d) {
                return xscale(d);
              })
          }
        }
      }
    });

[Here is the working JSFiddle.][1]


  [1]: https://jsfiddle.net/rishabh1990/1om83rzt/

## D3.js chart with Angular v1
HTML:

    <div ng-app="myApp" ng-controller="Controller">
        <some-chart data="data"></some-chart>
    </div>

Javascript:

    angular.module('myApp', [])
    .directive('someChart', function() {
        return {
            restrict: 'E',
            scope: {data: '=data'},
            link: function (scope, element, attrs) {
            var chartElement = d3.select(element[0]);
                // here you have scope.data and chartElement
                // so you may do what you want
            } 
        };
    });

    function Controller($scope) {
        $scope.data = [1,2,3,4,5]; // useful data
    }

## D3.js component with ReactJS
This example is based on a [blog post][1] by [Nicolas Hery][2]. It utilizes ES6 classes and ReactJS's lifecycle methods to keep the D3 component updated

----------

## d3_react.html ##
    <!DOCTYPE html>
    <html>
    
    <head>
      <meta charset="utf-8">
      <title>Hello, d3React!</title>
      <style>
        .d3Component {
          width: 720px;
          height: 120px;
        }
      </style>
    </head>
    <script src="https://fb.me/react-15.2.1.min.js"></script>
    <script src="https://fb.me/react-dom-15.2.1.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/babel-core/5.8.34/browser.min.js"></script>
    <script src="https://d3js.org/d3.v4.min.js"></script>
    
    <body>
      <div id="app" />
      <script type="text/babel" src="d3_react.js"></script>
    </body>
    
    </html>
    
## d3_react.js ##
    class App extends React.Component {
      constructor(props) {
        super(props);
        this.state = {
          d3React: new d3React()
        };
        this.getd3ReactState = this.getd3ReactState.bind(this);
      }
    
      getd3ReactState() {
        // Using props and state, calculate the d3React state
        return ({
          data: {
            x: 0,
            y: 0,
            width: 42,
            height: 17,
            fill: 'red'
          }
        });
      }
    
      componentDidMount() {
        var props = {
          width: this._d3Div.clientWidth,
          height: this._d3Div.clientHeight
        };
        var state = this.getd3ReactState();
        this.state.d3React.create(this._d3Div, props, state);
      }
    
      componentDidUpdate(prevProps, prevState) {
        var state = this.getd3ReactState();
        this.state.d3React.update(this._d3Div, state);
      }
    
      componentWillUnmount() {
        this.state.d3React.destroy(this._d3Div);
      }
    
      render() {
        return (
          <div>
            <h1>{this.props.message}</h1>
            <div className="d3Component" ref={(component) => { this._d3Div = component; } } />
          </div>
        );
      }
    }
    
    class d3React {
      constructor() {
        this.create = this.create.bind(this);
        this.update = this.update.bind(this);
        this.destroy  = this.destroy.bind(this);
        this._drawComponent = this._drawComponent.bind(this);
      }
    
      create(element, props, state) {
        console.log('d3React create');
        var svg = d3.select(element).append('svg')
          .attr('width', props.width)
          .attr('height', props.height);
    
        this.update(element, state);
      }
    
      update(element, state) {
        console.log('d3React update');
        this._drawComponent(element, state.data);
      }
    
      destroy(element) {
        console.log('d3React destroy');
      }
    
      _drawComponent(element, data) {
        // perform all drawing on the element here
        var svg = d3.select(element).select('svg');
    
        svg.append('rect')
          .attr('x', data.x)
          .attr('y', data.y)
          .attr('width', data.width)
          .attr('height', data.height)
          .attr('fill', data.fill);
      }
    }

    ReactDOM.render(<App message="Hello, D3.js and React!"/>, document.getElementById('app'));

----------
Place the contents of `d3_react.html` and `d3_react.js` in the same directory and navigate a web browser to the d3React.html file. If all goes well, you will see a header stating `Hello, D3.js and React!` rendered from the React component and a red rectangle below from the custom D3 component.

React uses [refs][3] to "reach out" to the component instance. The lifecycle methods of the `d3React` class require this ref to append, modify, and remove DOM elements. The `d3React` class can be extended to create more custom components and inserted anywhere a `div.d3Component` is created by React.

  [1]: http://nicolashery.com/integrating-d3js-visualizations-in-a-react-app/
  [2]: http://nicolashery.com
  [3]: https://facebook.github.io/react/docs/more-about-refs.html

