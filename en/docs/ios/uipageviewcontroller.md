---
title: "UIPageViewController"
slug: "uipageviewcontroller"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

UIPageViewController provides users the ability to easily transition between several views by using a swipe gesture. In order to create a UIPageViewController, you must implement the UIPageViewControllerDataSource methods. These include methods to return both the UIPageViewController before and after the current UIPageViewController along with the presentationCount and presentationIndex methods. 

## Syntax


 1. UIPageViewControllerTransitionStyle
 2. UIPageViewControllerNavigationOrientation
 3. UIPageViewControllerSpineLocation
 4. UIPageViewControllerNavigationDirection



Apple Developer reference [here](https://developer.apple.com/reference/uikit/uipageviewcontroller)

## Create a horizontal paging UIPageViewController programatically
   
1. Init array of view controllers which will be managed by UIPageViewController. Add a base view controller class which has property `identifier` which will be used to identify view controllers when working with UIPageViewController data source methods. Let the view controllers to inherit from that base class.
 

    UIViewController *firstVC = [[UIViewController alloc] init]; 
    firstVC.identifier = 0  
    UIViewController *secondVC = [[UIViewController alloc] init];   
    secondVC.identifier = 1
    NSArray *viewControllers = [[NSArray alloc] initWithObjects: firstVC, secondVC, nil];
    
2. Create UIPageViewController instance.


    UIPageViewController *pageViewController = [[UIPageViewController alloc] initWithTransitionStyle:UIPageViewControllerTransitionStyleScroll
                                                                               navigationOrientation:UIPageViewControllerNavigationOrientationHorizontal
                                                                                             options:nil];


3. Data source is current class which must implement `UIPageViewControllerDataSource` protocol.


    pageViewController.dataSource = self;

            

4. `setViewControllers` will add only first view controller, next will be added to the stack using data source methods


    if (viewControllers.count) {
        [pageViewController setViewControllers:@[[viewControllers objectAtIndex:0]]
                                     direction:UIPageViewControllerNavigationDirectionForward
                                      animated:NO
                                    completion:nil];
    }
            

5. Add UIPageViewController as a child view controller so it will receive from it's parent view controller `appearance` and `rotation` events.

      
     [self addChildViewController:pageViewController];
     pageViewController.view.frame = self.view.frame;
     [self.view addSubview:pageViewController.view];
     [pageViewController didMoveToParentViewController:self];

6. Implementing UIPageViewControllerDataSource methods


    - (UIViewController *)pageViewController:(UIPageViewController *)pageViewController
          viewControllerBeforeViewController:(UIViewController *)viewController
    {
        index = [(Your View Controler Base Class *)viewController identifier];
        index--;
        return [self childViewControllerAtIndex:index];
    }

    - (UIViewController *)pageViewController:(UIPageViewController *)pageViewController
           viewControllerAfterViewController:(UIViewController *)viewController
    {
        index = [(Your View Controler Base Class *)viewController identifier];
        index++;
        return [self childViewControllerAtIndex:index];
    }
    
    - (NSInteger)presentationCountForPageViewController:(UIPageViewController *)pageViewController
    {
        return [viewControllers count];
    }
    
    - (NSInteger)presentationIndexForPageViewController:(UIPageViewController *)pageViewController
    {
        return index;
    }

7. Utility method which returns a view controller using an index, if index is out of bounds it returns nil.


    - (UIViewController *)childViewControllerAtIndex:(NSInteger)index
    {
        if (index <= ([viewControllers count] - 1)) {
            return [viewControllers objectAtIndex:index];
        } else {
            return nil;
        }
    }



## A simple way to create horizontal page view controllers ( infinite pages )
1. Let's create a new project, I'm choosing Single View Application for better demonstration 

[![enter image description here][1]][1]

2. Drag a page view controller to the storyboard, there are 2 things you should change after that: 
    1. Set the page view controller as initial view controller
    2. Change the transition style to scroll

[![enter image description here][2]][2]

3. And you need to create a UIPageViewController class, then set it as custom class of the page view controller on the storyboard
4. Paste this code into your UIPageViewController class, you should get a colorful infinite paged app :)

       class PageViewController: UIPageViewController, UIPageViewControllerDataSource {
    
           override func viewDidLoad() {
               self.dataSource = self
               let controller = createViewController()
               self.setViewControllers([controller], direction: .forward, animated: false, completion: nil)
           }
    
           func pageViewController(_ pageViewController: UIPageViewController, viewControllerBefore viewController: UIViewController) -> UIViewController? {
               let controller = createViewController()
               return controller
           }
    
           func pageViewController(_ pageViewController: UIPageViewController, viewControllerAfter viewController: UIViewController) -> UIViewController? {
               let controller = createViewController()
               return controller
           }
    
           func createViewController() -> UIViewController {
               var randomColor: UIColor {
                   return UIColor(hue: CGFloat(arc4random_uniform(360))/360, saturation: 0.5, brightness: 0.8, alpha: 1)
               }
               let storyboard = UIStoryboard(name: "Main", bundle: nil)
               let controller = storyboard.instantiateViewController(withIdentifier: "View Controller")
               controller.view.backgroundColor = randomColor
               return controller
           }
       }

This is what the final project looks like, you get a view controller with different color with every scroll: 

  [![enter image description here][3]][3]


  [1]: https://i.stack.imgur.com/sJg7e.png
  [2]: https://i.stack.imgur.com/najU6.png
  [3]: https://i.stack.imgur.com/aFEUm.png

