---
title: "UICollectionView"
slug: "uicollectionview"
draft: false
images: []
weight: 9704
type: docs
toc: true
---

## Create a UICollectionView

Initialize a `UICollectionView` with a `CGRect` frame:

**Swift:**

    let collection = UICollectionView(frame: CGRect(x: 0, y: 0, width: 200, height: 21))
**Objective C:**

    UICollectionView *collection = [[UICollectionView alloc] initWithFrame:CGRectMake(0, 0, 200, 21)];
You can also create a `UICollectionView` in Interface Builder

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/HxNsRm.png

## UICollectionView - Datasource
Every collection view must have a `Datasource` object. The `Datasource` object is the content that your app will display within the `UICollectionView`. At a minimum, all `Datasource` objects must implement the `collectionView:numberOfItemsInSection:` and `collectionView:cellForItemAtIndexPath:` methods.

*Required Methods* 

**Swift**

    func collectionView(collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        // Return how many items in section
        let sectionArray = _data[section]
        return sectionArray.count
    }
        
    func collectionView(collectionView: UICollectionView, cellForItemAtIndexPath indexPath: NSIndexPath) -> UICollectionViewCell {

       let cell = collectionView.dequeueReusableCellWithReuseIdentifier(MyCellID) 
       // If you use a custom cell class then cast the cell returned, like:
       // as! MyCollectionViewCellClass
       // or you will have errors when you try to use features of that class.

       //Customize your cell here, default UICollectionViewCells do not contain any inherent
       //text or image views (like UITableView), but some could be added, 
       //or a custom UICollectionViewCell sub-class could be used
       return cell
    }

**Objective C**

    - (NSInteger)collectionView:(UICollectionView*)collectionView numberOfItemsInSection:(NSInteger)section {
        // Return how many items in section
        NSArray *sectionArray = [_data objectAtIndex:section];
        return [sectionArray count];
    }
    
    - (UICollectionViewCell *)collectionView:(UICollectionView *)collectionView
                      cellForItemAtIndexPath:(NSIndexPath *)indexPath {
       // Return a cell
       UICollectionViewCell *newCell = [self.collectionView 
                                           dequeueReusableCellWithReuseIdentifier:MyCellID                                                                             
                                                                     forIndexPath:indexPath];
       //Customize your cell here, default UICollectionViewCells do not contain any inherent
       //text or image views (like UITableView), but some could be added, 
       //or a custom UICollectionViewCell sub-class could be used
       return newCell;
    }

## Basic Swift example of a Collection View
# Create a new project

It can be just a Single View Application.

# Add the code

Create a new Cocoa Touch Class file (File > New > File... > iOS > Cocoa Touch Class). Name it `MyCollectionViewCell`. This class will hold the outlets for the views that you add to your cell in the storyboard.

    import UIKit
    class MyCollectionViewCell: UICollectionViewCell {
        
        @IBOutlet weak var myLabel: UILabel!
    }

We will connect this outlet later.

Open ViewController.swift and make sure you have the following content:

    import UIKit
    class ViewController: UIViewController, UICollectionViewDataSource, UICollectionViewDelegate {
        
        let reuseIdentifier = "cell" // also enter this string as the cell identifier in the storyboard
        var items = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48"]
        
        
        // MARK: - UICollectionViewDataSource protocol
        
        // tell the collection view how many cells to make
        func collectionView(collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
            return self.items.count
        }
        
        // make a cell for each cell index path
        func collectionView(collectionView: UICollectionView, cellForItemAtIndexPath indexPath: NSIndexPath) -> UICollectionViewCell {
            
            // get a reference to our storyboard cell
            let cell = collectionView.dequeueReusableCellWithReuseIdentifier(reuseIdentifier, forIndexPath: indexPath) as! MyCollectionViewCell
            
            // Use the outlet in our custom class to get a reference to the UILabel in the cell
            cell.myLabel.text = self.items[indexPath.item]
            cell.backgroundColor = UIColor.yellowColor() // make cell more visible in our example project
            
            return cell
        }
        
        // MARK: - UICollectionViewDelegate protocol
        
        func collectionView(collectionView: UICollectionView, didSelectItemAtIndexPath indexPath: NSIndexPath) {
            // handle tap events
            print("You selected cell #\(indexPath.item)!")
        }
    }

**Notes**

- `UICollectionViewDataSource` and `UICollectionViewDelegate` are the protocols that the collection view follows. You could also add the `UICollectionViewDelegateFlowLayout` protocol to change the size of the views programmatically, but it isn't necessary.
- We are just putting simple strings in our grid, but you could certainly do images later. 

# Setup the storyboard

Drag a Collection View to the View Controller in your storyboard. You can add constraints to make it fill the parent view if you like.

[![adding collection view to storyboard screenshot][1]][1]


Make sure that your defaults in the Attribute Inspector are also

- Items: 1
- Layout: Flow

The little box in the top left of the Collection View is a Collection View Cell. We will use it as our prototype cell. Drag a Label into the cell and center it. You can resize the cell borders and add constraints to center the Label if you like.

[![collection view label screenshot][2]][2]

Write "cell" (without quotes) in the Identifier box of the Attributes Inspector for the Collection View Cell. Note that this is the same value as `let reuseIdentifier = "cell"` in ViewController.swift.

[![setting cell reuse identifier screenshot][3]][3]

And in the Identity Inspector for the cell, set the class name to `MyCollectionViewCell`, our custom class that we made.

[![setting custom cell class name screenshot][4]][4]

# Hook up the outlets

- Hook the Label in the collection cell to `myLabel` in the `MyCollectionViewCell` class. (You can [Control-drag][5].)
- Hook the Collection View `delegate` and `dataSource` to the View Controller. (Right click Collection View in the Document Outline. Then click and drag the plus arrow up to the View Controller.)

[![setting delegate and datasource outlets screenshot][6]][6]


# Finished

Here is what it looks like after adding constraints to center the Label in the cell and pinning the Collection View to the walls of the parent.


[![final result screenshot][7]][7]

# Making Improvements

If you want to make improvements on the appearance, [see the original post that this example comes from][8].


[![collection view with improved appearance screenshot][10]][10]

# Further study

- [A Simple UICollectionView Tutorial][11]
- [UICollectionView Tutorial Part 1: Getting Started][12]
- [UICollectionView Tutorial Part 2: Reusable Views and Cell Selection][13]
 

  [1]: http://i.stack.imgur.com/60tSB.png
  [2]: http://i.stack.imgur.com/veuvJ.png
  [3]: http://i.stack.imgur.com/wBzIp.png
  [4]: http://i.stack.imgur.com/lP6gC.png
  [5]: https://developer.apple.com/library/ios/recipes/xcode_help-IB_connections/chapters/CreatingOutlet.html
  [6]: http://i.stack.imgur.com/go3aZ.png
  [7]: http://i.stack.imgur.com/hIxR3.png
  [8]: http://stackoverflow.com/a/31735229/3681880
  [9]: http://stackoverflow.com/a/34984063/3681880
  [10]: http://i.stack.imgur.com/j7CiY.png
  [11]: http://adoptioncurve.net/archives/2012/09/a-simple-uicollectionview-tutorial/
  [12]: http://www.raywenderlich.com/78550/beginning-ios-collection-views-swift-part-1
  [13]: http://www.raywenderlich.com/78551/beginning-ios-collection-views-swift-part-2

## Create a Collection View Programmatically
**Swift**

    func createCollectionView() {
        let layout: UICollectionViewFlowLayout = UICollectionViewFlowLayout()
        let collectionView = UICollectionView(frame: CGRect(x: 0, y: 0, width: view.frame.width, height: view.frame.height), collectionViewLayout: layout)
        collectionView.dataSource = self
        collectionView.delegate = self
        view.addSubview(collectionView)
    }

**Objective-C**

    - (void)createCollectionView {
        UICollectionViewFlowLayout *layout = [[UICollectionViewFlowLayout alloc] init];
        UICollectionView *collectionView = [[UICollectionView alloc] initWithFrame:CGRectMake(0, 0, self.view.frame.size.width, self.view.frame.size.height) collectionViewLayout:layout];
        [collectionView setDataSource:self];
        [collectionView setDelegate:self];
        [self.view addSubview:collectionView];
    }

## Swift - UICollectionViewDelegateFlowLayout


## Performing batch updates


## UICollectionViewDelegate setup and item selection
Sometimes, if an action should be bind to a collection view's cell selection, you have to implement the `UICollectionViewDelegate` protocol.

Let's say the collection view is inside a `UIViewController MyViewController`.

**Objective-C**

In your *MyViewController.h* declares that it implements the `UICollectionViewDelegate` protocol, as below

    @interface MyViewController : UIViewController <UICollectionViewDelegate, .../* previous existing delegate, as UICollectionDataSource *>

**Swift**

In your *MyViewController.swift* add the following

    class MyViewController : UICollectionViewDelegate {
    }

<br/>
The method that must be implemented is 
<br/>
<br/>


**Objective-C**

    -(void)collectionView:(UICollectionView *)collectionView didSelectItemAtIndexPath:(NSIndexPath *)indexPath
    {
    }

**Swift**

    func collectionView(collectionView: UICollectionView, didSelectItemAtIndexPath indexPath: NSIndexPath)
    {
    }
       

<br/>
As just an example we can set the background color of selected cell to green.
<br/>
<br/>

**Objective-C**

    -(void)collectionView:(UICollectionView *)collectionView didSelectItemAtIndexPath:(NSIndexPath *)indexPath
    {
        UICollectionViewCell* cell = [collectionView cellForItemAtIndexPath:indexPath];
        cell.backgroundColor = [UIColor greenColor];
    }

**Swift**

    class MyViewController : UICollectionViewDelegate {
        func collectionView(collectionView: UICollectionView, didSelectItemAtIndexPath indexPath: NSIndexPath)
        {
            var cell : UICollectionViewCell = collectionView.cellForItemAtIndexPath(indexPath)!
            cell.backgroundColor = UIColor.greenColor()
        }
    }


## Manage Multiple Collection view with DataSource and Flowlayout
   Here we are managing multiple collection there delegate methods with didselect events.
    
    extension ProductsVC: UICollectionViewDelegate, UICollectionViewDataSource{
            
            // MARK: - UICollectionViewDataSource
            func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
                guard collectionView == collectionCategory else {
                    return arrOfProducts.count
                }
                return arrOfCategory.count
            }
            
            func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
                
                guard collectionView == collectionProduct else {
                      let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "ProductCategoryCell", for: indexPath) as! ProductCategoryCell
                      cell.viewBackground.layer.borderWidth = 0.5
                      //Do some thing as per use
                      return cell
                }
                
                let cell = collectionView.dequeueReusableCell(withReuseIdentifier: cellIdentifier, for: indexPath) as! ProductCell
                cell.contentView.layer.borderWidth = 0.5
                cell.contentView.layer.borderColor = UIColor.black.cgColor
                let json = arrOfProducts[indexPath.row]
                //Do something as per use
        
                return cell
            }
            
            func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
                guard collectionView == collectionCategory else {
                    let json = arrOfProducts[indexPath.row]
                    // Do something for  collectionProduct here
                    return
                }
                let json = arrOfCategory[indexPath.row] as [String: AnyObject]
                let id = json["cId"] as? String ?? ""
                // Do something
            }
        }
        
        extension ProductsVC: UICollectionViewDelegateFlowLayout{
            
            // MARK: - UICollectionViewDelegateFlowLayout
            func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAt indexPath: IndexPath) -> CGSize {
                
                let collectionWidth = collectionView.bounds.width
                guard collectionView == collectionProduct else {
                    var itemWidth = collectionWidth / 4 - 1;
                    
                    if(UI_USER_INTERFACE_IDIOM() == .pad) {
                        itemWidth = collectionWidth / 4 - 1;
                    }
                    return CGSize(width: itemWidth, height: 50)
                }
                
                var itemWidth = collectionWidth / 2 - 1;
                if(UI_USER_INTERFACE_IDIOM() == .pad) {
                    itemWidth = collectionWidth / 4 - 1;
                }
                return CGSize(width: itemWidth, height: 250);
            }
            
            func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, minimumInteritemSpacingForSectionAt section: Int) -> CGFloat {
                return 1
            }
            
            func collectionView(_ collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, minimumLineSpacingForSectionAt section: Int) -> CGFloat {
                return 1
            }
            
        }
[![collection views][1]][1]


  [1]: https://i.stack.imgur.com/qcnAj.png

