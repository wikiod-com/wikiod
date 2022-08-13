---
title: "UISearchController"
slug: "uisearchcontroller"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## Syntax
 - UISearchController(searchResultsController: UIViewController?) // Pass nil as the parameter if the search updating controller also displays the searchable content.
 - func updateSearchResults(for searchController: UISearchController) // Required method to implement when adopting the UISearchResultsUpdating protocol

## Parameters
Parameter | Details
-------  |  ---------
`UISearchController.searchBar` | The search bar to install in your interface. *(read-only)*
`UISearchController.searchResultsUpdater` | The object responsible for updating the contents of the search results controller.
`UISearchController.isActive` | The presented state of the search interface.
`UISearchController.obscuresBackgroundDuringPresentation` | A Boolean indicating whether the underlying content is obscured during a search.
`UISearchController.dimsBackgroundDuringPresentation` | A Boolean indicating whether the underlying content is dimmed during a search.
`UISearchController.hidesNavigationBarDuringPresentation` | A Boolean indicating whether the navigation bar should be hidden when searching.
`UIViewController.definesPresentationContext` | A Boolean value that indicates whether this view controller's view is covered when the view controller or one of its descendants presents a view controller.
`UIViewController.navigationItem.titleView` | A custom view displayed in the center of the navigation bar when the receiver is the top item in which a search bar can be placed.
`UITableViewController.tableView.tableHeaderView` | Returns an accessory view that is displayed above the table in which a search bar can be placed.

UIKit Framework Reference:

[UISearchController][1]

[UISearchResultsUpdating][2]


  [1]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UISearchController/
  [2]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UISearchResultsUpdating_ClassRef/index.html

## Search Bar in Navigation Bar Title
This example uses a search controller to filter the data inside a table view controller. The search bar is placed inside the navigation bar that the table view is embedded into.

[![search-bar-in-nav-bar][1]][1]

Embed a `UITableViewController` into a `UINavigationController` to get the `UINavigationItem` (which contains the navigation bar).
Then set our custom ViewController class to inherit from `UITableViewController` and adopt the `UISearchResultsUpdating` protocol.

    class ViewController: UITableViewController, UISearchResultsUpdating {

        let entries = [(title: "Easiest", image: "green_circle"),
                       (title: "Intermediate", image: "blue_square"),
                       (title: "Advanced", image: "black_diamond"),
                       (title: "Expert Only", image: "double_black_diamond")]
        
        // An empty tuple that will be updated with search results.
        var searchResults : [(title: String, image: String)] = []
        
        let searchController = UISearchController(searchResultsController: nil)
    
        override func viewDidLoad() {
            super.viewDidLoad()
            
            searchController.searchResultsUpdater = self
            self.definesPresentationContext = true

            // Place the search bar in the navigation item's title view.
            self.navigationItem.titleView = searchController.searchBar

            // Don't hide the navigation bar because the search bar is in it.
            searchController.hidesNavigationBarDuringPresentation = false
        }
        
        func filterContent(for searchText: String) {
            // Update the searchResults array with matches
            // in our entries based on the title value.
            searchResults = entries.filter({ (title: String, image: String) -> Bool in
                let match = title.range(of: searchText, options: .caseInsensitive)
                // Return the tuple if the range contains a match.
                return match != nil
            })
        }

        // MARK: - UISearchResultsUpdating method
        
        func updateSearchResults(for searchController: UISearchController) {
            // If the search bar contains text, filter our data with the string
            if let searchText = searchController.searchBar.text {
                filterContent(for: searchText)
                // Reload the table view with the search result data.
                tableView.reloadData()
            }
        }
    
        // MARK: - UITableViewController methods
        
        override func numberOfSections(in tableView: UITableView) -> Int { return 1 }
    
        override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
            // If the search bar is active, use the searchResults data.
            return searchController.isActive ? searchResults.count : entries.count
        }
        
        override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
            // If the search bar is active, use the searchResults data.
            let entry = searchController.isActive ? 
                        searchResults[indexPath.row] : entries[indexPath.row]
            
            let cell = tableView.dequeueReusableCell(withIdentifier: "Cell", for: indexPath)
            cell.textLabel?.text = entry.title
            cell.imageView?.image = UIImage(named: entry.image)
            return cell
        }
    }

  [1]: http://i.stack.imgur.com/y85SV.png

## Search Bar in Table View Header
This example uses a search controller to filter the cells in a table view controller. The search bar is placed inside the header view of the table view. The table view content is offset with the same height as the search bar so that the search bar is hidden at first. Upon scrolling up past the top edge of the table view, the search bar is revealed. Then when the search bar becomes active, it hides the navigation bar.

[![search-bar-in-table-header-gif][1]][1]
[![search-bar-in-table-header][2]][2]

Embed a UITableViewController into a UINavigationController to get the UINavigationItem (which contains the navigation bar). Then set our custom ViewController class to inherit from UITableViewController and adopt the UISearchResultsUpdating protocol.

    class ViewController: UITableViewController, UISearchResultsUpdating {
    
        let entries = [(title: "Easiest", image: "green_circle"),
                       (title: "Intermediate", image: "blue_square"),
                       (title: "Advanced", image: "black_diamond"),
                       (title: "Expert Only", image: "double_black_diamond")]
        
        // An empty tuple that will be updated with search results.
        var searchResults : [(title: String, image: String)] = []
        
        let searchController = UISearchController(searchResultsController: nil)
    
        override func viewDidLoad() {
            super.viewDidLoad()
            
            searchController.searchResultsUpdater = self
            self.definesPresentationContext = true
    
            // Place the search bar in the table view's header.
            self.tableView.tableHeaderView = searchController.searchBar
    
            // Set the content offset to the height of the search bar's height
            // to hide it when the view is first presented.
            self.tableView.contentOffset = CGPoint(x: 0, y: searchController.searchBar.frame.height)
        }
        
        func filterContent(for searchText: String) {
            // Update the searchResults array with matches
            // in our entries based on the title value.
            searchResults = entries.filter({ (title: String, image: String) -> Bool in
                let match = title.range(of: searchText, options: .caseInsensitive)
                // Return the tuple if the range contains a match.
                return match != nil
            })
        }
    
        // MARK: - UISearchResultsUpdating method
        
        func updateSearchResults(for searchController: UISearchController) {
            // If the search bar contains text, filter our data with the string
            if let searchText = searchController.searchBar.text {
                filterContent(for: searchText)
                // Reload the table view with the search result data.
                tableView.reloadData()
            }
        }
    
        // MARK: - UITableViewController methods
        
        override func numberOfSections(in tableView: UITableView) -> Int { return 1 }
    
        override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
            // If the search bar is active, use the searchResults data.
            return searchController.isActive ? searchResults.count : entries.count
        }
        
        override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
            // If the search bar is active, use the searchResults data.
            let entry = searchController.isActive ? 
                        searchResults[indexPath.row] : entries[indexPath.row]
            
            let cell = tableView.dequeueReusableCell(withIdentifier: "Cell", for: indexPath)
            cell.textLabel?.text = entry.title
            cell.imageView?.image = UIImage(named: entry.image)
            return cell
        }
    }


  [1]: http://i.stack.imgur.com/rfQUp.gif
  [2]: http://i.stack.imgur.com/34yk2.png

## Implementation


## UISerachController in Objective-C
    Delegate: UISearchBarDelegate, UISearchControllerDelegate, UISearchBarDelegate

    @property (strong, nonatomic)  UISearchController *searchController;

    - (void)searchBarConfiguration
    {
        self.searchController = [[UISearchController alloc] initWithSearchResultsController:nil];
        self.searchController.searchBar.delegate = self;
        self.searchController.hidesNavigationBarDuringPresentation = NO;
        
        // Hides search bar initially.  When the user pulls down on the list, the search bar is revealed.
        [self.tableView setContentOffset:CGPointMake(0, self.searchController.searchBar.frame.size.height)];
        
        self.searchController.searchBar.backgroundColor = [UIColor DarkBlue];
        self.searchController.searchBar.tintColor = [UIColor DarkBlue];
        
        self.tableView.contentOffset = CGPointMake(0, CGRectGetHeight(_searchController.searchBar.frame));
        self.tableView.tableHeaderView = _searchController.searchBar;
        _searchController.searchBar.delegate = self;
        _searchController.searchBar.showsCancelButton = YES;
        self.tapGestureRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(resetSearchbarAndTableView)];
        [self.view addGestureRecognizer:self.tapGestureRecognizer];
        
    }

    - (void)resetSearchbarAndTableView{
    // Reload your tableview and resign keyboard.
    }


    - (void)searchBarCancelButtonClicked:(UISearchBar *)searchBar{
    // Search cancelled
    }
    - (void)searchBarSearchButtonClicked:(UISearchBar *)searchBar{
    // Implement filtration of your data as per your need using NSPredicate or else.
    // then reload your data control like Tableview.
    }



