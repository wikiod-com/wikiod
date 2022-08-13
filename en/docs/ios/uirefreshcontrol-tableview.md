---
title: "UIRefreshControl TableView"
slug: "uirefreshcontrol-tableview"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

A UIRefreshControl object provides a standard control that can be used to initiate the refreshing of a table view’s contents. You link a refresh control to a table through an associated table view controller object. The table view controller handles the work of adding the control to the table’s visual appearance and managing the display of that control in response to appropriate user gestures.

## Objective-C Example
First declare a property like this in the ViewController

    @property (nonatomic) UIRefreshControl *refreshControl;

Later in the `viewDidLoad()` set up the refreshControl as given below:

    self.refreshControl = [[UIRefreshControl alloc]init];
    [self.tableView addSubview:self.refreshControl];
    [self.refreshControl addTarget:self action:@selector(refreshTable) forControlEvents:UIControlEventValueChanged];
    //Setting the tint Color of the Activity Animation
    self.refreshControl.tintColor = [UIColor redColor];
    //Setting the attributed String to the text
    NSMutableAttributedString * string = [[NSMutableAttributedString alloc] initWithString:@"firstsecondthird"];
    [string addAttribute:NSForegroundColorAttributeName value:[UIColor redColor] range:NSMakeRange(0,5)];
    [string addAttribute:NSForegroundColorAttributeName value:[UIColor greenColor] range:NSMakeRange(5,6)];
    [string addAttribute:NSForegroundColorAttributeName value:[UIColor blueColor] range:NSMakeRange(11,5)];
    self.refreshControl.attributedTitle = string;

Now The function `refreshTable` is defined as:

    - (void)refreshTable {
        //TODO: refresh your data
        [self.refreshControl endRefreshing];
        [self.refreshControl beginRefreshing];
        [self.tableView reloadData];
        [self.refreshControl endRefreshing];
    }


[![The resulting refresh View is:][1]][1]


  [1]: https://i.stack.imgur.com/JWQt1.png

## Set up refreshControl on tableView:
    UIRefreshControl *refreshControl = [[UIRefreshControl alloc] init];
    [refreshControl addTarget:self action:@selector(pullToRefresh:) forControlEvents:UIControlEventValueChanged];
    self.scrollView.alwaysBounceVertical = YES;
    [self.scrollView addSubview:refreshControl];

    - (void)pullToRefresh:(UIRefreshControl*) sender{
    //Do work off the main thread
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        // Simulate network traffic (sleep for 2 seconds)
        [NSThread sleepForTimeInterval:2];
        //Update data
        //Call complete on the main thread
        dispatch_sync(dispatch_get_main_queue(), ^{
            //Update network activity UI
            NSLog(@"COMPLETE");
            [sender endRefreshing];
        });
    });
}

