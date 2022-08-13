---
title: "Custom methods of selection of UITableViewCells"
slug: "custom-methods-of-selection-of-uitableviewcells"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Distinction between single and double selection on row.
An example of implementation UITableView which allows to detect if cell has been tapped single or double time.  

    override func viewDidLoad() {
        viewDidLoad()
        
        let doubleTapGestureRecognizer = UITapGestureRecognizer(target: self, action: #selector(handleDoubleTap(sender:)))
        doubleTapGestureRecognizer.numberOfTapsRequired = 2
        tableView.addGestureRecognizer(doubleTapGestureRecognizer)
        
        let tapGestureRecognizer = UITapGestureRecognizer(target: self, action: #selector(handleTapGesture(sender:)))
        tapGestureRecognizer.numberOfTapsRequired = 1
        tapGestureRecognizer.require(toFail: doubleTapGestureRecognizer)
        tableView.addGestureRecognizer(tapGestureRecognizer)
    }

    func handleTapGesture(sender: UITapGestureRecognizer) {
        let touchPoint = sender.location(in: tableView)
        if let indexPath = tableView.indexPathForRow(at: touchPoint) {
            print(indexPath)
        }
    }

    func handleDoubleTap(sender: UITapGestureRecognizer) {
        let touchPoint = sender.location(in: tableView)
        if let indexPath = tableView.indexPathForRow(at: touchPoint) {
            print(indexPath)
        }
    }

