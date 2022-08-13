---
title: "NSFetchedResultsController"
slug: "nsfetchedresultscontroller"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

NSFetchedResultsController is a connection between core-data table (entity in core-data, table in sqlite) and UITableView.
UITableView can be attached to any core-data entity using NSFetchedResultsController and UITableView will be updated as and when core-data updates that entity/table.

## NSFetchedResultsController for UITableView
    class ConversationsTableViewController: UITableViewController, NSFetchedResultsControllerDelegate {

    private var fetchedResultsController: NSFetchedResultsController<Conversation>!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        initializeFetchedResultsController()
    }
    private func initializeFetchedResultsController() {
        let request = NSFetchRequest<Conversation>(entityName: "Conversation")
        let timeSort = NSSortDescriptor(key: "lastMessageTime", ascending: false)
        request.sortDescriptors = [timeSort]
        
        let MOC = AppManagedObjectContext()//It should be main thread MOC
        fetchedResultsController = NSFetchedResultsController(fetchRequest: request, managedObjectContext: MOC, sectionNameKeyPath: nil, cacheName: nil)
        fetchedResultsController.delegate = self
        
        do {
            try fetchedResultsController.performFetch()
        } catch {
            print("Failed to initialize FetchedResultsController: \(error)")
        }
    }
    
    //table view methods
    override func numberOfSections(in tableView: UITableView) -> Int {
        if let n = fetchedResultsController?.sections!.count {
            return n
        }
        return 0
    }
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        let sectionInfo = fetchedResultsController.sections![section]
        let n =  sectionInfo.numberOfObjects
        return n
    }
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "identifier", for: indexPath)
        //configure cell
        return cell
    }

    //NSFetchedResultsController Delegates
    func controllerWillChangeContent(_ controller: NSFetchedResultsController<NSFetchRequestResult>) {
        tableView.beginUpdates()
    }
    
    func controller(_ controller: NSFetchedResultsController<NSFetchRequestResult>, didChange sectionInfo: NSFetchedResultsSectionInfo, atSectionIndex sectionIndex: Int, for type: NSFetchedResultsChangeType) {
        switch type {
        case .insert:
            tableView.insertSections(IndexSet(integer: sectionIndex), with: .fade)
        case .delete:
            tableView.deleteSections(IndexSet(integer: sectionIndex), with: .fade)
        case .move:
            break
        case .update:
            break
        }
    }
    
    func controller(_ controller: NSFetchedResultsController<NSFetchRequestResult>, didChange anObject: Any, at indexPath: IndexPath?, for type: NSFetchedResultsChangeType, newIndexPath: IndexPath?) {
        switch type {
        case .insert:
            tableView.insertRows(at: [newIndexPath!], with: .fade)
        case .delete:
            tableView.deleteRows(at: [indexPath!], with: .fade)
        case .update:
            tableView.reloadRows(at: [indexPath!], with: .none)
        case .move:
            tableView.deleteRows(at: [indexPath!], with: .fade)
            tableView.insertRows(at: [newIndexPath!], with: .fade)
        }
    }
    
    func controllerDidChangeContent(_ controller: NSFetchedResultsController<NSFetchRequestResult>) {
        tableView.endUpdates()
    }
    }

