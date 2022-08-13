---
title: "UIKit elements with SpriteKit"
slug: "uikit-elements-with-spritekit"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## UITableView in SKScene
    import SpriteKit
    import UIKit
    class GameRoomTableView: UITableView,UITableViewDelegate,UITableViewDataSource {
        var items: [String] = ["Player1", "Player2", "Player3"]
        override init(frame: CGRect, style: UITableViewStyle) {
            super.init(frame: frame, style: style)
            self.delegate = self
            self.dataSource = self
        }
        required init?(coder aDecoder: NSCoder) {
            fatalError("init(coder:) has not been implemented")
        }
        // MARK: - Table view data source
        func numberOfSections(in tableView: UITableView) -> Int {
            return 1
        }
        func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
            return items.count
        }
        func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
            let cell:UITableViewCell = tableView.dequeueReusableCell(withIdentifier: "cell")! as UITableViewCell
            cell.textLabel?.text = self.items[indexPath.row]
            return cell
        }
        func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
            return "Section \(section)"
        }
        func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
            print("You selected cell #\(indexPath.row)!")
        }
    }
    class GameScene: SKScene {
        var gameTableView = GameRoomTableView()
        private var label : SKLabelNode?
        override func didMove(to view: SKView) {
            self.label = self.childNode(withName: "//helloLabel") as? SKLabelNode
            if let label = self.label {
                label.alpha = 0.0
                label.run(SKAction.fadeIn(withDuration: 2.0))
            }
            // Table setup
            gameTableView.register(UITableViewCell.self, forCellReuseIdentifier: "cell")
            gameTableView.frame=CGRect(x:20,y:50,width:280,height:200)
            view.addSubview(gameTableView)
            gameTableView.reloadData()
        }
    }

**Output**:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/b54eT.png

## Protocol/Delegate to call a game ViewController method from the game scene
*GameScene* code example:

    import SpriteKit
    protocol GameViewControllerDelegate: class {
        func callMethod(inputProperty:String)
    }
    class GameScene: SKScene {
        weak var gameViewControllerDelegate:GameViewControllerDelegate?
        override func didMove(to view: SKView) {
            gameViewControllerDelegate?.callMethod(inputProperty: "call game view controller method")
        }
    }

*GameViewController* code example:

    class GameViewController: UIViewController, GameViewControllerDelegate {
        override func viewDidLoad() {
            super.viewDidLoad()
            if let view = self.view as! SKView? {
                // Load the SKScene from 'GameScene.sks'
                if let scene = SKScene(fileNamed: "GameScene") {
                    let gameScene = scene as! GameScene
                    gameScene.gameViewControllerDelegate = self
                    gameScene.scaleMode = .aspectFill
                    view.presentScene(gameScene)
                }
                view.ignoresSiblingOrder = true
                view.showsFPS = true
                view.showsNodeCount = true
            }
        }
        func callMethod(inputProperty:String) {
            print("inputProperty is: ",inputProperty)
        }
    }

**Output**:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/c0ciU.png

## StackView in SKScene
    import SpriteKit
    import UIKit
    protocol StackViewDelegate: class {
        func didTapOnView(at index: Int)
    }
    class GameMenuView: UIStackView {
        weak var delegate: StackViewDelegate?
        override init(frame: CGRect) {
            super.init(frame: frame)
            self.axis = .vertical
            self.distribution = .fillEqually
            self.alignment = .fill
            self.spacing = 5
            self.isUserInteractionEnabled = true
            //set up a label
            for i in 1...5 {
                let label = UILabel()
                label.text = "Menu voice \(i)"
                label.textColor = UIColor.white
                label.backgroundColor = UIColor.blue
                label.textAlignment = .center
                label.tag = i
                self.addArrangedSubview(label)
            }
            configureTapGestures()
        }
        required init(coder: NSCoder) {
            fatalError("init(coder:) has not been implemented")
        }
        private func configureTapGestures() {
            arrangedSubviews.forEach { view in
                view.isUserInteractionEnabled = true
                let tapGesture = UITapGestureRecognizer(target: self, action: #selector(didTapOnView))
                view.addGestureRecognizer(tapGesture)
            }
        }
        func didTapOnView(_ gestureRecognizer: UIGestureRecognizer) {
            if let index = arrangedSubviews.index(of: gestureRecognizer.view!) {
                delegate?.didTapOnView(at: index)
            }
        }
    }
    class GameScene: SKScene, StackViewDelegate {
        var gameMenuView = GameMenuView()
        private var label : SKLabelNode?
        override func didMove(to view: SKView) {
            self.label = self.childNode(withName: "//helloLabel") as? SKLabelNode
            if let label = self.label {
                label.alpha = 0.0
                label.run(SKAction.fadeIn(withDuration: 2.0))
            }
            // Menu setup with stackView
            gameMenuView.frame=CGRect(x:20,y:50,width:280,height:200)
            view.addSubview(gameMenuView)
            gameMenuView.delegate = self
        }
        func didTapOnView(at index: Int) {
            switch index {
            case 0: print("tapped voice 1")
            case 1: print("tapped voice 2")
            case 2: print("tapped voice 3")
            case 3: print("tapped voice 4")
            case 4: print("tapped voice 5")
            default:break
            }
        }
    }


**Output**:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/pOtM3.png

## Multiple UIViewController in a game: how to jump from the scene to a viewController
**Storyboard**:
--
[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/s4vXY.png

**Initial viewController**: an empty viewController with a button to present the GameViewController

**GameViewController**: the typical GameViewController of the *"Hello World"* Sprite-kit template.

**Goal**: I want to present the first viewController from my `SKScene` game with the correct deallocation of my scene.

**Description**: To obtain the result I've extended the `SKSceneDelegate` class to build a custom `protocol/delegate` that make the transition from the `GameViewController` to the first initial controller (main menu). This method could be extended to other viewControllers of your game.




**GameViewController**:
--

    import UIKit
    import SpriteKit
    class GameViewController: UIViewController,TransitionDelegate {
        override func viewDidLoad() {
            super.viewDidLoad()
            if let view = self.view as! SKView? {
                if let scene = SKScene(fileNamed: "GameScene") {
                    scene.scaleMode = .aspectFill
                    scene.delegate = self as TransitionDelegate
                    view.presentScene(scene)
                }
                view.ignoresSiblingOrder = true
                view.showsFPS = true
                view.showsNodeCount = true
            }
        }
        func returnToMainMenu(){
            let appDelegate = UIApplication.shared.delegate as! AppDelegate
            guard  let storyboard = appDelegate.window?.rootViewController?.storyboard else { return }
            if let vc = storyboard.instantiateInitialViewController() {
                print("go to main menu")
                self.present(vc, animated: true, completion: nil)
            }
        }
    }

**GameScene**:
--

    import SpriteKit
    protocol TransitionDelegate: SKSceneDelegate {
        func returnToMainMenu()
    }
    class GameScene: SKScene {
        override func didMove(to view: SKView) {
            self.run(SKAction.wait(forDuration: 2),completion:{[unowned self] in
                guard let delegate = self.delegate else { return }
                self.view?.presentScene(nil)
                (delegate as! TransitionDelegate).returnToMainMenu()
            })
        }
        deinit {
            print("\n THE SCENE \((type(of: self))) WAS REMOVED FROM MEMORY (DEINIT) \n")
        }
    }



