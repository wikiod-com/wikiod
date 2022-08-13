---
title: "Dockable control bars (panes)"
slug: "dockable-control-bars-panes"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

Almost all MFC applications have toolbar and status bar - special types of the control bar that docked to top and bottom part of application main frame. But often application logic requires more the information bars docked to some side of frame, for example it may be properties bar or classes bar, preview bar, output bar and many others.
Classic MFC have good solution only for toolbars and others controls that not dynamically resizable. But MFC Feature Pack have advanced docking manager that allows:
 - Dynamically resize docked bars.
 - Redocking to any frame sides with live preview.
 - Serialize state and position of docked bars in registry.
 - Docking bars to same frame side and collecting them to the tabbed pane.

All you need is to inherit your docking bar from [CDockablePane](https://msdn.microsoft.com/en-us/library/bb984433(v=vs.140).aspx) class.

## Docking pane to the left side of frame.
    // Main frame class declaration
    class CMainFrame 
       : public CMDIFrameWndEx
    {
        DECLARE_DYNAMIC(CMainFrame)
    
    protected:
        // declare our pane
        CDockablePane m_wndPane;
    
        // .... other class memebers

    public:
        CMainFrame();

    protected:
        afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
        DECLARE_MESSAGE_MAP()
    };


    // Main frame class implementation
    IMPLEMENT_DYNAMIC(CMainFrame, CMDIFrameWndEx)
    
    BEGIN_MESSAGE_MAP(CMainFrame, CMDIFrameWndEx)
        ON_WM_CREATE()
    END_MESSAGE_MAP()
    
    CMainFrame::CMainFrame()
    {
    }

    int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
    {
        if (CMDIFrameWndEx::OnCreate(lpCreateStruct) == -1)
            return -1;

        // enable frame docking to any sides
        EnableDocking(CBRS_ALIGN_ANY);

        // set the visual manager used to draw all user interface elements
        CMFCVisualManager::SetDefaultManager(RUNTIME_CLASS(CMFCVisualManagerWindows));

        // enable smart docking style window behavior
        CDockingManager::SetDockingMode(DT_SMART);

    
        // Other frame initialization code
        // ....


        // Creating the pane.
        // ID_VIEW_PANE_ID - pane ID, must be declared in resource.h
        // CRect(0, 0, 100, 100) - default pane size in floating state (pane is not docked to any frame sides).
        // CBRS_LEFT - default side for pane docking.
        if (!m_wndPane.Create(_T("My Pane"), this, CRect(0, 0, 100, 100), TRUE, ID_VIEW_PANE_ID, WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | CBRS_LEFT | CBRS_FLOAT_MULTI)) {
            TRACE0("Failed to create Pane\n");
            return -1; // failed to create
        }
        // Enable docking and redocking pane to frame any sides (you can pass a combination of CBRS_ALIGN_ flags)
        m_wndPane.EnableDocking(CBRS_ALIGN_ANY);

        // Dock pane to the default (left) side of the frame.
        DockPane(&m_wndPane);

        return 0;
    }


## Docking panes to the Child frame.
Some times application must have panes that docked not to the main frame, but to the child frame. Usually it's MDI application. In MFC Feature pack such child frame is inherited from `CMDIChildWndEx` class and as main frame (inherited from `CMDIFrameWndEx`) have all required code for such docking.

But there is some tricks for child frame. And this example shows them.


    // Declare child frame
    class CChildFrame : public CMDIChildWndEx
    {
        DECLARE_DYNCREATE(CChildFrame)
    
    protected:
        // declare our pane
        CDockablePane m_wndPane;
    
    public:
        CChildFrame();

    protected:
        virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
        
        // CMDIChildWndEx class haven't serialization for state of the docking manager,
        // so we need to realize it manually.
        //
        // Docking state serialization methods:
        virtual void SaveBarState(LPCTSTR lpszProfileName) const;
        virtual void LoadBarState(LPCTSTR lpszProfileName);

    protected:
        afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
        afx_msg void OnDestroy();

        DECLARE_MESSAGE_MAP()
    };



    // CChildFrame Implementation 
    
    IMPLEMENT_DYNCREATE(CChildFrame, CMDIChildWndEx)
    
    BEGIN_MESSAGE_MAP(CChildFrame, CMDIChildWndEx)
        ON_WM_CREATE()
        ON_WM_DESTROY()
    END_MESSAGE_MAP()
    
    CChildFrame::CChildFrame()
    {
        // Trick#1: Add this line for enable floating toolbars
        m_bEnableFloatingBars = TRUE;
    }
    
    BOOL CChildFrame::PreCreateWindow(CREATESTRUCT& cs)
    {
        if( !CMDIChildWndEx::PreCreateWindow(cs) )
            return FALSE;
    
        cs.style = WS_CHILD | WS_VISIBLE | WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU
            | FWS_ADDTOTITLE | WS_THICKFRAME | WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_MAXIMIZE;
    
        // Trick#2: Add this line for remove the ugly client edge of the child frame.
        cs.dwExStyle &= (~WS_EX_CLIENTEDGE);
    
        return TRUE;
    }

    void CChildFrame::SaveBarState(LPCTSTR lpszProfileName) const
    {
        const_cast<CChildFrame*>(this)->GetDockingManager()->SaveState(lpszProfileName);
    
        // Trick#3: we need to call serialization method of CMFCToolBar panes that may be docked to the child frame.
        CObList list;
        const_cast<CChildFrame*>(this)->GetDockingManager()->GetPaneList(list, FALSE, NULL, FALSE);
        if (list.GetCount() > 0) {
            POSITION pos = list.GetTailPosition();
            while (pos != NULL) {
                CMFCToolBar* pToolBar = DYNAMIC_DOWNCAST(CMFCToolBar, list.GetPrev(pos));
                if (pToolBar != nullptr) {
                    pToolBar->SaveState(lpszProfileName);
                }
            }
        }
    }
    
    void CChildFrame::LoadBarState(LPCTSTR lpszProfileName)
    {
        // Trick#3: we need to call serialization method of CMFCToolBar panes that may be docked to the child frame.
        CObList list;
        GetDockingManager()->GetPaneList(list, FALSE, NULL, FALSE);
        if (list.GetCount() > 0) {
            POSITION pos = list.GetTailPosition();
            while (pos != NULL) {
                CMFCToolBar* pToolBar = DYNAMIC_DOWNCAST(CMFCToolBar, list.GetPrev(pos));
                if (pToolBar != nullptr) {
                    pToolBar->LoadState(lpszProfileName);
                }
            }
        }
    
        GetDockingManager()->LoadState(lpszProfileName);
        GetDockingManager()->SetDockState();
        GetDockingManager()->ShowDelayShowMiniFrames(TRUE);
    
        // Trick#4: MFC BUGFIX: force assigning the child frame docking manager to all miniframes (for details look at http://stackoverflow.com/q/39253843/987850).
        for (POSITION pos = GetDockingManager()->GetMiniFrames().GetHeadPosition(); pos != NULL;)
        {
            CWnd* pWndNext = (CWnd*)GetDockingManager()->GetMiniFrames().GetNext(pos);
            if (pWndNext != nullptr && pWndNext->IsKindOf(RUNTIME_CLASS(CPaneFrameWnd))) {
                STATIC_DOWNCAST(CPaneFrameWnd, pWndNext)->SetDockingManager(GetDockingManager());
            }
        }
    }


    int CChildFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
    {
        bool bRes = CMDIChildWndEx::OnCreate(lpCreateStruct) == 0;
        if (bRes)
        {
            // enable docking
            EnableDocking(CBRS_ALIGN_ANY);
    
            // enable Visual Studio 2005 style docking window behavior
            CDockingManager::SetDockingMode(DT_SMART);

            // Creating the pane.
            // ID_VIEW_PANE_ID - pane ID, must be declared in resource.h
            // CRect(0, 0, 100, 100) - default pane size in floating state (pane is not docked to any frame sides).
            // CBRS_LEFT - default side for pane docking.
            if (!m_wndPane.Create(_T("My Pane"), this, CRect(0, 0, 100, 100), TRUE, ID_VIEW_PANE_ID, WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | CBRS_LEFT | CBRS_FLOAT_MULTI)) {
                TRACE0("Failed to create Pane\n");
                return -1; // failed to create
            }
            // Enable docking and redocking pane to frame any sides (you can pass a combination of CBRS_ALIGN_ flags)
            m_wndPane.EnableDocking(CBRS_ALIGN_ANY);
    
            // Dock pane to the default (left) side of the frame.
            DockPane(&m_wndPane);
        }

        // Loading dock manager state
        if (bRes) {
           LoadBarState(theApp.GetRegSectionPath(_T("ChildFrame")));
        }

        return bRes ? 0 : 1;
    }

    void CChildFrame::OnDestroy()
    {
        // Save dock manager state
        SaveBarState(theApp.GetRegSectionPath(_T("ChildFrame")));

        CMDIChildWndEx::OnDestroy();
    }

