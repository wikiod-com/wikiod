---
title: "Retrieving updated TDataSet data in a background thread"
slug: "retrieving-updated-tdataset-data-in-a-background-thread"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

This FireDAC example, and the others I'm planning to submit, will avoid the use of native calls to asynchronously open the dataset.

## FireDAC example
The code sample below shows one way to retrieve records from an MSSql Server
in a background thread using FireDAC.  Tested for Delphi 10 Seattle

As written:

- The thread retrieves data using its own TFDConnection and TFDQuery and transfers
the data to the form's FDQuery in a call to Sychronize().

- The Execute retrieves the data only once.  It could be altered to run the
query repeatedly in response to a message posted from the VCL thread.

Code:

      type
        TForm1 = class;
    
      TFDQueryThread = class(TThread)
      private
        FConnection: TFDConnection;
        FQuery: TFDQuery;
        FForm: TForm1;
      published
        constructor Create(AForm : TForm1);
        destructor Destroy; override;
        procedure Execute; override;
        procedure TransferData;
        property Query : TFDQuery read FQuery;
        property Connection : TFDConnection read FConnection;
        property Form : TForm1 read FForm;
      end;
    
      TForm1 = class(TForm)
        FDConnection1: TFDConnection;
        FDQuery1: TFDQuery;
        DataSource1: TDataSource;
        DBGrid1: TDBGrid;
        DBNavigator1: TDBNavigator;
        Button1: TButton;
        procedure FormDestroy(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
      private
      public
        QueryThread : TFDQueryThread;
      end;
    
      var
      Form1: TForm1;
    
      implementation
    
      {$R *.dfm}
    
      { TFDQueryThread }
    
      constructor TFDQueryThread.Create(AForm : TForm1);
      begin
        inherited Create(True);
        FreeOnTerminate := False;
        FForm := AForm;
        FConnection := TFDConnection.Create(Nil);
        FConnection.Params.Assign(Form.FDConnection1.Params);
        FConnection.LoginPrompt := False;
    
        FQuery := TFDQuery.Create(Nil);
        FQuery.Connection := Connection;
        FQuery.SQL.Text := Form.FDQuery1.SQL.Text;
      end;
    
      destructor TFDQueryThread.Destroy;
      begin
        FQuery.Free;
        FConnection.Free;
        inherited;
      end;
    
      procedure TFDQueryThread.Execute;
      begin
        Query.Open;
        Synchronize(TransferData);
      end;
    
      procedure TFDQueryThread.TransferData;
      begin
        Form.FDQuery1.DisableControls;
        try
          if Form.FDQuery1.Active then
            Form.FDQuery1.Close;
          Form.FDQuery1.Data := Query.Data;
        finally
          Form.FDQuery1.EnableControls;
        end;
      end;
    
      procedure TForm1.FormDestroy(Sender: TObject);
      begin
        QueryThread.Free;
      end;
    
      procedure TForm1.Button1Click(Sender: TObject);
      begin
        if not QueryThread.Finished then
          QueryThread.Start
        else
          ShowMessage('Thread already executed!');
      end;
    
      procedure TForm1.FormCreate(Sender: TObject);
      begin
        FDQuery1.Open;
        QueryThread := TFDQueryThread.Create(Self);
      end;
    
      end.



