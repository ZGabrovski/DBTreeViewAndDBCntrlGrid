unit uTreeViewTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  StdCtrls, ExtCtrls, DBCtrls, DBTreeView, dbcntrlgrid, DB, SQLDB,IBConnection;

type

  { TfTreeViewVes }

  TfTreeViewVes = class(TForm)
    ActionList1: TActionList;
    AddChild: TAction;
    AddFirstChild: TAction;
    AddSibling: TAction;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBCntrlGrid1: TDBCntrlGrid;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBNavigator1: TDBNavigator;
    DBTreeView1: TDBTreeView;
    DeleteNode: TAction;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblData: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    IBConnection1 : TIBConnection;
    SQLQuery1: TSQLQuery;
    SQLQuery1BUDGET: TBCDField;
    SQLQuery1CHILDCOUNT: TLargeintField;
    SQLQuery1DEPARTMENT: TStringField;
    SQLQuery1DEPT_NO: TStringField;
    SQLQuery1HEAD_DEPT: TStringField;
    SQLQuery1IMAGEINDEX: TLongintField;
    SQLQuery1LOCATION: TStringField;
    SQLQuery1MNGR_NO: TSmallintField;
    SQLQuery1PHONE_NO: TStringField;
    SQLQuery2: TSQLQuery;
    SQLQuery2DEPT_NO: TStringField;
    SQLQuery2EMP_NO: TSmallintField;
    SQLQuery2FIRST_NAME: TStringField;
    SQLQuery2FULL_NAME: TStringField;
    SQLQuery2HIRE_DATE: TDateTimeField;
    SQLQuery2JOB_CODE: TStringField;
    SQLQuery2JOB_COUNTRY: TStringField;
    SQLQuery2JOB_GRADE: TSmallintField;
    SQLQuery2LAST_NAME: TStringField;
    SQLQuery2PHONE_EXT: TStringField;
    SQLQuery2SALARY: TBCDField;
    SQLTransaction1: TSQLTransaction;
    SQLTransaction2: TSQLTransaction;
    procedure AddChildExecute(Sender: TObject);
    procedure AddChildUpdate(Sender: TObject);
    procedure AddFirstChildExecute(Sender: TObject);
    procedure AddSiblingExecute(Sender: TObject);
    procedure DBTreeView1SelectionChanged(Sender: TObject);
    procedure DeleteNodeExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  fTreeViewVes: TfTreeViewVes;

implementation

{$R *.lfm}

{ TfTreeViewVes }

procedure TfTreeViewVes.DeleteNodeExecute(Sender: TObject);
begin
  if MessageDlg(Format('Do you want to delete the %s department?',[DBTreeview1.Selected.Text]),
              mtConfirmation,[mbYes,mbNo],0) = mrYes then
    TDBTreeNode(DBTreeview1.Selected).DeleteAll
end;

procedure TfTreeViewVes.FormShow(Sender: TObject);
begin
SQLQuery1.Open;
end;

procedure TfTreeViewVes.AddChildUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := DBTreeView1.Selected <> nil
end;

procedure TfTreeViewVes.AddFirstChildExecute(Sender: TObject);
begin
  DBTreeView1.Selected.Expand(true);
  DBTreeView1.Selected := DBTreeView1.Items.AddChildFirst(DBTreeView1.Selected,'');
  DBTreeView1.Selected.Expand(true);
  DBTreeView1.Selected.EditText;
end;

procedure TfTreeViewVes.AddSiblingExecute(Sender: TObject);
begin
  DBTreeView1.Selected := DBTreeView1.Items.Add(DBTreeView1.Selected,'');
  DBTreeView1.Selected.EditText;
end;

procedure TfTreeViewVes.DBTreeView1SelectionChanged(Sender: TObject);
begin
  if Assigned( DBTreeView1.Selected ) then begin
    lblData.Caption := TDBTreeNode(DBTreeView1.Selected).DataValue.Values['PHONE_NO'];
    DBCntrlGrid1.BeginUpdate;
    try
    SQLQuery2.close;
    if SQLTransaction2.Active then
      SQLTransaction2.Commit;
    SQLQuery2.ParamByName('DEPT_NO').Value := SQLQuery1.FieldByName('DEPT_NO').Value;
    SQLTransaction2.StartTransaction;
    SQLQuery2.Open;
    finally
    DBCntrlGrid1.EndUpdate();
    end;
    end;
end;

procedure TfTreeViewVes.AddChildExecute(Sender: TObject);
begin
  DBTreeView1.Selected.Expand(true);
  DBTreeView1.Selected := DBTreeView1.Items.AddChild(DBTreeView1.Selected,'');
  DBTreeView1.Selected.Expand(true);
  DBTreeView1.Selected.EditText;
end;

end.

