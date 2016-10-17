unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, Menus, ExtCtrls, Grids, udb, urequestform,
  umetadata, urequestbuilder;

type

  { TMainForm }

  TMainForm = class(TForm)
    AbAutorItem: TMenuItem;
    DataSource: TDataSource;
    SQLQuery: TSQLQuery;
    TimeTable: TDrawGrid;
    ExitItem: TMenuItem;
    FileItem: TMenuItem;
    HelpItem: TMenuItem;
    MainMenu: TMainMenu;
    ShowTableItem: TMenuItem;
    procedure AbAutorItemClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowTableOnClick(Sender: TObject);
    procedure AddNewTableItem(nameI, captionI : string; index : integer);
    procedure CreateDrawGrid();
  private
    procedure CreateTimeTable();
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AbAutorItemClick(Sender: TObject);
begin
  ShowMessage('Разумов Максим, Б8103а' + ''#13'' + 'https://github.com/7cats/db-dvfu-');
end;


procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Application.Terminate;
end;


procedure TMainForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
  DataBase.Connect();
  for i := 0 to MetaData.CountOfTables() do begin
    AddNewTableItem(MetaData[i].DBName + 'Item', MetaData[i].Caption, i);
  end;
  CreateTimeTable();
end;


procedure TMainForm.ShowTableOnClick(Sender: TObject);
begin
  TRequestForm.Create(Sender as TMenuItem).Show();
end;


procedure TMainForm.AddNewTableItem(nameI, captionI: string; index: integer);
var
  NewItem: TMenuItem;
begin
  NewItem := TMenuItem.Create(ShowTableItem);
  NewItem.Name := NameI;
  NewItem.Caption := CaptionI;
  NewItem.OnClick := @ShowTableOnClick;
  NewItem.Tag := index;
  ShowTableItem.Add(NewItem);
end;

procedure TMainForm.CreateDrawGrid;
var
  i : integer;
begin
  //for i := 0 to MetaData[GetTableIndex(WEEKDAYS)].F;
  //TimeTable;
  RequestBuilder.NewRequest(MetaData.GetTableIndex('WEEKDAYS'));
  RequestBuilder.GetReq(SQLQuery);
  SQLQuery.Open();

  while (not DataSource.DataSet.EOF) do begin
    for i := 1 to Datasource.DataSet.Fields.Count - 1 do begin
      //ShowMessage(Datasource.DataSet.Fields.Fields[i].AsString);
    end;
    DataSource.DataSet.Next;
  end;

  RequestBuilder.NewRequest(MetaData.GetTableIndex('GROUPS'));
  RequestBuilder.GetReq(SQLQuery);
  SQLQuery.Open();

  while (not DataSource.DataSet.EOF) do begin
    for i := 1 to Datasource.DataSet.Fields.Count - 1 do begin
      //ShowMessage(Datasource.DataSet.Fields.Fields[i].AsString);
    end;
    DataSource.DataSet.Next;
  end;
  SQLQuery.Close();
end;


procedure TMainForm.CreateTimeTable;
var
  i : integer;
  fields : array of array of string;
begin
  SQLQuery.Transaction := DataBase.SQLTransaction;

  RequestBuilder.NewRequest(MetaData.GetTableIndex('TIMETABLE'));
  RequestBuilder.GetReq(SQLQuery);
  SQLQuery.Open;

  //DataSource.DataSet.;
  //DataSource.DataSet.Next;
  while (not DataSource.DataSet.EOF) do begin
    for i := 1 to Datasource.DataSet.Fields.Count - 1 do begin
      //ShowMessage(Datasource.DataSet.Fields.Fields[i].AsString);
    end;
    DataSource.DataSet.Next;
  end;

  //ShowMessage( inttostr( DataSource.DataSet.Fields.Count));

end;

end.

