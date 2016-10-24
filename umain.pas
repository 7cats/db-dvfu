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
    procedure TimeTableDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    cells: array of array of string;
    procedure CreateTimeTable();
    function IsNameIsDay: integer;
    function IsNameIsTime: integer;
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

  SQLQuery.Transaction := DataBase.SQLTransaction;
  CreateDrawGrid();
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
begin
  RequestBuilder.NewRequest(MetaData.GetTableIndex('WEEKDAYS'));
  RequestBuilder.GetReq(SQLQuery);
  SQLQuery.Open();

  SetLength(cells, 1, 1);
  while (not DataSource.DataSet.EOF) do begin
    SetLength(cells, 1, Length(cells[0]) + 1);
    cells[0, High(cells[0])] := Datasource.DataSet.Fields.Fields[1].AsString;
    DataSource.DataSet.Next;
  end;

  SQLQuery.Close();
  RequestBuilder.NewRequest(MetaData.GetTableIndex('LESSONS_TIMES'));
  RequestBuilder.GetReq(SQLQuery);
  SQLQuery.Open();

  while (not DataSource.DataSet.EOF) do begin
    SetLength(cells, Length(cells) + 1, Length(cells[0]));
    cells[High(cells), 0] := Datasource.DataSet.Fields.Fields[1].AsString;
    DataSource.DataSet.Next;
  end;
  SQLQuery.Close();

  ShowMessage(inttostr(Length(cells)));
  TimeTable.RowCount := Length(cells);
  TimeTable.ColCount := Length(cells[0]);
end;


procedure TMainForm.TimeTableDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  TimeTable.Canvas.TextRect(aRect, aRect.Left, aRect.Top, cells[aRow, aCol]);
end;


procedure TMainForm.CreateTimeTable;
var
  i : integer;
  j : integer;
  dayidx, timeidx : integer;
begin
  SQLQuery.Transaction := DataBase.SQLTransaction;

  RequestBuilder.NewRequest(MetaData.GetTableIndex('TIMETABLE'));
  RequestBuilder.GetReq(SQLQuery);
  SQLQuery.Open;

  while (not DataSource.DataSet.EOF) do begin
    dayidx := -1;
    timeidx := -1;
    for i := 1 to Datasource.DataSet.Fields.Count - 1 do begin

    end;
    DataSource.DataSet.Next;
  end;
end;

function TMainForm.IsNameIsDay: integer;
var
  j : integer;
begin
  for j := 1 to High(cells) do begin
    if (Datasource.DataSet.Fields.Fields[i].AsString = cells[0, j]) then
      exit(j);
  end;
end;

function TMainForm.IsNameIsTime: integer;
var
  j : integer;
begin
  for j := 1 to High(cells[0]) do begin
    if (Datasource.DataSet.Fields.Fields[i].AsString = cells[j, 0]) then
      exit(j);
  end;
end;

end.
