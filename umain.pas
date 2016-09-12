unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs, DBGrids, Menus, ExtCtrls, udb, urequestform, umetadata;

type

  { TMainForm }

  TMainForm = class(TForm)
    AbAutorItem: TMenuItem;
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
  private
    { private declarations }
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

end.

