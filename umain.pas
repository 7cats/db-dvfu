unit UMain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, IBConnection, sqldb, db, FileUtil, Forms, Controls,
    Graphics, Dialogs, DBGrids, Menus, utable;

type

    { TMainForm }

    TMainForm = class(TForm)
        DataSource: TDataSource;
        DBGrid: TDBGrid;
        IBConnection: TIBConnection;
        MainMenu: TMainMenu;
        FileItem: TMenuItem;
        AbAutorItem: TMenuItem;
        ExitItem: TMenuItem;
        ShowTableItem: TMenuItem;
        HelpItem: TMenuItem;
        SQLQuery: TSQLQuery;
        SQLTransaction: TSQLTransaction;
        procedure AbAutorItemClick(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure ExitItemClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ShowTableOnClick(Sender: TObject);
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

procedure TMainForm.Button1Click(Sender: TObject);
begin
    SQLQuery.Active:= true;
end;

procedure TMainForm.AbAutorItemClick(Sender: TObject);
begin
    ShowMessage('Разумов Максим, Б8103а' + ''#10'' + ' https://github.com/7cats/db-dvfu-');
end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
    Application.Terminate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    SQLTableList := TSQLTableList.Create();
    //(sqlpath : string; ItemCap: string; ItemName: string; ParentItem : TMenuItem);
    SQLTableList.NewItem('./sqls/groups.sql', 'Группы', 0, ShowTableItem, @ShowTableOnClick);
    SQLTableList.NewItem('./sqls/lessons.sql', 'Предметы', 1, ShowTableItem, @ShowTableOnClick);
    SQLTableList.NewItem('./sqls/teachers.sql', 'Преподаватели', 2, ShowTableItem, @ShowTableOnClick);
    SQLTableList.NewItem('./sqls/timelessons.sql', 'Время занятий', 3, ShowTableItem, @ShowTableOnClick);
end;

procedure TMainForm.ShowTableOnClick(Sender: TObject);
begin
    SQLTableList[(Sender as TMenuItem).Tag].ShowTable(SQLQuery);
end;

























end.

