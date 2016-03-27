unit urequestform;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
    DBGrids, udb, menus, StdCtrls, usqlrequestlist;

type

    { TRequestForm }

    TRequestForm = class(TForm)
        DataSource: TDataSource;
        DBGrid: TDBGrid;
        SQLQuery: TSQLQuery;
        ToggleBox1: TToggleBox;
        procedure FormCreate(Sender: TObject);
    private
        procedure ShowTable(request : TStringList);
    public
        constructor Create(Component : TComponent); overload;
    end;

var
    RequestForm: TRequestForm;

implementation

{$R *.lfm}

{ TRequestForm }

procedure TRequestForm.FormCreate(Sender: TObject);
begin

end;

procedure TRequestForm.ShowTable(request: TStringList);
begin
    SQLQuery.Close;
    SQLQuery.SQL := request;
    SQLQuery.Open;
end;

constructor TRequestForm.Create(Component: TComponent);
begin
    inherited Create(Component);
    Caption := (Component as TMenuItem).Caption;
    Self.Show();

    SQLQuery.Transaction := DataBase.SQLTransaction;
    SQLQuery.DataBase := DataBase.IBConnection;
    ShowTable(SQLRequestList[(Component as TMenuItem).Tag]);

end;

end.

