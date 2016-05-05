unit ueditform;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Dialogs, DBCtrls,
    ExtCtrls, StdCtrls, sqldb, DB, udb, umetadata,
    Graphics, Buttons;

type

    {TEditForm}

    TEditForm = class(TForm)
        TableNameLabel : TLabel;
        PanelTop : TPanel;
        PanelBottom : TPanel;
        SaveBtn : TButton;
        CancelBtn : TButton;
        DataScrollBox : TScrollBox;
        constructor Create(tableID, fieldID : integer; Component : TComponent);
        public

        private
            procedure CreateFieldChangers(tableName : string);
    end;
var
    EditForm : TEditForm;
implementation

{$R *.lfm}

{ TEditForm }


constructor TEditForm.Create(tableID, fieldID: integer; Component: TComponent);
begin
    Inherited Create(Component);
    Self.Caption := (Component as TButton).Caption + ' поле в таблицу';
    TableNameLabel.Caption := TableNameLabel.Caption + '"' + MetaData[tableID].Caption + '"';

    //CreteFieldChangers(tableName);
end;


procedure TEditForm.CreateFieldChangers(tableName : string);
begin

end;

end.
