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
        SaveBtn : TButton;
        CancelBtn : TButton;
        constructor Create(Component : TComponent; tableName : string);
        public

        private
            procedure CreateFieldChangers(tableName : string);
    end;
var
    EditForm : TEditForm;
implementation

{$R *.lfm}

{ TEditForm }


constructor TEditForm.Create(Component: TComponent; tableName: string);
begin
    Inherited Create(Component);
    Self.Caption := (Component as TButton).Caption + ' поле в таблицу';
    TableNameLabel.Caption := TableNameLabel.Caption + tableName;

    //CreteFieldChangers(tableName);
end;

procedure TEditForm.CreateFieldChangers(tableName : string);
begin

end;

end.
