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
        constructor CreateNew(Component : TComponent; tableName : TCaption);
        public

        private
            procedure CreateFieldChangers(tableName : string);
    end;
var
    EditForm : TEditForm;
implementation

{$R *.lfm}

{ TEditForm }

constructor TEditForm.CreateNew(Component : TComponent; tableName : TCaption);
begin
    Inherited Create(Component);
    Self.Caption := (Component as TButton).Caption + ' поле в таблицу';
    TableNameLabel.Caption := TableNameLabel.Caption + tableName;

    //CreteFieldChangers(tableName);
    Self.Show();
end;

procedure TEditForm.CreateFieldChangers(tableName : string);
begin

end;

end.
