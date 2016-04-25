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
        public

        private
    end;
var
    EditForm : TEditForm;
implementation

{$R *.lfm}

end.
