unit ueditform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, DBCtrls, ExtCtrls, StdCtrls,
  sqldb, DB, udb, umetadata, Graphics, Buttons, DBDateTimePicker, urequestbuilder;

type

  {TEditForm}

  TEditForm = class(TForm)
    DataSource: TDataSource;
    SQLQuery: TSQLQuery;
    TableNameLabel : TLabel;
    PanelTop : TPanel;
    PanelBottom : TPanel;
    SaveBtn : TButton;
    CancelBtn : TButton;
    ScrollBox : TScrollBox;
    procedure CancelBtnClick(Sender: TObject);
    constructor Create(tableID, fieldID : integer; Component : TComponent);
    procedure FormCreate(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    public

    private
      FTableID, FFieldID : integer;
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
  FFieldID := fieldID;
  FTableID := tableID;
  SQLQuery.Transaction := DataBase.SQLTransaction;
end;

procedure TEditForm.CancelBtnClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TEditForm.FormCreate(Sender: TObject);
var
  i : integer;
  lDataSource : TDataSource;
  localSQLQuery : TSQLQuery;
  s : string;

  procedure CreateEditor(AColumn : TMetaColumn);
  var
    panel : TPanel;
    fieldName : TLabel;
    DBEdit : TDBEdit;
    DBComboBox : TDBLookupComboBox;
  begin
    panel := TPanel.Create(Self);// TODO : закончить генерацию
    panel.Align := alTop;
    panel.BorderStyle := bsNone;
    panel.BevelOuter := bvNone;
    panel.Parent := ScrollBox;

    fieldName := TLabel.Create(Self);
    fieldName.Align := alLeft;
    fieldName.Parent := panel;

    if (not AColumn.IsReference) then begin
      fieldName.Caption := AColumn.Caption;
      DBEdit := TDBEdit.Create(Panel);
      DBEdit.Align := alRight;
      DBEdit.BorderSpacing.Around := 1;
      DBEdit.DataSource := Datasource;
      DBEdit.DataField := AColumn.DBName;
      DBEdit.Parent := Panel;
    end
    else begin
      fieldName.Caption := AColumn.ReferenceTable.Caption;

      localSQLQuery := TSQLQuery.Create(Self);
      lDataSource := TDataSource.Create(Self);
      lDataSource.DataSet := localSQLQuery;
      localSQLQuery.Transaction := DataBase.SQLTransaction;

      localSQLQuery.SQL.Text := 'SELECT ID, (' + AColumn.ReferenceTable.FieldListStrNoID('%s', ' || ''  '' || ') + ') as NAME FROM ' + AColumn.ReferenceTable.DBName + ' ORDER BY NAME';
      localSQLQuery.Open;

      DBComboBox := TDBLookupComboBox.Create(Panel);
      DBComboBox.Align:= alRight;
      DBComboBox.BorderSpacing.Around := 1;
      DBComboBox.Parent := Panel;
      DBComboBox.DataSource := DataSource;
      DBComboBox.DataField := AColumn.DBName;
      DBComboBox.ListSource := lDataSource;
      DBComboBox.ListField := 'NAME';
      DBComboBox.KeyField := 'ID';
      DBComboBox.ReadOnly := true;
    end;
end;

begin
  SQLQuery.Transaction := DataBase.SQLTransaction;

  SQLQuery.SQL.Text := RequestBuilder.GetSelectSQLText(FTableID, FFieldID);
  if (FFieldID = -1) then begin
    SQLQuery.InsertSQL.Text := RequestBuilder.GetInsertSQLText(FTableID);
    SQLQuery.Open;
    SQLQuery.Append;
		end
  else begin
    SQLQuery.UpdateSQL.Text := RequestBuilder.GetUpdateSQLText(FTableID, FFieldID);
    SQLQuery.Open;
    SQLQuery.Edit;
  end;
  Datasource.DataSet.FieldByName('ID').Required := false;

  for i := MetaData[FTableID].CountOfColumns() - 1 downto 0 do begin
    CreateEditor(MetaData[FTableID, i]);
  end;
end;

procedure TEditForm.SaveBtnClick(Sender: TObject);
begin
  SQLQuery.Post();
  DataBase.SQLTransaction.Commit();
  Self.Close;
end;

end.
