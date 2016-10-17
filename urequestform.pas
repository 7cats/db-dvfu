unit urequestform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, FBEventMonitor, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, udb, menus, StdCtrls, Buttons, ExtCtrls,
  umetadata, urequestbuilder, Grids, math, ucondition, ueditform;
const
  space = 10;
type

  { TRequestForm }

  TRequestForm = class(TForm)
  AddToTableBtn: TButton;
  ChangeTableBtn: TButton;
  EraseFTableBtn: TButton;
  FBEventMonitor: TFBEventMonitor;
  Image1: TImage;
  PlusFilterBtn: TButton;
  DataSource: TDataSource;
  DBGrid: TDBGrid;
  ScrollBox : TScrollBox;
  SQLQuery: TSQLQuery;
  ApplyFiltersBtn: TToggleBox;
  procedure ChangeTableBtnClick(Sender : TObject);
  procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
    DataCol: Integer; Column: TColumn; State: TGridDrawState);
  procedure ApplyFiltersBtnClick(Sender: TObject);
  procedure EraseFTableBtnClick(Sender: TObject);
  procedure FBEventMonitorError(Sender : TObject; ErrorCode : integer);
  procedure FBEventMonitorEventAlert(Sender : TObject; EventName : string;
      EventCount : longint; var CancelAlerts : boolean);
  procedure PlusFilterBtnClick(Sender: TObject);
  procedure RemoveFilterBtnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
  procedure UpdateWidthAndCaptionGrid();
  procedure UpdateSheet();
  private
    TableIDInMetaData : integer;
    FFilters : array of TFilterComponent;
    procedure ShowWithFilters();
    procedure FilterAdd(filter : TFilterComponent);
    procedure FiltersPopBack();
  public
    constructor Create(Component : TComponent); overload;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TRequestForm }

procedure TRequestForm.ApplyFiltersBtnClick(Sender: TObject);
var
  filters : TVectorConditions;
  i : integer;
begin
  try
    for i := 0 to High(FFilters) do begin
      if (FFilters[i].FOperationCB.Text = 'Like') then begin
        filters.PushBack(FFilters[i].FFieldCB.Text, FFilters[i].FOperationCB.Text, '%' + FFilters[i].FSearchEdit.Text + '%', FFilters[i].FFieldCB.ItemIndex);
      end
      else begin
        filters.PushBack(FFilters[i].FFieldCB.Text, FFilters[i].FOperationCB.Text, FFilters[i].FSearchEdit.Text, FFilters[i].FFieldCB.ItemIndex);
      end;
  end;
  RequestBuilder.NewRequest(TableIDInMetaData, filters);
  ShowWithFilters();
  except
    ShowMessage('Введите корректно данные');
    ApplyFiltersBtn.State := cbUnchecked;
  end;
end;


procedure TRequestForm.EraseFTableBtnClick(Sender: TObject);
  procedure DeleteField(tableID, fieldID : integer);
  var
    lSQLQuery : TSQLQuery;
  begin
    lSQLQuery := TSQLQuery.Create(self);
    lSQLQuery.Transaction := DataBase.SQLTransaction;
    //ShowMessage(RequestBuilder.GetDeleteSQLText(tableID, fieldID));
    lSQLQuery.SQL.Text := RequestBuilder.GetDeleteSQLText(tableID, fieldID);
    lSQLQuery.ExecSQL;
    DataBase.SQLTransaction.Commit;
  end;
begin
  DeleteField(Self.TableIDInMetaData, DBGrid.DataSource.DataSet.FieldByName('ID').Value);
end;

procedure TRequestForm.FBEventMonitorError(Sender : TObject;
    ErrorCode : integer);
begin
  ShowMessage('Возникла ошибка, проверьте ');
end;

procedure TRequestForm.FBEventMonitorEventAlert(Sender : TObject;
    EventName : string; EventCount : longint; var CancelAlerts : boolean);
begin
  UpdateSheet();
end;


procedure TRequestForm.PlusFilterBtnClick(Sender: TObject);
var
  i : integer;
  FieldCB, OperationCB : TComboBox;
  SearchEdit : TEdit;
  CloseIm : TButton;
  panel : TPanel;
begin
  FieldCB := TComboBox.Create(Self);

  panel := TPanel.Create(Self);
  panel.Top := maxSmallint;
  panel.Align := alTop;
  panel.BevelOuter := bvNone;
  panel.Height := FieldCB.Height + space;
  panel.Parent := ScrollBox;

  FieldCB := TComboBox.Create(Self);
  FieldCB.Left := maxSmallint;
  FieldCB.Align := alLeft;
  FieldCB.BorderSpacing.Around := 6;
  FieldCB.ReadOnly := True;
  FieldCB.Parent := panel;

  for i := 0 to DBGrid.Columns.Count - 1 do  begin
    FieldCB.AddItem(DBGrid.Columns[i].Title.Caption, FieldCB);
  end;

  OperationCB := TComboBox.Create(Self);
  OperationCB.Left := maxSmallint;
  OperationCB.Align := alLeft;
  OperationCB.BorderSpacing.Around := 6;
  OperationCB.Parent := panel;
  OperationCB.ReadOnly := True;
  OperationCB.AddItem(' > ', OperationCB);
  OperationCB.AddItem(' <> ', OperationCB);
  OperationCB.AddItem(' < ', OperationCB);
  OperationCB.AddItem(' <= ', OperationCB);
  OperationCB.AddItem(' >= ', OperationCB);
  OperationCB.AddItem(' = ', OperationCB);
  OperationCB.AddItem(' Like ', OperationCB);

  SearchEdit := TEdit.Create(Self);
  SearchEdit.Left := maxSmallint;
  SearchEdit.Align := alLeft;
  SearchEdit.BorderSpacing.Around := 5;
  SearchEdit.Parent := panel;

  CloseIm := TButton.Create(Self);
  CloseIm.Left := maxSmallint;
  CloseIm.Align := alLeft;
  CloseIm.BorderSpacing.Around := 4;
  CloseIm.Parent := panel;
  CloseIm.Caption := 'X';
  CloseIm.OnMouseDown := @RemoveFilterBtnMouseDown;

  FilterAdd(TFilterComponent.Create(FieldCB, OperationCB, SearchEdit, CloseIm, panel));

  ApplyFiltersBtn.State := cbUnchecked;
end;


procedure TRequestForm.RemoveFilterBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i : integer;
  tmp : TFilterComponent;
begin
  for i := (Sender as TButton).Tag to High(FFilters) - 1 do begin
    tmp := FFilters[i];
    FFilters[i] := FFilters[i + 1];
    FFilters[i + 1] := tmp;
    FFilters[i].FPanel.Top := FFilters[i].FPanel.Top - 1;
    FFilters[i].FCloseItem.Tag := FFilters[i].FCloseItem.Tag - 1;
  end;

  ApplyFiltersBtn.Checked := false;
  FiltersPopBack();
end;


procedure TRequestForm.UpdateWidthAndCaptionGrid;
var
  i, col_grid, j : integer;
begin
  col_grid := 0;
  for i := 0 to MetaData[TableIDInMetaData].CountOfColumns() - 1 do begin
    with MetaData[TableIDInMetaData, i] do begin
      if (IsReference) then begin
        for j := 0 to ReferenceTable.CountOfColumns() - 1 do begin
          with DBGrid.Columns[col_grid] do begin
            if (ReferenceTable[j].ShowInReference) then begin
              Title.Caption := ReferenceTable[j].Caption;
              Width := space + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);
              inc(col_grid);
            end;
          end;
        end;
      end
      else begin
        with  DBGrid.Columns[col_grid] do begin
          Title.Caption := Caption;
          Width := space + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);
        end;
        inc(col_grid);
      end;
    end;
  end;
end;

procedure TRequestForm.UpdateSheet;
var
  emptyConditions : TVectorConditions;
begin
  RequestBuilder.NewRequest(TableIDInMetaData, emptyConditions);
  ShowWithFilters();
end;


procedure TRequestForm.DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  Column.Width := max(Column.Width ,space + DBGrid.Canvas.TextExtent(Column.Field.DisplayText).cx);
end;


procedure TRequestForm.ChangeTableBtnClick(Sender : TObject);
var
  fieldID : integer;
begin
  fieldID := DBGrid.DataSource.DataSet.FieldByName('ID').Value;
  if ((Sender as TButton).Caption = 'Добавить') then begin
    fieldID := -1;
  end;
  TEditForm.Create(Self.TableIDInMetaData, fieldID, Sender as TButton).Show();
end;


procedure TRequestForm.ShowWithFilters;
begin
  SQLQuery.Close;

  RequestBuilder.GetReq(SQLQuery);

  SQLQuery.Open;
  UpdateWidthAndCaptionGrid();
end;


procedure TRequestForm.FilterAdd(filter: TFilterComponent);
begin
  SetLength(FFilters, Length(FFilters) + 1);
  filter.FCloseItem.Tag := High(FFilters);
  FFilters[High(FFilters)] := filter;
end;


procedure TRequestForm.FiltersPopBack;
begin
  FFilters[High(FFilters)].Free;
  SetLength(FFilters, High(FFilters));
end;


constructor TRequestForm.Create(Component: TComponent);
begin
  inherited Create(Component);
  Caption := (Component as TMenuItem).Caption;
  TableIDInMetaData := (Component as TMenuItem).Tag;

  FBEventMonitor.Connection := DataBase.IBConnection;
  FBEventMonitor.RegisterEvents;

  SQLQuery.Transaction := DataBase.SQLTransaction;
  SQLQuery.DataBase := DataBase.IBConnection;
  UpdateSheet();
end;


destructor TRequestForm.Destroy;
var
  i : integer;
begin
  for i := 0 to High(FFilters) do begin
    FFilters[i].Free;
  end;
  inherited Destroy;
end;

end.

