unit urequestform;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
    DBGrids, udb, menus, StdCtrls, Buttons, ExtCtrls, umetadata, urequestbuilder,
    Grids, math, ucondition, ueditform;
const
    space = 10;
    space_btn = 8;
type

    { TRequestForm }

    TRequestForm = class(TForm)
        AddToTableBtn: TButton;
        ChangeTableBtn: TButton;
        EraseFTableBtn: TButton;
        PlusFilterBtn: TButton;
        DataSource: TDataSource;
        DBGrid: TDBGrid;
        SQLQuery: TSQLQuery;
        ApplyFiltersBtn: TToggleBox;
        procedure ChangeTableBtnClick(Sender : TObject);
        procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
            DataCol: Integer; Column: TColumn; State: TGridDrawState);
        procedure FormCreate(Sender: TObject);
        procedure ApplyFiltersBtnClick(Sender: TObject);
        procedure PlusFilterBtnClick(Sender: TObject);
        procedure RemoveFilterBtnMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure UpdateWidthAndCaptionGrid();
        private
            FCurrNewFilterPoint : TPoint;
            Table : TMetaTable;
            FFilters : array of TFilterComponent;
            CellIndex : integer;
            procedure ShowWithFilters();
            procedure FilterAdd(filter : TFilterComponent);
            procedure NewPos(filter : TFilterComponent);
            procedure FiltersPopBack();
        public
            constructor Create(Component : TComponent); overload;
            destructor Destroy; override;
    end;

implementation

{$R *.lfm}

{ TRequestForm }

procedure TRequestForm.FormCreate(Sender: TObject);
begin
    FCurrNewFilterPoint := Point(PlusFilterBtn.Left +  3 * PlusFilterBtn.Width ,PlusFilterBtn.Top);
end;


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
        RequestBuilder.NewRequest(Self.Text, filters);
        ShowWithFilters();
    except
        ShowMessage('Введите корректно данные');
        ApplyFiltersBtn.State := cbUnchecked;
    end;
end;


procedure TRequestForm.PlusFilterBtnClick(Sender: TObject);
var
    i, x, y : integer;
    FieldCB, OperationCB : TComboBox;
    SearchEdit : TEdit;
    CloseIm : TButton;
begin
    x := FCurrNewFilterPoint.x;
    y := FCurrNewFilterPoint.y;

    FieldCB := TComboBox.Create(Self);
    FieldCB.Top :=  Y;
    FieldCB.Left := X;
    FieldCB.Parent := Self;
    FieldCB.ReadOnly := True;

    X += FieldCB.Width + space;

    for i := 0 to DBGrid.Columns.Count - 1 do  begin
        FieldCB.AddItem(DBGrid.Columns[i].Title.Caption, FieldCB);
    end;

    OperationCB := TComboBox.Create(Self);
    OperationCB.Top := Y;
    OperationCB.Left := X;
    OperationCB.Parent := Self;
    OperationCB.ReadOnly := True;
    OperationCB.AddItem(' > ', OperationCB);
    OperationCB.AddItem(' <> ', OperationCB);
    OperationCB.AddItem(' < ', OperationCB);
    OperationCB.AddItem(' <= ', OperationCB);
    OperationCB.AddItem(' >= ', OperationCB);
    OperationCB.AddItem(' = ', OperationCB);
    OperationCB.AddItem(' Like ', OperationCB);

    X += OperationCB.Width + space;

    SearchEdit := TEdit.Create(nil);
    SearchEdit.Parent := Self;
    SearchEdit.Top := Y;
    SearchEdit.Left := X;

    X += SearchEdit.Width + space;

    CloseIm := TButton.Create(Self);
    CloseIm.Parent := Self;
    CloseIm.Caption := 'X';
    CloseIm.OnMouseDown := @RemoveFilterBtnMouseDown;
    CloseIm.Top := Y;
    CloseIm.Left := X;

    FilterAdd(TFilterComponent.Create(FieldCB, OperationCB, SearchEdit, CloseIm));

    FCurrNewFilterPoint.Y += FieldCB.Height + space;

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
        FFilters[i].FCloseItem.Tag := FFilters[i].FCloseItem.Tag - 1;
        NewPos(FFilters[i]);
    end;

    FCurrNewFilterPoint.Y -= (FFilters[High(FFilters)].FFieldCB.Height + space);

    ApplyFiltersBtn.Checked := false;
    FiltersPopBack();
end;


procedure TRequestForm.UpdateWidthAndCaptionGrid;
var
    i, col_grid, j : integer;
begin
    col_grid := 0;
    for i := 0 to Table.CountOfColumns() - 1 do begin
        if (Length(Table[i].FForeignFields) > 0) then begin
            for j := 0 to High((Table[i].FForeignFields)) do begin
                with DBGrid.Columns[col_grid] do begin
                    Title.Caption := Table[i].FForeignFields[j];
                    Width := space + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);
                    inc(col_grid);
                end;
            end;
        end
        else begin
           with  DBGrid.Columns[col_grid] do begin
               Title.Caption := Table[i].FCaption;
               Width := space + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);
           end;
           inc(col_grid);
        end;
    end;
end;


procedure TRequestForm.DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
    DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
    Column.Width := max(Column.Width ,space + DBGrid.Canvas.TextExtent(Column.Field.DisplayText).cx);
end;


procedure TRequestForm.ChangeTableBtnClick(Sender : TObject);
begin
    TEditForm.Create(Sender as TButton, Self.Caption).Show();
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


procedure TRequestForm.NewPos(filter: TFilterComponent);
begin
    with filter do begin
        FFieldCB.Top := FFieldCB.Top - (FFieldCB.Height + space);
        FOperationCB.Top := FOperationCB.Top - (FOperationCB.Height + space);
        FSearchEdit.Top := FSearchEdit.Top - (FSearchEdit.Height + space);
        FCloseItem.Top := FCloseItem.Top - (FCloseItem.Height + space_btn);
    end;
end;


procedure TRequestForm.FiltersPopBack;
begin
    FFilters[High(FFilters)].Free;
    SetLength(FFilters, High(FFilters));
end;


constructor TRequestForm.Create(Component: TComponent);
var
    emptyConditions : TVectorConditions;
begin
    inherited Create(Component);
    Caption := (Component as TMenuItem).Caption;

    Table := MetaData.FTables[MetaData.GetTableIndex(caption)];

    SQLQuery.Transaction := DataBase.SQLTransaction;
    SQLQuery.DataBase := DataBase.IBConnection;

    RequestBuilder.NewRequest(Self.Text, emptyConditions);
    ShowWithFilters();
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

