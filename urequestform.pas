unit urequestform;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
    DBGrids, udb, menus, StdCtrls, Buttons, ExtCtrls, usqlrequestlist, umetadata,
    urequestbuilder, Grids, math, utility;
const
    space = 10;
    space_btn = 8;
type

    { TRequestForm }

    TRequestForm = class(TForm)
        PlusFilterBtn: TButton;
        DataSource: TDataSource;
        DBGrid: TDBGrid;
        AppllyFiltersBtn: TSpeedButton;
        SQLQuery: TSQLQuery;
        procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
            DataCol: Integer; Column: TColumn; State: TGridDrawState);
        procedure FormCreate(Sender: TObject);
        procedure AppllyFiltersBtnClick(Sender: TObject);
        procedure PlusFilterBtnClick(Sender: TObject);
        procedure PlusFilterBtnMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure UpdateWidthAndCaptionGrid();
        private
            PTable : PMetaTable;
            FCurrNewFilterPoint : TPoint;
            FFilters : array of TFilter;
            procedure ShowTable(request : TStringList);
            procedure ShowWithFilters();
            procedure FilterAdd(filter : TFilter);
            procedure NewPos(filter : TFilter);
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

{*****************************************************************************}

procedure TRequestForm.AppllyFiltersBtnClick(Sender: TObject);
begin

end;

{*****************************************************************************}

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
    //FieldCB.OnChange := @ChangeParamsCB;

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
    //FOperationCB.OnChange := @ChangeParamsCmbBox;

    X += OperationCB.Width + space;

    SearchEdit := TEdit.Create(nil);
//      SearchEdit.OnChange := @ChangeParamsTEdit;
    SearchEdit.Parent := Self;
    SearchEdit.Top := Y;
    SearchEdit.Left := X;

    X += SearchEdit.Width + space;

    CloseIm := TButton.Create(Self);
    CloseIm.Parent := Self;
    CloseIm.Caption := 'X';
    CloseIm.OnMouseDown := @PlusFilterBtnMouseDown;
    CloseIm.Top := Y;
    CloseIm.Left := X;

    FilterAdd(TFilter.Create(FieldCB, OperationCB, SearchEdit, CloseIm));

    FCurrNewFilterPoint.Y += FieldCB.Height + space;

end;

{*****************************************************************************}

procedure TRequestForm.PlusFilterBtnMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    i : integer;
    tmp : TFilter;
begin
    ShowMessage( IntToStr((Sender as TButton).Tag) + ' ' +  IntToStr(High(FFilters) - 1));

    for i := (Sender as TButton).Tag to High(FFilters) - 1 do begin
        tmp := FFilters[i];
        FFilters[i] := FFilters[i + 1];
        FFilters[i + 1] := tmp;
        FFilters[i].FCloseItem.Tag := FFilters[i].FCloseItem.Tag - 1;
        NewPos(FFilters[i]);
    end;

    FCurrNewFilterPoint.Y -= (FFilters[High(FFilters)].FFieldCB.Height + space);
    FiltersPopBack();
end;

{*****************************************************************************}

procedure TRequestForm.UpdateWidthAndCaptionGrid;
var
    i, col_grid, j : integer;
begin
    col_grid := 0;
    for i := 0 to PTable^.CountOfColumns() - 1 do begin

        if (Length(PTable^[i].FForeignFields) > 0) then begin

            for j := 0 to High((PTable^[i].FForeignFields)) do begin

                    with DBGrid.Columns[col_grid] do begin

                        Title.Caption := PTable^[i].FForeignFields[j];
                        Width := space + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);
                        inc(col_grid);

                    end;
            end;
        end
        else begin

           with  DBGrid.Columns[col_grid] do begin

               Title.Caption := PTable^[i].FCaption;
               Width := space + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);

           end;

           inc(col_grid);
        end;
    end;
end;

{*****************************************************************************}

procedure TRequestForm.DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
    DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
    Column.Width := max(Column.Width ,space + DBGrid.Canvas.TextExtent(Column.Field.DisplayText).cx);
end;

{*****************************************************************************}

procedure TRequestForm.ShowTable(request: TStringList);
begin
    SQLQuery.Close;
    SQLQuery.SQL := request;
    SQLQuery.Open;
    UpdateWidthAndCaptionGrid();
end;

{*****************************************************************************}

procedure TRequestForm.ShowWithFilters;
begin
    SQLQuery.Close;

    RequestBuilder.GetReq(SQLQuery);

    SQLQuery.Open;
    UpdateWidthAndCaptionGrid();
end;

{*****************************************************************************}

procedure TRequestForm.FilterAdd(filter: TFilter);
begin
    SetLength(FFilters, Length(FFilters) + 1);
    filter.FCloseItem.Tag := High(FFilters);
    FFilters[High(FFilters)] := filter;
end;

procedure TRequestForm.NewPos(filter: TFilter);
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

{*****************************************************************************}

constructor TRequestForm.Create(Component: TComponent);
begin
    inherited Create(Component);
    Caption := (Component as TMenuItem).Caption;

    PTable := MetaData.FTables[MetaData.GetTableIndex(caption)].GetAddr();
    Self.Show();

    SQLQuery.Transaction := DataBase.SQLTransaction;
    SQLQuery.DataBase := DataBase.IBConnection;
    ShowTable(SQLRequestList[(Component as TMenuItem).Tag]);
end;

{*****************************************************************************}

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

