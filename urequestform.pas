unit urequestform;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
    DBGrids, udb, menus, StdCtrls, Buttons, ExtCtrls, usqlrequestlist, umetadata,
    urequestbuilder, Grids, math;

type

    { TFilter }

    TFilter = record
        FFieldCB, FOperationCB : TComboBox;
        FSearchEdit : TEdit;
    end;

    { TFiltersVector }

    TFiltersVector = class
        constructor Create();
        procedure Add();
        procedure Remove();
        function GetItem(i : integer) : TFilter;
        property Items[index : integer] : TFilter read GetItem; default;
        private
            FFilters : array of TFilter;
    end;

    { TRequestForm }

    TRequestForm = class(TForm)
        BitBtn1: TBitBtn;
        BitBtn2: TBitBtn;
        DataSource: TDataSource;
        DBGrid: TDBGrid;
        SQLQuery: TSQLQuery;
        procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
            DataCol: Integer; Column: TColumn; State: TGridDrawState);
        procedure FormCreate(Sender: TObject);
        procedure NewFilterBtnMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure UpdateWidthAndCaptionGrid();
    private
        Filters : TFiltersVector;
        procedure ShowTable(request : TStringList);
        procedure ShowWithFilters();
    public
        constructor Create(Component : TComponent); overload;
    end;

implementation

{$R *.lfm}

{ TFiltersVector }

constructor TFiltersVector.Create;
begin
end;

procedure TFiltersVector.Add;
begin

end;

procedure TFiltersVector.Remove;
begin

end;

function TFiltersVector.GetItem(i: integer): TFilter;
begin
    result := FFilters[i];
end;

{ TRequestForm }

procedure TRequestForm.FormCreate(Sender: TObject);
begin
    Filters := TFiltersVector.Create();
end;



procedure TRequestForm.NewFilterBtnMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    tmp : TVectorTriplet;
    a, b, c : string;
begin
    a := 'Фамилия';
    b := '=';
    c := 'Кленин';
    tmp.PushBack (a, b, c , 6);
    RequestBuilder.NewRequest('Расписание', tmp);
    ShowWithFilters();
end;

procedure TRequestForm.UpdateWidthAndCaptionGrid;
var
    i, col_grid, j : integer;
    table : TMetaTable;
begin
    col_grid := 0;
    table := MetaData[Caption];
    for i := 0 to table.CountOfColumns() - 1 do begin

        if (Length(table[i].FForeignFields) > 0) then begin

            for j := 0 to High((table[i].FForeignFields)) do begin

                    with DBGrid.Columns[col_grid] do begin

                        Title.Caption := table[i].FForeignFields[j];
                        Width := 10 + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);
                        inc(col_grid);

                    end;
            end;
        end
        else begin

           with  DBGrid.Columns[col_grid] do begin

               Title.Caption := table[i].FCaption;
               Width := 10 + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);

           end;

           inc(col_grid);
        end;
    end;
end;

procedure TRequestForm.DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
    DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
    Column.Width := max(Column.Width ,10 + DBGrid.Canvas.TextExtent(Column.Field.DisplayText).cx);
end;

procedure TRequestForm.ShowTable(request: TStringList);
begin
    SQLQuery.Close;
    SQLQuery.SQL := request;
    SQLQuery.Open;
    UpdateWidthAndCaptionGrid();
end;

procedure TRequestForm.ShowWithFilters;
begin
    SQLQuery.Close;

    RequestBuilder.GetReq(SQLQuery);

    SQLQuery.Open;
    UpdateWidthAndCaptionGrid();
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

