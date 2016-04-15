unit urequestform;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, db, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
    DBGrids, udb, menus, StdCtrls, Buttons, ExtCtrls, usqlrequestlist, umetadata,
    urequestbuilder, Grids, math;

type

     { TCloseImage }

    TCloseBtn = class(TButton)
        private
            FilterIndex : integer;
    end;

    TFilter = object
        FFieldCB, FOperationCB : TComboBox;
        FSearchEdit : TEdit;
        FCloseItem : TCloseBtn;
    end;

    { TRequestForm }

    TRequestForm = class(TForm)
            PlusFilterBtn: TButton;
        DataSource: TDataSource;
        DBGrid: TDBGrid;
        GetResFilterBtn: TSpeedButton;
        SQLQuery: TSQLQuery;
        procedure DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
            DataCol: Integer; Column: TColumn; State: TGridDrawState);
        procedure FormCreate(Sender: TObject);
        procedure GetResFilterBtnClick(Sender: TObject);
        procedure NewFilterBtnMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure NewRequstBImageMouseUp(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure PlusFilterBtnClick(Sender: TObject);
        procedure RemoveFilterMouseClick(Sender: TObject);
        procedure UpdateWidthAndCaptionGrid();
        private
            PTable : PMetaTable;
            CurrNewFilterPoint : TPoint;
            procedure ShowTable(request : TStringList);
            procedure ShowWithFilters();
        public
            constructor Create(Component : TComponent); overload;
            destructor Destroy; override;
    end;

implementation

{$R *.lfm}

{ TRequestForm }

procedure TRequestForm.FormCreate(Sender: TObject);

begin
    CurrNewFilterPoint := Point(DBGrid.Height + 10, 30);

end;

{*****************************************************************************}

procedure TRequestForm.GetResFilterBtnClick(Sender: TObject);
begin

end;

{*****************************************************************************}

procedure TRequestForm.NewFilterBtnMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    tmp : TVectorTriplet;
    a, b, c : string;
    i, j : integer;
    FieldCB, OperationCB : TComboBox;
    SearchEdit : TEdit;
    tmpCurrPos : TPoint;
begin

        //tmpCurrPos := FCurrPoint;

        FieldCB := TComboBox.Create(Self);
        FieldCB.Top :=  tmpCurrPos.Y;
        FieldCB.Left := tmpCurrPos.X;
        FieldCB.Parent := Self;

        FieldCB.ReadOnly := True;
        //FieldCB.OnChange := @ChangeParamsCB;
//      FieldCB.Tag := High(Filters);



        tmpCurrPos.X += FieldCB.Width + 10;

        for i := 0 to DBGrid.Columns.Count - 1 do  begin

            FieldCB.AddItem(DBGrid.Columns[i].Title.Caption, FieldCB);

        end;

        OperationCB := TComboBox.Create(Self);
        OperationCB.Top := tmpCurrPos.Y;
        OperationCB.Left := tmpCurrPos.X;
        OperationCB.Parent := Self;
        OperationCB.ReadOnly := True;
        OperationCB.AddItem(' > ', OperationCB);
        OperationCB.AddItem(' <> ', OperationCB);
        OperationCB.AddItem(' < ', OperationCB);
        OperationCB.AddItem(' <= ', OperationCB);
        OperationCB.AddItem(' >= ', OperationCB);
        OperationCB.AddItem(' = ', OperationCB);
        OperationCB.AddItem(' Like ', OperationCB);


        tmpCurrPos.X += OperationCB.Width + 10;
//      FOperationCB.OnChange := @ChangeParamsCmbBox;
        //FOperationCB.tag := High(Filters);

        //Add(FOperationCB.SelText);
        //Poi


        SearchEdit := TEdit.Create(nil);
//      SearchEdit.OnChange := @ChangeParamsTEdit;
        //SearchEdit.Tag := High(Filters);
        SearchEdit.Parent := Self;
        SearchEdit.Top := tmpCurrPos.Y;
        SearchEdit.Left := tmpCurrPos.X;

        tmpCurrPos.X += SearchEdit.Width + 10;

        //CloseIm := TFilterCloseImage.Create(Self);
        {CloseIm.Parent := Self;
        CloseIm.Picture.PNG.LoadFromFile('./icons/close_up.png');
        CloseIm.OnClick := @RemoveFilterMouseClick;
        CloseIm.Top := tmpCurrPos.Y;
        CloseIm.Left := tmpCurrPos.X;}


    //end;}
    //end;
    //tmp := TFilter.Create(CreatePos);
    //Filters[High(Filters)] := tmp;


//    FFilters.Add(PTable);
end;

{*****************************************************************************}

procedure TRequestForm.NewRequstBImageMouseUp(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

{*****************************************************************************}

procedure TRequestForm.PlusFilterBtnClick(Sender: TObject);
begin

end;

{*****************************************************************************}

procedure TRequestForm.RemoveFilterMouseClick(Sender: TObject);
begin

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
                        Width := 10 + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);
                        inc(col_grid);

                    end;
            end;
        end
        else begin

           with  DBGrid.Columns[col_grid] do begin

               Title.Caption := PTable^[i].FCaption;
               Width := 10 + DBGrid.Canvas.TextWidth(DBGrid.Columns[col_grid].title.caption);

           end;

           inc(col_grid);
        end;
    end;
end;

{*****************************************************************************}

procedure TRequestForm.DBGridDrawColumnCell(Sender: TObject; const Rect: TRect;
    DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
    Column.Width := max(Column.Width ,10 + DBGrid.Canvas.TextExtent(Column.Field.DisplayText).cx);
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
begin
    inherited Destroy;
end;

end.

