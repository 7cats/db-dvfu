unit urequestbuilder;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, umetadata, dialogs, sqldb;

type

    { TPairString }

    TPairString = object
        FTableName, FFieldName : ^string;
        procedure MakePair(var first, second : string);
    end;

    { TVectorPairString }

    TVectorPairString = object
        private
            function GetItem(index : integer) : TPairString;
        public
            FPairs : array of TPairString;
            procedure PushBack(var first, second : string);
            property Item[index : integer] : TPairString read GetItem; default;
            function High_() : integer;
    end;

    { TTriplet }

    TTriplet = object
        FField, FOperation, FParam : string;
        FIndexParam : integer;
        procedure MakeTriplet(const Field, Operation, Param : string; index : integer);
    end;

    { TVectorTriplet }

    TVectorTriplet = object
        private
            function GetItem(index : integer) : TTriplet;
        public
            FTriplets : array of TTriplet;
            procedure PushBack(var Field, Operation, Param : string; index : integer);
            property Item[index : integer] : TTriplet read GetItem; default;
            function High_() : integer;
    end;

    { TRequestBuilder }

    TRequestBuilder = class
        constructor Create();
        destructor Destroy; override;
        procedure NewRequest(const TableCaption : string; const conditions : TVectorTriplet);
        procedure Initial();
        procedure GetReq(var SQLQuery : TSQLQuery);
        private
            FRequest : TStringList;
            FirstFilter : Boolean;
            FParams : array of String;
    end;

var
    RequestBuilder : TRequestBuilder;

implementation

{ TVectorTriplet }

procedure TVectorTriplet.PushBack(var Field, Operation, Param: string;
    index: integer);
var
    tmp : TTriplet;
begin
    tmp.MakeTriplet(Field, Operation, Param, index);
    SetLength(FTriplets, Length(FTriplets) + 1);
    FTriplets[Self.High_()] := tmp;
end;

{*****************************************************************************}

function TVectorTriplet.High_: integer;
begin
    result := High(FTriplets);
end;

{*****************************************************************************}

function TVectorTriplet.GetItem(index: integer): TTriplet;
begin
    Assert((0 <= index) and (index <= High(FTriplets)));
    result := FTriplets[index];
end;

{*****************************************************************************}


{ TVectorPairString }

procedure TVectorPairString.PushBack(var first, second: string);
var
    tmp : TPairString;
begin
    tmp.MakePair(first, second);
    SetLength(FPairs, Length(FPairs) + 1);
    FPairs[High(FPairs)] := tmp;
end;

{*****************************************************************************}

function TVectorPairString.High_: integer;
begin
    result := High(FPairs);
end;

{*****************************************************************************}

function TVectorPairString.GetItem(index: integer): TPairString;
begin
    Assert((0 <= index) and (index <= High(FPairs)));
    result := FPairs[index];
end;

{*****************************************************************************}


{ TPairString }

procedure TPairString.MakePair(var first, second: string);
begin
    FTableName := @first;
    FFieldName := @second;
end;

{*****************************************************************************}


{ TTriplet }

procedure TTriplet.MakeTriplet(const Field, Operation, Param: string;
    index: integer);
begin
    FField := Field;
    FOperation := Operation;
    FParam := Param;
    FIndexParam := index;
end;

{*****************************************************************************}


{ TRequestBuilder }

constructor TRequestBuilder.Create;
begin
    FRequest := TStringList.Create();
    Initial();
end;

{*****************************************************************************}

destructor TRequestBuilder.Destroy;
begin
    FRequest.Free;
    inherited Destroy;
end;

{*****************************************************************************}

procedure TRequestBuilder.NewRequest(const TableCaption: string;
    const conditions: TVectorTriplet);
var
    i, j, k : integer;
    tableTmp : PMetaTable;
    first : boolean = true;
    ForeingFields : TVectorPairString;
    PTable : PMetaTable;
begin
    Initial();
    FRequest.Add('SELECT');

    PTable :=  MetaData[TableCaption].GetAddr();

    ShowMessage(PTable^.FCaption);
    for i := 0 to PTable^.CountOfColumns() - 1 do begin

            if (Length(PTable^[i].FForeignFields) > 0) then begin

            j := 0; k := 0;
            tableTmp := MetaData[PTable^[i].FRefTableName].GetAddr();
            while (j <> Length(PTable^[i].FForeignFields)) do begin

                if (PTable^[i].FForeignFields[j] = tableTmp^[k].FCaption) then begin

                    ForeingFields.PushBack(PTable^.FColumns[i].FRefTableName, tableTmp^.FColumns[k].FDBName);
                    if (not first) then begin
                        FRequest.Add(',');
                    end
                    else begin
                        first :=  false;
                    end;

                    FRequest.Add(PTable^[i].FRefTableName + '.' + tableTmp^[k].FDBName);
                    inc(j);
                end;

                inc(k);
            end;

        end
        else begin

            if (not first) then begin
                FRequest.Add(',');
            end
            else begin
                first :=  false;
            end;
            FRequest.Add(PTable^.FDBName + '.' + PTable^.FColumns[i].FDBName);
            ForeingFields.PushBack(PTable^.FDBName, PTable^.FColumns[i].FDBName);
        end;

    end;
    FRequest.Add('FROM ' + PTable^.FDBName);

    for i := 0 to PTable^.CountOfColumns() - 1 do begin

         if (Length(PTable^[i].FForeignFields) > 0) then begin

             FRequest.Add('INNER JOIN');
             FRequest.Add(PTable^[i].FRefTableName + ' ON'
                        + ' '  + PTable^[i].FRefTableName + '.' + PTable^[i].FForeignKey
                        + '=' + PTable^.FDBName    + '.' + PTable^[i].FDBName);

         end;
    end;

    first := true;
    j := 0;
    for i := 0 to conditions.High_() do begin
        if (not first) then begin
            FRequest.Add('AND');
        end
        else begin
            FRequest.Add('WHERE');
            first := false;
        end;

        FRequest.Add(ForeingFields[conditions[i].FIndexParam].FTableName^ + '.' + ForeingFields[conditions[i].FIndexParam].FFieldName^
                    + conditions[i].FOperation + ':p' + IntToStr(j) );
        SetLength(FParams, Length(FParams) + 1);
        FParams[High(FParams)] := conditions[i].FParam;

    end;

    FRequest.Add('ORDER BY ' + PTable^.FDBName + '.' + PTable^[0].FDBName);
end;

{*****************************************************************************}

procedure TRequestBuilder.Initial;
begin
    FRequest.Clear;
    FirstFilter := true;
    SetLength(FParams, 0);
end;

{*****************************************************************************}

procedure TRequestBuilder.GetReq(var SQLQuery : TSQLQuery);
var
    i : integer;
begin
    SQLQuery.SQL := FRequest;
    SQLQuery.Prepare;
    for i := 0 to High(FParams) do begin
        with SQLQuery.Params do begin
            ParamByName('p' + IntToStr(i)).AsString := FParams[i];
        end;
    end;
end;

initialization
    RequestBuilder := TRequestBuilder.Create();

finalization
    RequestBuilder.Free;
end.

