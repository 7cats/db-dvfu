unit urequestbuilder;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, umetadata, dialogs, sqldb, ucondition;

type

    { TRequestBuilder }

    TRequestBuilder = class
        constructor Create();
        destructor Destroy; override;
        procedure NewRequest(const TableCaption : string; const conditions : TVectorConditions);
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

{ TRequestBuilder }

constructor TRequestBuilder.Create;
begin
    FRequest := TStringList.Create();
    Initial();
end;


destructor TRequestBuilder.Destroy;
begin
    FRequest.Free;
    inherited Destroy;
end;


procedure TRequestBuilder.NewRequest(const TableCaption: string;
    const conditions: TVectorConditions);
var
    i, j, k : integer;
    tableTmp : PMetaTable;
    isFirst : boolean = true;
    ForeingFields : TVectorPairString;
    PTable : PMetaTable;

    procedure AddToRequest(toAddToRequest, addIfFirstTrue, addIfFirstFalse : string);
    begin
        if (not isFirst) then begin
            FRequest.Add(addIfFirstTrue);
        end
        else begin
            FRequest.Add(addIfFirstFalse);
            isFirst :=  false;
        end;
        FRequest.Add(toAddToRequest);
    end;

begin
    Initial();
    FRequest.Add('SELECT');

    PTable :=  MetaData[TableCaption].GetAddr();
    for i := 0 to PTable^.CountOfColumns() - 1 do begin
        with PTable^[i] do begin
            if (Length(PTable^[i].FForeignFields) > 0) then begin
                j := 0; k := 0;
                tableTmp := MetaData[FRefTableName].GetAddr();
                while (j <> Length(PTable^[i].FForeignFields)) do begin
                    if (PTable^[i].FForeignFields[j] = tableTmp^[k].FCaption) then begin
                        ForeingFields.PushBack(PTable^.FColumns[i].FRefTableName, tableTmp^.FColumns[k].FDBName);
                        AddToRequest(PTable^[i].FRefTableName + '.' + tableTmp^[k].FDBName, ',', '');
                        inc(j);
                    end;
                    inc(k);
                end;
            end
            else begin
                AddToRequest(PTable^.FDBName + '.' + PTable^.FColumns[i].FDBName, ',', '');
                ForeingFields.PushBack(PTable^.FDBName, PTable^.FColumns[i].FDBName);
            end;
        end;
    end;

    FRequest.Add('FROM ' + PTable^.FDBName);

    for i := 0 to PTable^.CountOfColumns() - 1 do begin
        with PTable^[i] do begin
            if (Length(FForeignFields) > 0) then begin
                FRequest.Add('INNER JOIN ' + FRefTableName + ' ON' + ' '  + FRefTableName   + '.' + FForeignKey + '='  + PTable^.FDBName + '.' + FDBName);
            end;
        end;
    end;
    isFirst := true;
    j := 0;
    for i := 0 to conditions.High_() do begin
        with ForeingFields[conditions[i].FIndexParam] do begin
            AddToRequest(FTableName^ + '.' + FFieldName^
                   + conditions[i].FOperation + ':p' + IntToStr(j), 'AND', 'WHERE');
        end;
        SetLength(FParams, Length(FParams) + 1);
        FParams[High(FParams)] := conditions[i].FParam;
        inc(j);
    end;

    FRequest.Add('ORDER BY ' + PTable^.FDBName + '.' + PTable^[0].FDBName);
end;


procedure TRequestBuilder.Initial;
begin
    FRequest.Clear;
    FirstFilter := true;
    SetLength(FParams, 0);
end;


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

