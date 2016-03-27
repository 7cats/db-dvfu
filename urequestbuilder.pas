unit urequestbuilder;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, umetadata, dialogs, sqldb;

type

    { TRequestBuilder }

    TRequestBuilder = class
        constructor Create();
        procedure NewRequest(TableCaption : string);
        procedure Initial();
        procedure AddFilter(field, operation, Param : string);
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

procedure TRequestBuilder.NewRequest(TableCaption: string);
var
    i : integer;
    table : TMetaTable;
begin
    Initial();
    FRequest.Add('SELECT');
    table := MetaData[TableCaption];
    for i := 0 to table.CountOfColumns() - 1 do begin
        with table.FColumnsNames[i] do begin
            FRequest.Add(FDBName);
            if (i <> table.CountOfColumns() - 1) then begin
                FRequest.Add(',');
            end;
        end;
    end;
    FRequest.Add('FROM ' + table.FTableName.FDBName);
end;

procedure TRequestBuilder.Initial;
begin
    FRequest.Clear;
    FirstFilter := true;
    SetLength(FParams, 0);
end;

procedure TRequestBuilder.AddFilter(field, operation, Param: string);
begin
    if (not FirstFilter) then begin
        FRequest.Add('AND');
    end
    else begin
        FirstFilter := false;
    end;

    FRequest.Add('WHERE');
    FRequest.Add('Lower(' + field + ')');
    FRequest.Add(Operation);

    SetLength(FParams, Length(FParams) + 1);
    FParams[High(FParams)] := Param;

    FRequest.Add('Lower(' + ':p' + IntToStr(High(FParams)) + ')');
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

end.

