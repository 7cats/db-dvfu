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
    procedure NewRequest(const TableID : integer;
      const conditions : TVectorConditions);
    procedure Initial();
    function GetInsertSQLText(tableID: integer): string;
    function GetSelectSQLText(tableID, fieldID: integer): string;
    function GetUpdateSQLText(tableID, fieldID: integer): string;
    function GetDeleteSQLText(tableID, fieldID: integer): string;
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


procedure TRequestBuilder.NewRequest(const TableID: integer;
  const conditions: TVectorConditions);
var
  i, j : integer;
  isFirst : boolean = true;
  ForeingFields : TVectorPairString;

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

  for i := 0 to MetaData[TableID].CountOfColumns() - 1 do begin
    with MetaData[TableID, i] do begin
      if (IsReference) then begin
        for j := 0 to ReferenceTable.CountOfColumns() - 1 do begin
          if (ReferenceTable[j].ShowInReference) then begin
            ForeingFields.PushBack(ReferenceTable.DBName, ReferenceTable[j].DBName);
            AddToRequest(ReferenceTable.DBName + '.' + ReferenceTable[j].DBName, ',', '');
          end;
        end;
      end
      else begin
        AddToRequest(MetaData[TableID].DBName + '.' + DBName, ',', '');
        ForeingFields.PushBack(MetaData[TableID].DBName, DBName);
      end;
    end;
  end;

  FRequest.Add('FROM ' + MetaData[TableID].DBName);

  for i := 0 to MetaData[TableID].CountOfColumns() - 1 do begin
    with MetaData[TableID, i] do begin
      if (IsReference) then begin
        FRequest.Add('INNER JOIN ' + ReferenceTable.DBName + ' ON '
          + ReferenceTable.DBName + '.' + ReferenceTable[0].DBName + '='
          + MetaData[TableID].DBName + '.' + DBName);
      end;
    end;
  end;
  isFirst := true;
  j := 0;
  for i := 0 to conditions.High_() do begin
    with ForeingFields[conditions[i].FIndexParam] do begin
      AddToRequest(FTableName + '.' + FFieldName
           + conditions[i].FOperation + ':p' + IntToStr(j), 'AND', 'WHERE');
    end;
    SetLength(FParams, Length(FParams) + 1);
    FParams[High(FParams)] := conditions[i].FParam;
    inc(j);
  end;

  FRequest.Add('ORDER BY ' + MetaData[TableID].DBName + '.' + MetaData[TableID, 0].DBName);
end;


procedure TRequestBuilder.Initial;
begin
  FRequest.Clear;
  FirstFilter := true;
  SetLength(FParams, 0);
end;

function TRequestBuilder.GetInsertSQLText(tableID: integer): string;
begin
  result := 'INSERT INTO ' + Metadata[TableId].DBName + ' (' + MetaData[tableID].FieldListStr('%s')
    + ')  VALUES (' + Metadata[TableId].FieldListStr(':%s') + ')';
end;

function TRequestBuilder.GetSelectSQLText(tableID, fieldID: integer
    ): string;
begin
  result := 'SELECT ' + MetaData[tableID].FieldListStr('%s') + ' FROM ' + MetaData[tableID].DBName
    + ' WHERE ID = ' + IntToStr(fieldID);
end;

function TRequestBuilder.GetUpdateSQLText(tableID, fieldID: integer
    ): string;
begin
  result := 'UPDATE ' + Metadata[TableId].DBName +  ' SET ' + MetaData[tableID].FieldListStr('%s = :%1$')  + ' where id = ' + intToStr(fieldID);
  ShowMessage(Result);
end;

function TRequestBuilder.GetDeleteSQLText(tableID, fieldID: integer): string;
begin
  result := Format('DELETE FROM %s WHERE ID = %d', [Metadata[TableId].DBName, fieldID]);
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

