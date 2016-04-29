unit usqlrequestlist;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    { TSQLRequest }

   TSQLRequest = class
        private
            FSQLReq : TStringList;
        public
            constructor Create(const sqlpath: string);
            destructor Destroy; override;
    end;

    { TSQLRequestList }

    TSQLRequestList = class(TSQLRequest)
        private
            FTables: array of TSQLRequest;
            function GetItem( index : integer) : TStringList;
        public
            procedure NewItem(const sqlpath : string);
            property Items[index : integer] : TStringList read GetItem; default;
            Constructor Create();
            destructor Destroy; override;
    end;

var
    SQLRequestList : TSQLRequestList;
implementation

{ TSQLRequestList }

function TSQLRequestList.GetItem(index: integer): TStringList;
begin
    Result := FTables[index].FSQLReq;
end;


procedure TSQLRequestList.NewItem(const sqlpath: string);
var
    NewSQLTable : TSQLRequest;
begin
    NewSQLTable := TSQLRequest.Create(sqlpath);
    SetLength(FTables, Length(FTables) + 1);
    FTables[High(FTables)] := NewSQLTable;
end;


constructor TSQLRequestList.Create();
begin
end;


destructor TSQLRequestList.Destroy;
var
    i : integer;
begin
    for i := 0 to High(FTables) do begin
        FTables[i].Free;
    end;
    inherited Destroy;
end;


constructor TSQLRequest.Create(const sqlpath: string);
var
    s : string;
begin
    FSQLReq := TStringList.Create();
    try
        assignFile(input, sqlpath);
        reset(input);
        while (not EOF) do begin
            readln(s);
            FSQLReq.Add(s);
        end;
        Close(input);
    Except
        Raise Exception.Create('Error :(');
    end;
end;


destructor TSQLRequest.Destroy;
begin
    FSQLReq.Free;
    inherited Destroy;
end;


initialization
    SQLRequestList := TSQLRequestList.Create();

    SQLRequestList.NewItem('./sqls/groups.sql');
    SQLRequestList.NewItem('./sqls/lessons.sql');
    SQLRequestList.NewItem('./sqls/teachers.sql');
    SQLRequestList.NewItem('./sqls/timelessons.sql');
    SQLRequestList.NewItem('./sqls/schedule.sql');

finalization
    SQLRequestList.Free;
end.

