unit usqlrequestlist;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, IBConnection, sqldb, db, FileUtil, Forms, Controls,
    Graphics, Dialogs, DBGrids, Menus;

type

    { TSQLRequest }

    TSQLRequest = class
        private
            FSQLReq : TStringList;
        public
            constructor Create(sqlpath: string);
    end;

    { TSQLRequestList }

    TSQLRequestList = class(TSQLRequest)
        private
            FTables: array of TSQLRequest;
            function GetItem( index : integer) : TStringList;
        public
            procedure NewItem(sqlpath : string);
            property Items[index : integer] : TStringList read GetItem; default;
            Constructor Create();
    end;

var
    SQLRequestList : TSQLRequestList;
implementation

{ TSQLRequestList }

function TSQLRequestList.GetItem(index: integer): TStringList;
begin
    Result := FTables[index].FSQLReq;
end;

procedure TSQLRequestList.NewItem(sqlpath: string);
var
    NewSQLTable : TSQLRequest;
begin
    NewSQLTable := TSQLRequest.Create(sqlpath);
    SetLength(FTables, Length(FTables) + 1);
    FTables[
    High(FTables)] := NewSQLTable;
end;

constructor TSQLRequestList.Create();
begin
end;


constructor TSQLRequest.Create(sqlpath: string);
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

initialization
    SQLRequestList := TSQLRequestList.Create();

    SQLRequestList.NewItem('./sqls/groups.sql');
    SQLRequestList.NewItem('./sqls/lessons.sql');
    SQLRequestList.NewItem('./sqls/teachers.sql');
    SQLRequestList.NewItem('./sqls/timelessons.sql');
    SQLRequestList.NewItem('./sqls/schedule.sql');
end.

