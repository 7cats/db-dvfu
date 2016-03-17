unit utable;

{$mode objfpc}{$H+}

interface

uses
     Classes, SysUtils, menus, sqldb, db, DBGrids;

type
    TOnClickProcOfObject = procedure (Sender: TObject) of object;

    { TSQLTable }

    TSQLTable = class
        private
            FItemName: string;
            FSQLReq : TStringList;
        public
            procedure ShowTable(SQLquery : TSQLQuery);
            constructor Create(sqlpath: string; ItemCap: string; ItemTag: integer;
                        ParentItem: TMenuItem; OnClick : TOnClickProcOfObject);
    end;

    { TSQLTableList }

    TSQLTableList = class(TSQLTable)
        FSQLQuery : TSQLQuery;
        private
            FTables: array of TSQLTable;
            function GetItem( index : integer) : TSQLTable;
        public
            procedure NewItem(sqlpath : string; ItemCap: string; ItemTag: integer; ParentItem : TMenuItem; OnClick : TOnClickProcOfObject);
            property Items[index : integer] : TSQLTable read GetItem; default;
            Constructor Create();
    end;

var
    SQLTableList : TSQLTableList;
implementation

{ TSQLTableList }

function TSQLTableList.GetItem(index: integer): TSQLTable;
begin
    Result := FTables[index];
end;

procedure TSQLTableList.NewItem(sqlpath: string; ItemCap: string;
  ItemTag: integer; ParentItem: TMenuItem; OnClick: TOnClickProcOfObject);
var
    NewSQLTable : TSQLTable;
begin
    NewSQLTable := TSQLTable.Create(sqlpath,ItemCap, ItemTag, ParentItem, OnClick);
    SetLength(FTables, Length(FTables) + 1);
    FTables[High(FTables)] := NewSQLTable;
end;

constructor TSQLTableList.Create();
begin
end;

{ TSQLTable }

procedure TSQLTable.ShowTable(SQLquery: TSQLQuery);
begin
    SQLQuery.Close;
    SQLQuery.SQL := FSQLReq;
    SQLQuery.Open;
end;

constructor TSQLTable.Create(sqlpath: string; ItemCap: string;
  ItemTag: integer; ParentItem: TMenuItem; OnClick: TOnClickProcOfObject);
var
    s : string;
    NewItem : TMenuItem;
begin
    FSQLReq := TStringList.Create();
    try
        assignFile(input, sqlpath);
        reset(input);
        while (not EOF) do begin
            readln(s);
            FSQLReq.Add(s);
        end;
        NewItem := TMenuItem.Create(ParentItem);
        NewItem.Tag:= ItemTag;
        NewItem.Caption := ItemCap;
        NewItem.OnClick:= OnClick;
        ParentItem.Add(NewItem);
        Close(input);
     Except
         Raise Exception.Create('Error :(');
     end;
end;

end.

