unit umetadata;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    TNameRelation = record
        FDBName, FCaption : string;
    end;

    { TMetaTable }

    TMetaTable = object
        FTableName, FPrimaryKey : TNameRelation;
        FColumnsNames : array of TNameRelation;
        function CountOfColumns() : integer;
    end;

    { TMetaData }

    TMetaData = class
        private
            function GetItem(index : integer) : TMetaTable;
            function GetItem(index : string) : TMetaTable;
        public
            FTables: array of TMetaTable;
            procedure AddTable(DBName, Caption :String);
            procedure AddTableColumn(DBName, Caption: string);
            procedure AddTableColumn(DBName, Caption: string; IsPrimaryKey : boolean);
            property Items[index : integer] : TMetaTable read GetItem;
            property ItemsS[index : string] : TMetaTable read GetItem; default;
            constructor Create();
    end;

var
    MetaData : TMetaData;

implementation

{ TMetaTable }

function TMetaTable.CountOfColumns: integer;
begin
    result := Length(FColumnsNames);
end;

{ TMetadata }

function TMetaData.GetItem(index: integer): TMetaTable;
begin
    if (index >= Length(FTables)) then begin
        Raise Exception.Create('Ooooops!');
    end
    else begin
        result := FTables[index];
    end;
end;

function TMetaData.GetItem(index: string): TMetaTable;
var
    i : integer;
begin
    for i := 0 to High(FTables) do begin
        with FTables[i].FTableName do begin
            if (FCaption = index) or (FDBName= index) then begin
                exit(FTables[i]);
            end;
        end;
    end;
    Raise Exception.Create('Invailid table');
end;

procedure TMetaData.AddTable(DBName, Caption: String);
begin
    SetLength(FTables, Length(FTables) + 1);
    FTables[High(FTables)].FTableName.FDBName := DBName;
    FTables[High(FTables)].FTableName.FCaption := Caption;
end;

procedure TMetaData.AddTableColumn(DBName, Caption: string);
begin
    with FTables[High(FTables)] do begin
        SetLength(FColumnsNames, Length(FColumnsNames) + 1);
        FColumnsNames[High(FColumnsNames)].FDBName := DBName;
        FColumnsNames[High(FColumnsNames)].FCaption := Caption;
    end;
end;
procedure TMetaData.AddTableColumn(DBName, Caption: string;
    IsPrimaryKey: boolean);
begin
    AddTableColumn(DBName, Caption);
    if (IsPrimaryKey = true) and (FTables[High(FTables)].FPrimaryKey.FCaption <> '') then begin
        Raise Exception.Create('Primary key is already exist');
    end
    else if (IsPrimaryKey = true) then begin
        FTables[High(FTables)].FPrimaryKey.FCaption := Caption;
        FTables[High(FTables)].FPrimaryKey.FDBName := DBName;
    end;
end;

constructor TMetaData.Create;
begin
end;

initialization
    MetaData := TMetaData.Create();

    MetaData.AddTable('CLASSROOMS', 'Аудитории');
        MetaData.AddTableColumn('ID', 'ID', true);
        MetaData.AddTableColumn('NAME', 'Аудитория');

    MetaData.AddTable('GROUPS', 'Группы');
        MetaData.AddTableColumn('ID', 'ID', true);
        MetaData.AddTableColumn('NAME', 'Группа');

    MetaData.AddTable('LESSONS', 'Предметы');
        MetaData.AddTableColumn('ID', 'ID', true);
        MetaData.AddTableColumn('NAME', 'Предмет');

    MetaData.AddTable('TEACHERS', 'Преподаватели');
        MetaData.AddTableColumn('ID', 'ID', true);
        MetaData.AddTableColumn('LAST_NAME', 'Фамилия');
        MetaData.AddTableColumn('FIRST_NAME', 'Имя');
        MetaData.AddTableColumn('MIDDLE_NAME', 'Отчество');

    MetaData.AddTable('LESSONS_TIMES', 'Время занитий');
        MetaData.AddTableColumn('ID', 'Номер', true);
        MetaData.AddTableColumn('BEGIN_', 'Начало');
        MetaData.AddTableColumn('END_', 'Конец');

    MetaData.AddTable('LESSONS_TYPES', 'Типы занятий');
        MetaData.AddTableColumn('ID', 'ID', true);
        MetaData.AddTableColumn('NAME', 'Название');

    MetaData.AddTable('WEEKDAYS', 'Дни недели');
        MetaData.AddTableColumn('ID', 'Номер', true);
        MetaData.AddTableColumn('NAME', 'Название');

end.

