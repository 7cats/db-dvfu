unit umetadata;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    TNameRelation = record
        FDBName, FCaption : string;
    end;

    TMetaTable = record
        FTableName, FPrimaryKey : TNameRelation;
        FColumnsNames : array of TNameRelation;
    end;

    { TMetaData }

    TMetaData = class
        private
            function GetItem(i : integer) : TMetaTable;
        public
            FTables: array of TMetaTable;
            procedure AddTable(DBName, Caption :String);
            procedure AddTableColumn(DBName, Caption: string);
            procedure AddTableColumn(DBName, Caption: string; IsPrimaryKey : boolean);
            property Items[index : integer] : TMetaTable read GetItem; default;
            constructor Create();
    end;

var
    MetaData : TMetaData;

implementation

{ TMetadata }

function TMetaData.GetItem(i: integer): TMetaTable;
begin
    if (i >= Length(FTables)) then begin
        Raise Exception.Create('Ooooops!');
    end
    else begin
        result := FTables[i];
    end;
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

