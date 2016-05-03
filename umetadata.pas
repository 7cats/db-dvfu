unit umetadata;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    TTableColumn = record
        FDBName, FCaption : string;
        FRefTableName, FRefFieldName : string;
        FForeignKey : string;
        FForeignFields : array of string;
    end;

    { TMetaTable }

    TMetaTable = class
    private
        function GetItem(const index : string): TTableColumn;
        function GetItem(const index : integer): TTableColumn;
    public
        FDBName, FCaption, FPrimaryCaption : string;
        FColumns : array of TTableColumn;
        function CountOfColumns() : integer;
        property Items[const index : string] : TTableColumn read GetItem; default;
    end;

    { TMetaData }

    TMetaData = class
        private
            function GetItem(const index : integer) : TMetaTable;
            function GetItem(const index : string) : TMetaTable;
        public
            FTables: array of TMetaTable;
            function GetTableIndex(caption : string) : integer;
            procedure AddTable(const DBName, Caption :String);
            procedure AddTableColumn(const DBName, Caption: string);
            procedure AddTableColumn(const DBName, Caption, RefTable, RefField: string;
                    const outRTField: array of string; const ForeingKey : string);
            property Items[const index : string] : TMetaTable read GetItem; default;
            function CountOfTables() : integer;
            constructor Create();
            destructor Destroy; override;
    end;

var
    MetaData : TMetaData;

implementation

{ TMetaTable }

function TMetaTable.GetItem(const index: string): TTableColumn;
var
    i : integer;
begin
    try
        exit(GetItem(StrToInt(index)));
    except
        for i := 0 to High(FColumns) do begin
            if (FColumns[i].FCaption = index) or (FColumns[i].FDBName = index) then begin
                exit(FColumns[i]);
            end;
        end;
        Raise Exception.Create('Invailid column');
    end;
end;


function TMetaTable.GetItem(const index: integer): TTableColumn;
begin
    Assert((0 <= index) and (index <= High(FColumns)));
    result := FColumns[index];
end;


function TMetaTable.CountOfColumns: integer;
begin
    result := Length(FColumns);
end;

{ TMetadata }

function TMetaData.GetItem(const index: integer): TMetaTable;
begin
    Assert((0 <= index) and (index <= High(FTables)));
    result := FTables[index];
end;


function TMetaData.GetItem(const index: string): TMetaTable;
var
    i : integer;
begin
    for i := 0 to High(FTables) do begin
        with FTables[i] do begin
            if (FCaption = index) or (FDBName = index) then begin
                exit(FTables[i]);
            end;
        end;
    end;
    Raise Exception.Create('Invailid table');
end;


function TMetaData.GetTableIndex(caption: string): integer;
var
    i : integer;
begin
    for i := 0 to High(FTables) do begin
        if (FTables[i].FCaption = caption) then begin
            exit(i);
        end;
    end;
    Raise Exception.Create('Invalid column');
end;


procedure TMetaData.AddTable(const DBName, Caption: String);
begin
    SetLength(FTables, Length(FTables) + 1);
    FTables[High(FTables)] := TMetaTable.Create();
    FTables[High(FTables)].FDBName := DBName;
    FTables[High(FTables)].FCaption := Caption;
end;


procedure TMetaData.AddTableColumn(const DBName, Caption: string);
begin
    with FTables[High(FTables)] do begin
        SetLength(FColumns, Length(FColumns) + 1);
        FColumns[High(FColumns)].FDBName := DBName;
        FColumns[High(FColumns)].FCaption := Caption;
        FColumns[High(FColumns)].FRefFieldName := '';
        FColumns[High(FColumns)].FRefTableName := '';
    end;
end;


procedure TMetaData.AddTableColumn(const DBName, Caption, RefTable,
    RefField: string; const outRTField: array of string;
    const ForeingKey: string);
var
    i, j : integer;
begin
    AddTableColumn(DBName, Caption);
    with FTables[High(FTables)].FColumns[High(FTables[High(FTables)].FColumns)] do begin
        FRefTableName := RefTable;
        FRefFieldName := RefField;
        SetLength(FForeignFields, Length(outRTField));
        for i := 0 to High(outRTField) do begin
            FForeignFields[i] := outRTField[i];
            FForeignKey := ForeingKey;
        end;
    end;
end;


function TMetaData.CountOfTables: integer;
begin
    result := High(FTables);
end;


constructor TMetaData.Create;
begin
end;

destructor TMetaData.Destroy;
var
    i : integer;
begin
    for i := 0 to High(FTables) do begin
        FTables[i].Free;
	end;
	inherited Destroy;
end;

initialization
    MetaData := TMetaData.Create();

    MetaData.AddTable('CLASSROOMS', 'Аудитории');
        MetaData.AddTableColumn('ID', 'ID');
        MetaData.AddTableColumn('NAME', 'Аудитория');

    MetaData.AddTable('GROUPS', 'Группы');
        MetaData.AddTableColumn('ID', 'ID');
        MetaData.AddTableColumn('NAME', 'Группа');

    MetaData.AddTable('LESSONS', 'Предметы');
        MetaData.AddTableColumn('ID', 'ID');
        MetaData.AddTableColumn('NAME', 'Предмет');

    MetaData.AddTable('TEACHERS', 'Преподаватели');
        MetaData.AddTableColumn('ID', 'ID');
        MetaData.AddTableColumn('LAST_NAME', 'Фамилия');
        MetaData.AddTableColumn('FIRST_NAME', 'Имя');
        MetaData.AddTableColumn('MIDDLE_NAME', 'Отчество');

    MetaData.AddTable('LESSONS_TIMES', 'Время занятий');
        MetaData.AddTableColumn('ID', 'Номер');
        MetaData.AddTableColumn('BEGIN_', 'Начало');
        MetaData.AddTableColumn('END_', 'Конец');

    MetaData.AddTable('LESSONS_TYPES', 'Типы занятий');
        MetaData.AddTableColumn('ID', 'ID');
        MetaData.AddTableColumn('NAME', 'Название');

    MetaData.AddTable('WEEKDAYS', 'Дни недели');
        MetaData.AddTableColumn('ID', 'Номер');
        MetaData.AddTableColumn('NAME', 'Название');

    Metadata.AddTable('TIMETABLE', 'Расписание');
        Metadata.AddTableColumn('ID', 'ID');
        Metadata.AddTableColumn('WEEKDAY_ID', 'День Недели', 'WEEKDAYS', 'ID', ['Название'],  'ID');
        Metadata.AddTableColumn('LESSON_TIME_ID', 'Время', 'LESSONS_TIMES', 'ID', ['Начало', 'Конец'], 'ID');
        Metadata.AddTableColumn('LESSON_ID', 'Название', 'LESSONS', 'ID', ['Предмет'], 'ID');
        Metadata.AddTableColumn('LESSON_TYPE_ID', 'Тип', 'LESSONS_TYPES', 'ID', ['Название'], 'ID');
        Metadata.AddTableColumn('TEACHER_ID', 'Преподаватель', 'TEACHERS', 'ID', ['Фамилия', 'Имя', 'Отчество'], 'ID');
        Metadata.AddTableColumn('GROUP_ID', 'Группа', 'GROUPS', 'ID', ['Группа'], 'ID');
        Metadata.AddTableColumn('CLASSROOM_ID', 'Аудитория', 'CLASSROOMS', 'ID', ['Аудитория'], 'ID');

finalization
    MetaData.Free;
 end.

