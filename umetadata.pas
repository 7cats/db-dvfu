unit umetadata;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    TMetaTable = class;

    { TMetaColumn }

    TMetaColumn = class
        private
            FDBName, FCaption : string;
            FIsReference, FShowInReference : boolean;
            FReferenceTable : TMetaTable;
        public
            property DBName : string read FDBName;
            property Caption : string read FCaption;
            property IsReference : boolean read FIsReference;
            property ShowInReference : boolean read FShowInReference;
            property ReferenceTable : TMetaTable read FReferenceTable;
            constructor Create(ADBName, ACaption : string; AShow : boolean);
            constructor Create(ADBName : string; ATable : TMetaTable);
    end;


    { TMetaTable }

    TMetaTable = class
    private
        FDBName, FCaption: string;
        FColumns : array of TMetaColumn;
        function GetItem(const index : string): TMetaColumn;
        function GetItem(const index : integer): TMetaColumn;
    public
        function FieldListStr(AFormat : string): string;
        function FieldListStrNoID(AFormat, ASeparator : string) : string;
        function CountOfColumns() : integer;
        function AddColumn(ADBName, ACaption : string; AShow : boolean) : TMetaTable;
        function AddColumn(ADBName : string; ATable : TMetaTable) : TMetaTable;
        property DBName : string read FDBName;
        property Caption : string read FCaption;
        property Items[const index : string] : TMetaColumn read GetItem; default;
        constructor Create(ADBName, ACaption : string);
        destructor Destroy; override;
    end;

    { TMetaData }

    TMetaData = class
    private
        FTables: array of TMetaTable;
        function GetItem(const index : integer) : TMetaTable;
        function GetItem(const indexI, indexJ : integer) : TMetaColumn;
    public
        function GetTableIndex(caption : string) : integer;
        function AddTable(table : TMetaTable) : TMetaData;
        function CountOfTables() : integer;
        property Items[const index : integer] : TMetaTable read GetItem; default;
        property ItemsIJ[const indexI, indexJ : integer] : TMetaColumn read GetItem;
        destructor Destroy; override;
    end;

var
    MetaData : TMetaData;

implementation

var
    classrooms, groups, lessons, teachers, lassons_times, lessons_types, weekdays, timetable : TMetaTable;


{ TMetaColumn }

constructor TMetaColumn.Create(ADBName, ACaption : string; AShow : boolean);
begin
    FDBName := ADBName;
    FCaption := ACaption;
    FShowInReference := AShow;
    FIsReference := false;
end;


constructor TMetaColumn.Create(ADBName : string; ATable : TMetaTable);
begin
    FDBName := ADBName;
    FCaption := '';
    FShowInReference := true;
    FIsReference := true;
    FReferenceTable := ATable;
end;


{ TMetaTable }

function TMetaTable.GetItem(const index: string): TMetaColumn;
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


function TMetaTable.GetItem(const index: integer): TMetaColumn;
begin
    Assert((0 <= index) and (index <= High(FColumns)));
    result := FColumns[index];
end;

function TMetaTable.FieldListStr(AFormat : string) : string;
var
    i: integer;
begin
    Result := '';
    for i := 0 to High(FColumns) do begin
        if (i > 0) then begin
            Result += ',';
        end;
        Result += Format(AFormat, [FColumns[i].FDBName]);
    end;
end;

function TMetaTable.FieldListStrNoID(AFormat, ASeparator : string) : string;
var
    i : integer;
begin
    Result := '';
    for i := 1 to High(FColumns) do begin
        if (i > 1) then begin
            Result += ASeparator;
        end;
        Result += Format(AFormat, [FColumns[i].FDBName]);
    end;
end;


function TMetaTable.CountOfColumns: integer;
begin
    result := Length(FColumns);
end;


function TMetaTable.AddColumn(ADBName, ACaption: string; AShow: boolean
		): TMetaTable;
begin
    SetLength(FColumns, Length(FColumns) + 1);
    FColumns[High(FColumns)] := TMetaColumn.Create(ADBName, ACaption, AShow);
    result := Self;
end;


function TMetaTable.AddColumn(ADBName : string; ATable : TMetaTable
    ) : TMetaTable;
begin
    SetLength(FColumns, Length(FColumns) + 1);
    FColumns[High(FColumns)] := TMetaColumn.Create(ADBName, ATable);
    result := Self;
end;


constructor TMetaTable.Create(ADBName, ACaption : string);
begin
    FDBName := ADBName;
    FCaption := ACaption;
end;


destructor TMetaTable.Destroy;
var
    i : integer;
begin
    for i := 0 to High(FColumns) do begin
        FColumns[i].Free;
    end;
    inherited Destroy;
end;


{ TMetadata }

function TMetaData.GetItem(const index: integer): TMetaTable;
begin
    Assert((0 <= index) and (index <= High(FTables)));
    result := FTables[index];
end;


function TMetaData.GetItem(const indexI, indexJ: integer): TMetaColumn;
begin
    Assert((0 <= indexI) and (indexI <= MetaData.CountOfTables()) and
           (0 <= indexJ) and (indexJ <= MetaData[indexI].CountOfColumns() - 1));
    result := MetaData.FTables[indexI].FColumns[indexJ];
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

function TMetaData.AddTable(table : TMetaTable) : TMetaData;
begin
    SetLength(FTables, Length(FTables) + 1);
    FTables[High(FTables)] := table;
end;


function TMetaData.CountOfTables: integer;
begin
    result := High(FTables);
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

    classrooms := TMetaTable.Create('CLASSROOMS', 'Аудитории')
        .AddColumn('ID', 'ID', false)
        .AddColumn('NAME', 'Аудитория', true);

    groups := TMetaTable.Create('GROUPS', 'Группы')
        .AddColumn('ID', 'ID', false)
        .AddColumn('NAME', 'Группа', true);

    lessons := TMetaTable.Create('LESSONS', 'Предметы')
        .AddColumn('ID', 'ID', false)
        .AddColumn('NAME', 'Предмет', true);

    teachers := TMetaTable.Create('TEACHERS', 'Преподаватели')
        .AddColumn('ID', 'ID', false)
        .AddColumn('LAST_NAME', 'Фамилия', true)
        .AddColumn('FIRST_NAME', 'Имя', true)
        .AddColumn('MIDDLE_NAME', 'Отчество', true);

    lassons_times := TMetaTable.Create('LESSONS_TIMES', 'Время занятий')
        .AddColumn('ID', 'ID', false)
        .AddColumn('BEGIN_', 'Начало', true)
        .AddColumn('END_', 'Конец', true);

    lessons_types := TMetaTable.Create('LESSONS_TYPES', 'Типы занятий')
        .AddColumn('ID', 'ID', false)
        .AddColumn('NAME', 'Название', true);

    weekdays := TMetaTable.Create('WEEKDAYS', 'Дни недели')
        .AddColumn('ID', 'ID', false)
        .AddColumn('NAME', 'Название', true);

    timetable := TMetaTable.Create('TIMETABLE', 'Расписание')
        .AddColumn('ID', 'ID', false)
        .AddColumn('WEEKDAY_ID', weekdays)
        .AddColumn('LESSON_TIME_ID', lassons_times)
        .AddColumn('LESSON_ID', lessons)
        .AddColumn('LESSON_TYPE_ID', lessons_types)
        .AddColumn('TEACHER_ID',  teachers)
        .AddColumn('GROUP_ID', groups)
        .AddColumn('CLASSROOM_ID', classrooms);

    MetaData := TMetaData.Create();
    MetaData.AddTable(classrooms);
    MetaData.AddTable(groups);
    MetaData.AddTable(lessons);
    MetaData.AddTable(teachers);
    MetaData.AddTable(lassons_times);
    MetaData.AddTable(lessons_types);
    MetaData.AddTable(weekdays);
    MetaData.AddTable(timetable);

finalization
    MetaData.Free;
 end.

