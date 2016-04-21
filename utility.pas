unit utility;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, StdCtrls, Buttons;

type

    { TFilter }

    TFilter = class
        FFieldCB, FOperationCB : TComboBox;
        FSearchEdit : TEdit;
        FCloseItem : TButton;
        constructor Create(fieldCB, operationCB : TComboBox; searchEdit : TEdit; closeItem : TButton);
        destructor Destroy(); override;
    end;

    { TPairString }

    TPairString = object
        FTableName, FFieldName : ^string;
        procedure MakePair(var first, second : string);
    end;

    { TVectorPairString }

    TVectorPairString = object
        private
            function GetItem(index : integer) : TPairString;
        public
            FPairs : array of TPairString;
            procedure PushBack(var first, second : string);
            property Item[index : integer] : TPairString read GetItem; default;
            function High_() : integer;
    end;

    { TTriplet }

    TTriplet = object
        FField, FOperation, FParam : string;
        FIndexParam : integer;
        procedure MakeTriplet(Field, Operation, Param : string; index : integer);
    end;

    { TVectorTriplet }

    TVectorTriplet = object
        private
            function GetItem(index : integer) : TTriplet;
        public
            FTriplets : array of TTriplet;
            procedure PushBack(Field, Operation, Param : string; index : integer);
            property Item[index : integer] : TTriplet read GetItem; default;
            function High_() : integer;
    end;

implementation

{ TFilter }

constructor TFilter.Create(fieldCB, operationCB: TComboBox; searchEdit: TEdit;
    closeItem: TButton);
begin
    FFieldCB := fieldCB;
    FOperationCB := operationCB;
    FSearchEdit := searchEdit;
    FCloseItem := closeItem;
end;

{*****************************************************************************}

destructor TFilter.Destroy;
begin
    FFieldCB.Hide;
    FFieldCB.Free;
    FOperationCB.Hide;
    FOperationCB.Free;
    FSearchEdit.Hide;
    FSearchEdit.Free;
    FCloseItem.Hide;
    FCloseItem.Free;
    inherited Destroy();
end;

{ TVectorTriplet }

procedure TVectorTriplet.PushBack(Field, Operation, Param: string;
    index: integer);
var
    tmp : TTriplet;
begin
    tmp.MakeTriplet(Field, Operation, Param, index);
    SetLength(FTriplets, Length(FTriplets) + 1);
    FTriplets[Self.High_()] := tmp;
end;

{*****************************************************************************}

function TVectorTriplet.High_: integer;
begin
    result := High(FTriplets);
end;

{*****************************************************************************}

function TVectorTriplet.GetItem(index: integer): TTriplet;
begin
    Assert((0 <= index) and (index <= High(FTriplets)));
    result := FTriplets[index];
end;

{*****************************************************************************}


{ TVectorPairString }

procedure TVectorPairString.PushBack(var first, second: string);
var
    tmp : TPairString;
begin
    tmp.MakePair(first, second);
    SetLength(FPairs, Length(FPairs) + 1);
    FPairs[High(FPairs)] := tmp;
end;

{*****************************************************************************}

function TVectorPairString.High_: integer;
begin
    result := High(FPairs);
end;

{*****************************************************************************}

function TVectorPairString.GetItem(index: integer): TPairString;
begin
    Assert((0 <= index) and (index <= High(FPairs)));
    result := FPairs[index];
end;

{*****************************************************************************}


{ TPairString }

procedure TPairString.MakePair(var first, second: string);
begin
    FTableName := @first;
    FFieldName := @second;
end;

{*****************************************************************************}


{ TTriplet }

procedure TTriplet.MakeTriplet(Field, Operation, Param: string; index: integer);
begin
    FField := Field;
    FOperation := Operation;
    FParam := Param;
    FIndexParam := index;
end;



end.

