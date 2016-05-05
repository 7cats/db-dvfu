unit ucondition;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, StdCtrls, Buttons, ExtCtrls;

type

    { TFilter }

    TFilterComponent = class
        FFieldCB, FOperationCB : TComboBox;
        FSearchEdit : TEdit;
        FCloseItem : TButton;
        FPanel : TPanel;
        constructor Create(fieldCB, operationCB : TComboBox; searchEdit : TEdit;
            closeItem : TButton; panel : TPanel);
        destructor Destroy(); override;
    end;

    { TPairString }

    TPairString = object
        FTableName, FFieldName : string;
        procedure MakePair(first, second : string);
    end;

    { TVectorPairString }

    TVectorPairString = object
        private
            function GetItem(index : integer) : TPairString;
        public
            FPairs : array of TPairString;
            procedure PushBack(first, second : string);
            property Item[index : integer] : TPairString read GetItem; default;
            function High_() : integer;
    end;

    { TCondition }

    TCondition = object
        FField, FOperation, FParam : string;
        FIndexParam : integer;
        procedure MakeTriplet(Field, Operation, Param : string; index : integer);
    end;

    { TVectorConditions }

    TVectorConditions = object
        private
            function GetItem(index : integer) : TCondition;
        public
            FTriplets : array of TCondition;
            procedure PushBack(Field, Operation, Param : string; index : integer);
            property Item[index : integer] : TCondition read GetItem; default;
            function High_() : integer;
    end;

implementation

{ TCondition }

constructor TFilterComponent.Create(fieldCB, operationCB: TComboBox; searchEdit: TEdit;
    closeItem: TButton; panel : TPanel);
begin
    FFieldCB := fieldCB;
    FOperationCB := operationCB;
    FSearchEdit := searchEdit;
    FCloseItem := closeItem;
    FPanel := panel;
end;


destructor TFilterComponent.Destroy;
begin
    FFieldCB.Hide;
    FFieldCB.Free;
    FOperationCB.Hide;
    FOperationCB.Free;
    FSearchEdit.Hide;
    FSearchEdit.Free;
    FCloseItem.Hide;
    FCloseItem.Free;
    FPanel.Hide;
    FPanel.Free;
    inherited Destroy();
end;

{ TVectorConditions }

procedure TVectorConditions.PushBack(Field, Operation, Param: string;
    index: integer);
var
    tmp : TCondition;
begin
    tmp.MakeTriplet(Field, Operation, Param, index);
    SetLength(FTriplets, Length(FTriplets) + 1);
    FTriplets[Self.High_()] := tmp;
end;


function TVectorConditions.High_: integer;
begin
    result := High(FTriplets);
end;


function TVectorConditions.GetItem(index: integer): TCondition;
begin
    Assert((0 <= index) and (index <= High(FTriplets)));
    result := FTriplets[index];
end;


{ TVectorPairString }

procedure TVectorPairString.PushBack(first, second: string);
var
    tmp : TPairString;
begin
    tmp.MakePair(first, second);
    SetLength(FPairs, Length(FPairs) + 1);
    FPairs[High(FPairs)] := tmp;
end;


function TVectorPairString.High_: integer;
begin
    result := High(FPairs);
end;


function TVectorPairString.GetItem(index: integer): TPairString;
begin
    Assert((0 <= index) and (index <= High(FPairs)));
    result := FPairs[index];
end;



{ TPairString }

procedure TPairString.MakePair(first, second: string);
begin
    FTableName := first;
    FFieldName := second;
end;


{ TCondition }

procedure TCondition.MakeTriplet(Field, Operation, Param: string; index: integer);
begin
    FField := Field;
    FOperation := Operation;
    FParam := Param;
    FIndexParam := index;
end;

end.

