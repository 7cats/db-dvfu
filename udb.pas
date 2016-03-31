unit udb;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, IBConnection, sqldb, db;

type
    TDataBase = class
        IBConnection: TIBConnection;
        SQLTransaction: TSQLTransaction;
        procedure Connect;
    end;

var
    DataBase:TDataBase;

implementation

procedure TDataBase.Connect;
begin
    try
        IBConnection.Password := 'masterkey';
        IBConnection.CharSet:='UTF8';
        IBConnection.UserName := 'SYSDBA';
        IBConnection.DatabaseName := '../MY.FDB';
        IBConnection.Transaction := DataBase.SQLTransaction;
        SQLTransaction.DataBase := IBConnection;
    except
        Raise Exception.Create('Problem with connect to database');
    end;
end;

initialization
    DataBase := TDataBase.Create();
    DataBase.IBConnection := TIBConnection.Create(nil);
    DataBase.SQLTransaction := TSQLTransaction.Create(nil);

finalization
    DataBase.IBConnection.Free;
    DataBase.SQLTransaction.Free;
    DataBase.Free;
end.


