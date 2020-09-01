unit CookieUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs;

type

  { TCookie }

  TCookie = class(HTTPDefs.TCookie)
  private
  public
    constructor Create(_Name, _Value: AnsiString; _Path: AnsiString = '';
      _Domain: AnsiString = ''; _MaxAge: Integer = 46800);
  end;

implementation

{ TCookie }

constructor TCookie.Create(_Name, _Value: AnsiString; _Path: AnsiString;
  _Domain: AnsiString; _MaxAge: Integer);
begin
  inherited Create(nil);

  Name := _Name;
  Value := _Value;
  Path := _Path;
  Domain := _Domain;
  Expires := TimeStampToDateTime(
    MSecsToTimeStamp(
      1000 * (DateTimeToTimeStamp(Now).Time + _MaxAge)));

end;

end.

