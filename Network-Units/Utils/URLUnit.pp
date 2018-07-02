unit URLUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GetDomain(const URL: AnsiString): AnsiString;
function GetProtocol(const URL: AnsiString): AnsiString;
function EncodeUrl(const URL: AnsiString): AnsiString;
function DecodeURL(const s: AnsiString): AnsiString;

implementation

function GetDomain(const URL: AnsiString): AnsiString;
var
  i: Integer;

begin
  Result := '';
  for i := Length(GetProtocol(URL)) + 4 to Length(URL) do
  begin
    if URL[i] = '/' then
      break;
    Result += URL[i];
  end;
end;

function GetProtocol(const URL: AnsiString): AnsiString;
var
  i: Integer;

begin
  Result := '';

  for i := 1 to Length(URL) do
    if URL[i] = ':' then
    begin
      Result := Copy(URL, 1, i - 1);
      break;
    end;

end;

function DecodeURL(const s: AnsiString): AnsiString;
var
   sAnsi: String;
   sUtf8: String;
   sWide: WideString;

   i, len: Cardinal;
   ESC: string[2];
   CharCode: integer;
   c: char;
begin
   sAnsi := PChar(s);
   SetLength(sUtf8, Length(sAnsi));
   i := 1;
   len := 1;

   while i <= Length(sAnsi) do
  begin
      if sAnsi[i] <> '%' then
      begin
         if (sAnsi[i] = '+') then
            c := ' '
         else
            c := sAnsi[i];
         sUtf8[len] := c;
         Inc(len);
      end
      else
      begin
         Inc(i);
         ESC := Copy(sAnsi, i, 2);
         Inc(i, 1);
         try
            CharCode := StrToInt('$' + ESC);
            c := Char(CharCode);
            sUtf8[len] := c;
            Inc(len);
         except end;
      end;
      Inc(i);
   end;
   Dec(len);
   SetLength(sUtf8, len);

   sWide := UTF8Decode(sUtf8);
   len := Length(sWide);

   Result := sWide;
end;

function EncodeUrl(const URL: AnsiString): AnsiString;
Const
  NotAllowed = [';', ':', '@', '&', '#', '+', '_', '<', '>',
   '"', '%', '{', '}', '|', '\', '^', '~', '[', ']', '`' ];
var
  i, o, l : Integer;
  h : string[2];
  P : PChar;
  c : AnsiChar;
begin
  Result :='';
  l := Length(URL);
  if l=0 then Exit;
  SetLength(Result, l * 3);
  P:= Pchar(Result);
// https://
  for i := 1 to L do
  begin
    c:= URL[i];
    O := Ord(c);

    if (Length('https://') <= i) and ((o <= $20) or ($7F <= O) or (c in NotAllowed)) then
    begin
      P^ := '%';
      Inc(P);
      h := IntToHex(Ord(c), 2);
      p^ := h[1];
      Inc(P);
      p^ := h[2];
      Inc(P);
    end
    else
    begin
      P^ := c;
      Inc(p);
    end;
  end;
  SetLength(Result,P - PChar(Result));
end;

end.

