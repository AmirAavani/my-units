unit ParameterManagerUnit_V2;

{$mode ObjFPC}{$H+}

interface

uses
  ValueUnit, Classes, SysUtils;

type

  { TRunTimeParameterManager }
  generic TRunTimeParameterManager<TParamList> = class(TObject)
  protected
    Params: TParamList;


  public
    constructor Create;

  end;

implementation

uses
  TypInfo;

{ TRunTimeParameterManager }

constructor TRunTimeParameterManager.Create;
var
  tInfo: PTypeInfo;
  td: PTypeData;
  FCount: Integer;
  vmt: TVmt;
  //dtm: PDmt;

begin
  vmt := PVmt(TParamList)^;
//  WriteLn('ClassNAme: ', vmt.vClassName);
  //dtm := PDmt(vmt.vDynamicTable)^;
//  //WriteLn('ClassNAme: ', vmt.vDynamicTable);

end;

end.

