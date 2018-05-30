program PB2PAS;
uses
  ParameterManagerUnit, PBParserUnit;


begin
  TProto.GenerateCode(TRunTimeParameterManager.GetInstance.ValueByName['--InputFile']);
end.

