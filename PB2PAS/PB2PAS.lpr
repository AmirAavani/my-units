program PB2PAS;
uses
  ParameterManagerUnit, PBParserUnit, WideStringUnit;


begin
  TProto.GenerateCode(TRunTimeParameterManager.GetInstance.ValueByName['--InputFile']);
end.

