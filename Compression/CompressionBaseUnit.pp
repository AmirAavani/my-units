unit CompressionBaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TBaseArchiverStream = class(TObject)
  private
  public
    // Returns the diff in size.
    function Compress(IStream, OStream: TStream): Integer; virtual; abstract;
    // Returns the diff in size.
    function DeCompress(IStream, OStream: TStream): Integer; virtual; abstract;
  end;

implementation


end.

