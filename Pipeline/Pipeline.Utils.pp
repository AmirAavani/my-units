unit Pipeline.Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamUnit, generics.Collections;

type

  { TPipelineList }

  generic TPipelineList<T> = class(specialize TList<T>)
  protected type
    TPipelineListT = specialize TPipelineList<T>;

  protected

    procedure SaveAnElement(const AnElement: T; OutputStream: TMyBinStream); virtual; abstract;
    function LoadAnElement(InputStream: TMyBinStream): T; virtual; abstract;

    function SaveToStream(OutputStream: TMyBinStream): Boolean; virtual;
    function LoadFromStream(InputStream: TMyBinStream): Boolean; virtual;

  public
    function SaveToFile(OutputFileName: AnsiString): Boolean; virtual;
    constructor LoadFromFile(InputFileName: AnsiString); virtual;
    constructor Create; virtual;

    procedure ComputeModule(Modulo, Remainder: Integer; Result: TPipelineListT);
    procedure ComputeDivide(Modulo, Quotient: Integer; Result: TPipelineListT);

  end;

  { TInt64List }

  TInt64List = class(specialize TPipelineList<Int64>)
  protected
    procedure SaveAnElement(const AnElement: Int64; OutputStream: TMyBinStream); override;
    function LoadAnElement(InputStream: TMyBinStream): Int64; override;


  end;

  { TUInt64List }

  TUInt64List = class(specialize TPipelineList<UInt64>)
  protected
    procedure SaveAnElement(const AnElement: UInt64; OutputStream: TMyBinStream); override;
    function LoadAnElement(InputStream: TMyBinStream): UInt64; override;

  end;

  { TInt32List }

  TInt32List = class(specialize TPipelineList<Int32>)
  protected
    procedure SaveAnElement(const AnElement: Int32; OutputStream: TMyBinStream); override;
    function LoadAnElement(InputStream: TMyBinStream): Int32; override;

  end;

  { TUInt32List }

  TUInt32List = class(specialize TPipelineList<UInt32>)
  protected
    procedure SaveAnElement(const AnElement: UInt32; OutputStream: TMyBinStream); override;
    function LoadAnElement(InputStream: TMyBinStream): UInt32; override;

  end;

implementation

{ TUInt32List }

procedure TUInt32List.SaveAnElement(const AnElement: UInt32;
  OutputStream: TMyBinStream);
begin
  OutputStream.WriteUInt32(AnElement);

end;

function TUInt32List.LoadAnElement(InputStream: TMyBinStream): UInt32;
begin
  Result := InputStream.ReadUInt32;

end;

{ TInt32List }

procedure TInt32List.SaveAnElement(const AnElement: Int32;
  OutputStream: TMyBinStream);
begin
  OutputStream.WriteInt32(AnElement);

end;

function TInt32List.LoadAnElement(InputStream: TMyBinStream): Int32;
begin
  Result := InputStream.ReadInt32;

end;

{ TUInt64List }

procedure TUInt64List.SaveAnElement(const AnElement: UInt64;
  OutputStream: TMyBinStream);
begin
  OutputStream.WriteUInt64(AnElement);

end;

function TUInt64List.LoadAnElement(InputStream: TMyBinStream): UInt64;
begin
  Result := InputStream.ReadUInt64;

end;

{ TInt64List }

procedure TInt64List.SaveAnElement(const AnElement: Int64;
  OutputStream: TMyBinStream);
begin
  OutputStream.WriteInt64(AnElement);

end;

function TInt64List.LoadAnElement(InputStream: TMyBinStream): Int64;
begin
  Result := InputStream.ReadInt64;

end;

{ PipelineList }

function TPipelineList.SaveToStream(OutputStream: TMyBinStream): Boolean;
var
  Element: T;

begin
  OutputStream.WriteInt(Self.Count);

  for Element in Self do
    SaveAnElement(Element, OutputStream);

  Result := True;
end;

function TPipelineList.LoadFromStream(InputStream: TMyBinStream): Boolean;
var
  i: Integer;
  ElementCount: Integer;
  Element: T;

begin
  ElementCount := InputStream.ReadInt32;
  Self.Capacity := ElementCount;

  for i := 0 to ElementCount - 1 do
  begin
    Element := LoadAnElement(InputStream);
    Self.Add(Element);

  end;
  Result := True;

end;

function TPipelineList.SaveToFile(OutputFileName: AnsiString): Boolean;
var
  Stream: TMyBinStream;

begin
  Stream := TMyBinStream.Create(TFileStream.Create(OutputFileName, fmCreate), true);

  Result := SaveToStream(Stream);

  Stream.Free;
end;

constructor TPipelineList.LoadFromFile(InputFileName: AnsiString);
var
  Stream: TMyBinStream;

begin
  inherited Create;

  Stream := TMyBinStream.Create(TFileStream.Create(InputFileName, fmOpenRead), true);

  LoadFromStream(Stream);

  Stream.Free;
end;

constructor TPipelineList.Create;
begin
  inherited Create;

end;

procedure TPipelineList.ComputeModule(Modulo, Remainder: Integer;
  Result: TPipelineListT);
var
  i: Integer;

begin
  Result.Clear;

  i := Remainder mod Modulo;
  while i < Self.Count do
  begin
    Result.Add(Self[i]);
    Inc(i, Modulo);

  end;
end;

procedure TPipelineList.ComputeDivide(Modulo, Quotient: Integer;
  Result: TPipelineListT);
var
  i: Integer;

begin
  Result.Clear;

  i := Modulo * Quotient;
  while (i < Self.Count) and (i < Modulo * (Quotient + 1)) do
  begin
    Result.Add(Self[i]);
    Inc(i);

  end;
end;

end.
