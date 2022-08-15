unit MeiStream;

interface

uses SysUtils, Classes;

type
  EBufferStream = class(Exception);

  TMeiStream = class(TMemoryStream)
  protected
    FCheckRange: Boolean;
    FBufferSize: Integer;
    function GetEof: Boolean; virtual;

//    function ReadWideString(ASize: Longint): string; overload;
    function GetMemory(Index: Int64): PByte;
  public
    procedure SetSize(NewSize: Longint); override;
    function Read(var Buffer; Count: Longint): Longint; override;
    procedure Delete(index, count: Int64);

    procedure Pop(Count: Longint);
    function SeekAddr(APos: Longint): Pointer;

    procedure WriteBoolean(AValue: Boolean);
    procedure WriteByte(AValue: Byte);
    procedure WriteWord(AValue: Word);
    procedure WriteLong(AValue: Longword);
    procedure WriteInteger(AValue: Integer);
    procedure WriteInt64(AValue: Int64);
    procedure WriteSingle(AValue: Single);
    procedure WriteDouble(AValue: Double);
    procedure WriteNativeUInt(AValue: NativeUInt);
    procedure writeCnt(p0: Int64; cnt: Integer);
//    procedure WriteWideString(AValue: string; encoding: TWideFormat); overload;
//    procedure WriteWideString(AValue: string; encoding: string); overload;
//    procedure WriteWideStringWithLength(AValue: string; encoding: TWideFormat; SizeSize: Longint = 1); overload;
//    procedure WriteWideStringWithLength(AValue: string; encoding: string; SizeSize: Longint = 1); overload;
//    procedure WriteWideStringWithSize(AValue: string; encoding: TWideFormat; SizeSize: Longint = 1); overload;
//    procedure WriteWideStringWithSize(AValue: string; encoding: string; SizeSize: Longint = 1); overload;

    procedure WriteWideString(AValue: string);
    procedure WriteWideStringWithSize(AValue: string; SizeSize: Integer = 1);
    procedure WriteStringWithSize(AValue: string; SizeSize: Integer = 1);
    procedure WritePWideChar(AValue: PWideChar); overload;
    procedure WritePWideChar(AValue: string); overload;
    procedure WritePAnsiChar(AValue: PAnsiChar); overload;
    procedure WritePAnsiChar(AValue: string); overload;
    {$IFDEF swserver}
    procedure WriteAnsiString(AValue: AnsiString);
    procedure WriteAnsiStringWithSize(AValue: AnsiString; SizeSize: Longint = 1);
    {$ELSE}
    procedure WriteAnsiString(AValue: string);
    procedure WriteAnsiStringWithSize(AValue: string; SizeSize: Integer = 1);
    {$ENDIF}
//    procedure WriteWideStringWithSize(AValue: AnsiString; SizeSize: Longint = 1);
    procedure WriteStream(AValue: TStream; ReadPosition: Int64 = -1;
        ReadSize: int64 = -1);

    function ReadByte: Byte;
    function ReadBoolean: Boolean;
    function ReadWord: Word;
    function ReadLong: Longword;
    function ReadInteger: Integer;
    function ReadInt64: Int64;
    function ReadDouble: Double;
    function ReadNativeUInt: NativeUInt;
//    function ReadWideString(ASize: Longint; encoding: TWideFormat): string; overload;
//    function ReadWideString(ASize: Longint; encoding: string): string; overload;
//    function ReadWideStringWithLength(encoding: TWideFormat; SizeSize: Longint = 1): string; overload;
//    function ReadWideStringWithLength(encoding: string; SizeSize: Longint = 1): string; overload;
//    function ReadWideStringWithSize(encoding: TWideFormat; SizeSize: Longint = 1): string; overload;
//    function ReadWideStringWithSize(encoding: string; SizeSize: Longint = 1): string; overload;
    function ReadAnsiString(ASize: Integer): string;
    function ReadAnsiStringWithSize(SizeSize: Integer = 1): string;
    function ReadWideString(ASize: Integer): string;
    function ReadWideStringWithSize(SizeSize: Integer = 1): String;
    function ReadStringWithSize(SizeSize: Integer = 1): String;
    function ReadPAnsiChar: string;
    function ReadPWideChar: String;
    function ReadStream(AValue: TStream; ASize: Longint = -1): Longint;
    function ReadHexText(isize: Integer): string;

    function CurrentMem: Pointer;
    function CurrentSize: Int64;
    procedure SetMemory(buf: Pointer; size: Int64);

    procedure loadFromString(s: AnsiString);

    property EOF: Boolean read GetEof;
    property CheckRange: Boolean read FCheckRange write FCheckRange;
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property Memories[Index: Int64]: PByte read GetMemory; default;
  end;

  TPacketStream = class(TMeiStream)
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

var
  GameEncoding: TEncoding;

implementation

{ TMeiStream }

function TMeiStream.CurrentMem: Pointer;
begin
    Result := GetMemory(Position);
end;

function TMeiStream.CurrentSize: Int64;
begin
    Result := Size - Position;
end;

procedure TMeiStream.Delete(index, count: Int64);
begin
  if (index >= size) then
    Exit;
  if count + index >= Size then;
    count := Size - index;

    Move(GetMemory(index + count)^, GetMemory(index)^, Size - (index + count));
    SetSize(Size - count);
end;

function TMeiStream.GetEof: Boolean;
begin
  Result := Position >= Size;
end;

function TMeiStream.GetMemory(Index: Int64): PByte;
begin
    if Index >= Size then
        Index := Size - 1;
    Result := PByte(NativeInt(Memory) + Index);
end;

procedure TMeiStream.loadFromString(s: AnsiString);
begin
  if s = '' then
  begin
    SetSize(0);
    Exit;
  end;
  SetSize(Length(s));
  Move(s[1], memory^, Size);
  Seek(0, 0);
end;

procedure TMeiStream.Pop(Count: Longint);
begin
  SetSize(Size - Count);
  Move(SeekAddr(Count)^, Memory^, Size);
end;

function TMeiStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (Position + Count > Size) and CheckRange then
    raise EBufferStream.CreateFmt('cannot read %04X octets AT %04X.', [Count, Position]);
  Result := inherited Read(Buffer, Count);
end;

function TMeiStream.ReadAnsiString(ASize: Integer): string;
var
  a, b: TBytes;
  pb: PByteArray;
  reads: Integer;
begin
  reads := Size - Position;
  if (ASize < 0) or (ASize > reads) then
    ASize := reads;
  if (ASize <= 0) then
    Exit('');
  SetLength(a, ASize);
  Read(a[0], ASize);
  b := TEncoding.Convert(GameEncoding.ANSI, TEncoding.Default, a);
  Result := StringOf(b);

  SetLength(a, 0);
  SetLength(b, 0);
end;

function TMeiStream.ReadAnsiStringWithSize(SizeSize: Integer): string;
var size: Longint;
begin
  size := 0;
  Read(size, SizeSize);
  Result := ReadAnsiString(size);
end;

function TMeiStream.ReadBoolean: Boolean;
begin
  Result := False;
  Read(Result, 1);
end;

function TMeiStream.ReadByte: Byte;
begin
  Result := 0;
  Read(Result, SizeOf(Result));
end;

function TMeiStream.ReadDouble: Double;
begin
  Result := 0;
  Read(Result, SizeOf(Result));
end;

function TMeiStream.ReadHexText(isize: Integer): string;
begin
//  Result := BufToHexText(CurrentMem, isize);
//  Seek(isize, 1);
end;

function TMeiStream.ReadInt64: Int64;
begin
    Read(Result, SizeOf(Result));
end;

function TMeiStream.ReadInteger: Integer;
begin
  Result := 0;
  Read(Result, SizeOf(Result));
end;

function TMeiStream.ReadLong: Longword;
begin
  Result := 0;
  Read(Result, SizeOf(Result));
end;

function TMeiStream.ReadNativeUInt: NativeUInt;
begin
  Result := 0;
  Read(Result, SizeOf(Result));
end;

function TMeiStream.ReadPAnsiChar: string;
var
  pb: PByteArray;
  i: Integer;
begin
  pb := CurrentMem;
  i := 0;
  for i := 0 to Size - 1 - Position do
    if pb[i] = 0 then
      Break;
  if i = 0 then
    Result := ('')
  else
    Result := ReadAnsiString(i);
  Seek(1, 1);
end;

function TMeiStream.ReadPWideChar: String;
begin
    Result := PWideChar(CurrentMem);
    Seek(Length(Result) * 2 + 2, 1);
end;

function TMeiStream.ReadStream(AValue: TStream; ASize: Longint): Longint;
begin
  Result := Size - Position;
  if (ASize < Result) and (ASize >= 0) then Result := ASize;
  AValue.Write(SeekAddr(Position)^, Result);
  Seek(Result, soFromCurrent);
end;
function TMeiStream.ReadStringWithSize(SizeSize: Integer): String;
begin
  {$IFDEF unicode}
  Result := ReadWideStringWithSize(SizeSize)
  {$ELSE}
  Result := ReadAnsiStringWithSize(sizeSize);
  {$ENDIF}
end;

{
function TMeiStream.ReadWideString(ASize: Integer; encoding: TWideFormat): string;
var reads: Integer;
begin
  reads := Size - Position;
  if (ASize < 0) or (ASize > reads) then
    ASize := reads;
  Result := encoding.GetString(SeekAddr(Position), ASize);
  Seek(ASize, soFromCurrent);
end;

function TMeiStream.ReadWideString(ASize: Integer; encoding: string): string;
begin
  Result := ReadWideString(ASize, GetWideFormat(encoding, false));
end;

function TMeiStream.ReadWideString(ASize: Integer): string;
begin
  Result := ReadWideString(ASize, UTF16WideForamt);
end;

function TMeiStream.ReadWideStringWithLength(encoding: TWideFormat; SizeSize: Integer): string;
var size: Integer;
begin
  size := 0;
  Read(size, SizeSize);
  Result := ReadWideString(size * 2, encoding);
end;

function TMeiStream.ReadWideStringWithLength(encoding: string; SizeSize: Integer): string;
begin
  Result := ReadWideStringWithLength(encoding, SizeSize);
end;

function TMeiStream.ReadWideStringWithSize(encoding: TWideFormat; SizeSize: Integer): string;
var count: Integer;
begin
  count := 0;
  Read(count, SizeSize);
  Result := ReadWideString(count, encoding);
end;

function TMeiStream.ReadWideStringWithSize(encoding: string; SizeSize: Integer): string;
begin
  Result := ReadWideStringWithSize(encoding, SizeSize);
end;
}
function TMeiStream.ReadWideString(ASize: Integer): string;
var reads: Integer;
begin
  reads := Size - Position;
  if (ASize < 0) or (ASize > reads) then
    ASize := reads div 2;
  SetLength(Result, ASize);
  Read(Pointer(Result)^, ASize * 2);
end;

function TMeiStream.ReadWideStringWithSize(SizeSize: Integer): String;
var size: Longint;
begin
  size := 0;
  Read(size, SizeSize);
  Result := ReadWideString(size);
end;

function TMeiStream.ReadWord: Word;
begin
  Result := 0;
  Read(Result, SizeOf(Result));
end;

function TMeiStream.SeekAddr(APos: LongInt): Pointer;
begin
  if Size <= 0 then
    Result := nil
  else
    Result := Pointer(Longint(Memory) + APos);
end;

procedure TMeiStream.SetMemory(buf: Pointer; size: Int64);
begin
    SetPointer(buf, size);
end;

procedure TMeiStream.SetSize(NewSize: LongInt);
begin
  if (NewSize > Capacity) or ((Capacity > BufferSize) and (NewSize < BufferSize)) then
    inherited SetSize(NewSize)
  else begin
    SetPointer(Memory, NewSize);
    if Position > NewSize then Seek(0, soFromEnd);
  end;
end;
{$IFDEF swserver}
procedure TMeiStream.WriteAnsiString(AValue: AnsiString);
begin
  if AValue = '' then
    Exit;

  Write(avalue[1], Length(AValue));
end;

procedure TMeiStream.WriteAnsiStringWithSize(AValue: AnsiString; SizeSize: Integer);
var
  i: Integer;
begin
  i := Length(AValue);

  Write(i, SizeSize);
  if i = 0 then
    Exit;
  Write(avalue[1], Length(AValue));
end;
{$ELSE}
procedure TMeiStream.WriteAnsiString(AValue: string);
var
  a, b: TBytes;
  pb: PByteArray;
begin
  a := BytesOf(AValue);
  b := TEncoding.Convert(TEncoding.Default, GameEncoding.ANSI, a);
  if Length(b) > 0 then
    Write(b[0], Length(b));
  SetLength(a, 0);
  SetLength(b, 0);
end;

procedure TMeiStream.WriteAnsiStringWithSize(AValue: string; SizeSize: Integer);
var
  a, b: TBytes;
  pb: PByteArray;
  i: Integer;
begin
  a := BytesOf(AValue);
  b := TEncoding.Convert(TEncoding.Default, GameEncoding.ANSI, a);
  i := Length(b);

  Write(i, SizeSize);
  if i > 0 then
    Write(b[0], i);
  SetLength(a, 0);
  SetLength(b, 0);
end;
{$ENDIF}

procedure TMeiStream.WriteBoolean(AValue: Boolean);
begin
  Write(AValue, SizeOf(AValue));
end;

procedure TMeiStream.WriteByte(AValue: Byte);
begin
    Write(AValue, SizeOf(AValue));
end;

procedure TMeiStream.writeCnt(p0: Int64; cnt: Integer);
begin
  Position := p0;
  Write(cnt, SizeOf(cnt));
  Position := Size;
end;

procedure TMeiStream.WriteDouble(AValue: Double);
begin
    Write(AValue, SizeOf(AValue));
end;

procedure TMeiStream.WriteInt64(AValue: Int64);
begin
    Write(AValue, SizeOf(AValue));
end;

procedure TMeiStream.WriteInteger(AValue: Integer);
begin
    Write(AValue, SizeOf(AValue));
end;

procedure TMeiStream.WriteLong(AValue: Longword);
begin
    Write(AValue, SizeOf(AValue));
end;

procedure TMeiStream.WriteNativeUInt(AValue: NativeUInt);
begin
  Write(AValue, SizeOf(AValue));
end;

procedure TMeiStream.WritePAnsiChar(AValue: PAnsiChar);
var
  len: Integer;
begin
  len := StrLen(AValue);
  Write(AValue^, len + 1);
end;

procedure TMeiStream.WritePAnsiChar(AValue: string);
var
  m: TMarshaller;
  pc: PAnsiChar;
begin
  pc := m.AsAnsi(AValue).ToPointer;
  Write(pc^, StrLen(pc) + 1);
end;

procedure TMeiStream.WritePWideChar(AValue: string);
var
  len: Integer;
begin
  len := AValue.Length;
  if len > 0 then
    Write(AValue[1], len);
  WriteWord(len);
end;

procedure TMeiStream.WritePWideChar(AValue: PWideChar);
var
  len: Integer;
begin
  len := StrLen(AValue);
  Write(AValue^, len * 2 + 2);
end;

procedure TMeiStream.WriteSingle(AValue: Single);
begin
    Write(AValue, SizeOf(AValue));
end;

procedure TMeiStream.WriteStream(AValue: TStream;
    ReadPosition: Int64 = -1; ReadSize: int64 = -1);
var
    last, size: Integer;
begin
    if ReadPosition >= 0 then
        AValue.Position := ReadPosition;

    size := AValue.Size - AValue.Position;

    Write(nil^, 0);
    last := Position;
    if (ReadSize >= 0) and (ReadSize <= size) then size := ReadSize;
    if GetSize - last < size then
      SetSize(last + size);
    AValue.ReadBuffer(SeekAddr( last )^, size);
    Seek(size, soFromCurrent);
end;
procedure TMeiStream.WriteStringWithSize(AValue: string; SizeSize: Integer);
begin
  {$IFDEF unicode}
  WriteWideStringWithSize(AValue, SizeSize);
  {$ELSE}
  WriteAnsiStringWithSize(AValue, SizeSize);
  {$ENDIF}
end;

{
procedure TMeiStream.WriteWideString(AValue: string; encoding: TWideFormat);
var last, size: Integer;
begin
  Write(nil^, 0);
  last := Position;
  size := encoding.GetByteCount(AValue);
  if GetSize - last < size then
    SetSize(last + size);
  encoding.GetBytes(AValue, SeekAddr( last ), size);
  Seek(size, soFromCurrent);
end;

procedure TMeiStream.WriteWideString(AValue, encoding: string);
begin
  WriteWideString(AValue, GetWideFormat(encoding, False));
end;

procedure TMeiStream.WriteWideStringWithLength(AValue: string; encoding: TWideFormat; SizeSize: Integer);
var size: Integer;
begin
  size := Length(AValue);
  Write(size, SizeSize);
  WriteWideString(AValue, encoding);
end;

procedure TMeiStream.WriteWideStringWithLength(AValue, encoding: string; SizeSize: Integer);
begin
  WriteWideStringWithLength(AValue, GetWideFormat(encoding, false), SizeSize);
end;

procedure TMeiStream.WriteWideStringWithSize(AValue: string; encoding: TWideFormat; SizeSize: Integer);
var size: Integer;
begin
  size := encoding.GetByteCount(AValue);
  Write(size, SizeSize);
  WriteWideString(AValue, encoding);
end;

procedure TMeiStream.WriteWideStringWithSize(AValue, encoding: string; SizeSize: Integer);
begin
  WriteWideStringWithSize(AValue, GetWideFormat(encoding, false), SizeSize);
end;
}
procedure TMeiStream.WriteWideString(AValue: String);
begin
  Write(Pointer(AValue)^, Length(AValue) * SizeOf(WideChar));
end;

procedure TMeiStream.WriteWideStringWithSize(AValue: String;
  SizeSize: Integer);
var Size: Longint;
begin
  Size := Length(AValue);
  Write(Size, SizeSize);
  WriteWideString(AValue);

end;

procedure TMeiStream.WriteWord(AValue: Word);
begin
  Write(AValue, SizeOf(AValue));
end;

{ TPacketStream }

function TPacketStream.Write(const Buffer; Count: Longint): Longint;
begin
  Seek(0, soFromEnd);
  Result := inherited Write(Buffer, Count);
end;

initialization
  {$IFDEF TW}
  GameEncoding := TMBCSEncoding.Create(950, 0, 0);
  {$ELSE}
  GameEncoding := TMBCSEncoding.Create(932, 0, 0);
  {$ENDIF}
finalization
  GameEncoding.Free;

end.
