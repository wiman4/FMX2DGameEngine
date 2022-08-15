unit pubUnit;

interface

uses
  {$IFDEF gameclient}objUnit, MeiPack,{$ENDIF}
  System.Types, FMX.Graphics, FMX.Types, System.UITypes;

const
  GamePetLianhuaGoldCount = 5000;
{$IF defined(android) or defined(ios)}
  ScreenWidth = 1024;
  ScreenHeight = 576;
{$ELSE}
  ScreenWidth = 1600;
  ScreenHeight = 900;
{$ENDIF}
  ScreenSlotSize = 20.0;

var
  GameServerLevel, GameMaxLevel: Byte;

  AppPath, AppTablesPath, AppMapPath: string;

  GameTimeUnix, GameTimeUnixOff, GameTimeNextShichen: Int64;
  GameTimeShichen: Integer;

  {$IFDEF gameclient}
  gamesock: TGameSock;
  gameAvatar: TGameAvatar;
  gameObjList: TGameObjList;
  mapPoint0: TPointF;

  gameRes: TMeiPack;
  {$ENDIF}

  function GetGameTime: Int64;
  function getGoldString(gold: Integer): string;
  function splitStr(subs: string; var s: String): String;
  function TrimStr(s: string): string;
  function ScanChinese(var s: string): string;
  function ScanInt(var s: string): Integer;


  function myBoundsF(x, y, w, h: Single): TRectF;
  function InflateRectF(var rt: TRectF; dx, dy: Single): TRectF;
  procedure drawBitmap(can: TCanvas; bmp: TBitmap; x, y, w, h: Single);

  procedure initAppPath;

implementation

uses
  System.SysUtils, System.IOUtils, System.DateUtils;

function GetGameTime: Int64;
begin
  Result := DateTimeToUnix(Now) + GameTimeUnixOff;
end;

function getGoldString(gold: Integer): string;
var
  len, i: Integer;
begin
  Result := gold.ToString();
  len := Result.Length - 1;
  for i := 0 to len - 1 do
  begin
    if i mod 4 = 3 then
    begin
      Result := Result.Insert(len - i, ',');
    end;
  end;
end;

function myBoundsF(x, y, w, h: Single): TRectF;
begin
  Result.Top := y; Result.Left := x;
  Result.Width := w; Result.Height := h;
end;

function InflateRectF(var rt: TRectF; dx, dy: Single): TRectF;
begin
  Result := rt;
  Result.Inflate(dx, dy);
end;

procedure drawBitmap(can: TCanvas; bmp: TBitmap; x, y, w, h: Single);
begin
  can.BeginScene();
  can.DrawBitmap(bmp, bmp.BoundsF, myBoundsF(x, y, w, h), 1);
  can.Font.Size := 22;
  can.Font.Style := [TFontStyle.fsBold];
  can.Fill.Color := TAlphaColorRec.Black;
  can.FillText(myBoundsF(x, y + 40, w - 3, 30), '99', False, 1, [], TTextAlign.Trailing);
  can.Font.Style := [];
  can.Fill.Color := TAlphaColorRec.White;
  can.FillText(myBoundsF(x, y + 40, w - 3, 30), '99', False, 1, [], TTextAlign.Trailing);
  can.EndScene;
end;

procedure initAppPath;
begin
  {$ifdef MSWINDOWS}
  AppPath := ExtractFilePath(ParamStr(0));
  AppTablesPath := AppPath + 'tables\';
  AppMapPath := AppPath + 'map\';
  {$ENDIF}
  {$ifdef android}
  AppPath := TPath.GetDocumentsPath + '/';
  AppTablesPath := AppPath + '/tables/';
  AppMapPath := AppPath + '/map/';
  {$ENDIF}
end;

function splitStr(subs: string; var s: String): String;
var
  p: integer;
begin
  p := s.IndexOf(subs);
  if p < 0 then
  begin
    Result := s;
    s := '';
    Exit;
  end;
  result := s.Substring(0, p);
  s := s.Substring(p + Length(subs));
end;

function TrimStr(s: string): string;
var
  i, idx, len: Integer;
begin
  len := Length(s);
  SetLength(Result, len);
  idx := Low(s);
  for i := idx to High(s) do
    if s[i] > ' ' then
    begin
      Result[idx] := s[i];
      Inc(idx);
    end;

  {$IFDEF win32}
  Delete(Result, idx, len)
  {$ELSE}
  Result := Result.Remove(idx, len);
  {$ENDIF}
end;

function ScanInt(var s: string): Integer;
var
  i, code: Integer;
begin
  Result := 0;
  if s = '' then
    Exit;
  for i := Low(s) to High(s) do
    if Pos(s[i], '0123456789') >= Low(s) then
      Break;
  if (i > 1) and (Pos(s[i-1], '$+-') > 0) then
    Dec(i);

  if i > 1 then
    Delete(s, 1, i - 1);
  Val(s, Result, code);
  if code > 0 then
    Delete(s, 1, code - 1)
  else
    Delete(s, 1, Length(s));
end;

function ScanFloat(var s: string): Single;
begin
  Result := 0.0;
end;

function ScanChinese(var s: string): string;
var
  i: Integer;
begin
  Result := '';
  if s = '' then
    Exit;
{$IFDEF swserver}
  for i := 1 to Length(s) do
    if s[i] > #127 then
      Break;

  if i > 1 then
    Delete(s, 1, i - 1);

  for i := 1 to Length(s) do
    if s[i] <= #127 then
      Break;

  Result := Copy(s, 1, i - 1);
  Delete(s, 1, i - 1);
{$ELSE}
  for i := 0 to Length(s) - 1 do
    if s.Chars[i] > #127 then
      Break;

  if i > 0 then
    s := s.Remove(0, i);

  for i := 0 to Length(s) - 1 do
    if s.Chars[i] <= #127 then
      Break;

  Result := s.Substring(0, i);
  s := s.Substring(i);
{$ENDIF}
end;

end.
