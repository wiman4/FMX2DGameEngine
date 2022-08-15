unit richTextUnit;

interface
uses
  System.SysUtils, System.Generics.Collections, UIUnit, System.Classes,
  System.UITypes;

procedure addGameText(rd: TUIRichView; s: string; bappend: Boolean = False;
  bscroll: Boolean = False; lineMax: Integer = 0);
procedure parseGameDataText(var s, text: string; var i: Integer);
procedure parseQuestText(rd: TUIRichView; var s: string; var idx: Integer;
  scr: string = '');

implementation

uses
  System.Math, pubUnit, TypeUnit, stringUnit;

var
  defFontSize: Integer = 24;

procedure parseGameText(rd: TUIRichView; var s, text: string; var i: Integer);
var
  k, v, code: Integer;
  scr, sname: string;
  guid: TGameGUID;
  clr: TAlphaColor;
begin
  for k := i to High(s) do
    if s.Chars[k] = ')' then
    begin
      sname := s.SubString(i + 2, k - (i + 2));
//      i := k;
      sname := TrimStr(sname);
      Val(sname, v, code);
      case v of
        16: text := getGoldString(StrToIntDef(sname.Substring(code), 0));
        13:
          begin
            splitStr(',', sname);
            scr := 'goto:' + sname;
            clr := rd.TextColor;
            rd.setFont(TAlphaColorRec.Aqua, 0, [TFontStyle.fsUnderline]);
            if Pos('$process', sname) > 0 then
              parseQuestText(rd, s, v, scr)
            else
              rd.addText(ScanChinese(sname), scr);
            rd.setFont(clr, 0, []);
            text := '';
          end
      else
//        text := getGameText(sname);
      end;
//      s := s.Remove(i+1, k - i);
//      s.Insert(i + 1, text);
      i := k;
      Exit;
    end;
end;

procedure parseGamePlayerName(var s: string; rd: TUIRichView; var i: Integer);
var
  k, v, code: Integer;
  sname: string;
  guid: TGameGUID;
begin
  for k := i to High(s) do
    if s.Chars[k] = ')' then
    begin
      sname := s.SubString(i + 2, k - (i + 2));
//      i := k;
      sname := TrimStr(sname);
      guid := StrToIntDef(splitStr(',', sname), 0);
      sname := splitStr(',', sname) + sColon;
      rd.addText(sname, Format('clickplayer:%d', [guid]));
      i := k + 1;
      Exit;
    end;
end;

procedure parseGameDataText(var s, text: string; var i: Integer);
var
  k: Integer;
  stype, sname: string;
begin
  for k := i to High(s) do
    if s.Chars[k] = ')' then
    begin
      sname := s.SubString(i + 2, k - (i + 2));
//      i := k;
      sname := TrimStr(sname);
      stype := splitStr(',', sname);
      if stype = '1' then
        text := 'skillId.' + sname ; //getSkillName(StrToIntDef(sname, 0), TSkillDataType.sdNone);
      i := k;
      Exit;
    end;
end;

procedure parseGameSkillText(var s, text: string; var i: Integer);
var
  k: Integer;
  stype, sname: string;
begin
  for k := i to High(s) do
    if s.Chars[k] = ')' then
    begin
      sname := s.SubString(i + 2, k - (i + 2));
      text := sname; //getSkillName(StrToIntDef(sname, 0), TSkillDataType.sdShimen);
      i := k;
      Exit;
    end;
end;

procedure parseQuestText(rd: TUIRichView; var s: string; var idx: Integer;
  scr: string = '');
var
  amt, k, i: Integer;
  pq: PGameQuest;
  clr: TAlphaColor;
begin
  clr := rd.TextColor;
//  for i := 0 to gameAvatar.FQuestCount - 1 do
//  begin
//    pq := @gameAvatar.FQuestList[i];
//    if (s <> pq._rwmb) and (s <> pq._rwhf) and (s <> pq._rwgl) and (s <> pq._rwms) then
//      Continue;
//
//    for k := 0 to High(pq._tgList) do
//      if pq._tgList[k]._id <> 0 then
//      begin
//        if pq._tgList[k]._id > 10000 then
//        begin
//          amt := gameAvatar.getItemCount(pq._tgList[k]._id);
//          if amt > pq._tgList[k]._amt then
//            amt := pq._tgList[k]._amt;
//          rd.setFont(TAlphaColorRec.Green);
//          rd.addText(getItemName(pq._tgList[k]._id), scr);
//          if amt < pq._tgList[k]._amt then
//            rd.setFont(TAlphaColorRec.Red);
//          if k = 0 then
//            rd.addText(Format('(%d/%d)', [amt, pq._tgList[k]._amt]))
//          else
//            rd.addText(Format(', (%d/%d)', [amt, pq._tgList[k]._amt]));
//        end else
//        begin
//
//        end;
//
//      end;
//    Break;
//  end;
  rd.setFont(clr);
  Inc(idx, 6);
end;

procedure addGameText(rd: TUIRichView; s: string; bappend: Boolean = False;
  bscroll: Boolean = False; lineMax: Integer = 0);
var
  opt, k, len, idx, i, cnt, lineCnt: Integer;
  text, scr: string;
  c: Char;
  ss: TStringList;
  clrflag: Boolean;

  procedure addText(rd: TUIRichView; var text: string; bappend, bscroll: Boolean);
  var
    b1, b2: string;
  begin
    if not bappend then
      rd.addNewLine;
    rd.addText(text);
    text := '';
  end;

begin
  rd.resetFont;
  if not bappend then
    rd.addNewLine;
  idx := 0; i := 0;
  lineCnt := lineMax;
  len := s.Length - 1;
  ss := TStringList.Create;
  opt := 0; clrflag := False;
  while i <= len do
  begin
    c := s.Chars[i];
    if c = '#' then
    begin
      text := s.SubString(idx, i - idx);
      if text <> '' then
        rd.addText(text, scr);
      Inc(i);
      case s.Chars[i] of
        '(': parseGamePlayerName(s, rd, i);
        'R': rd.setFont(TAlphaColorRec.Red);
        'G': rd.setFont(TAlphaColorRec.Green);
        'B': rd.setFont(TAlphaColorRec.Blue);
        'Y': rd.setFont(TAlphaColorRec.Yellow);
        'n':
          begin
            if opt > 0 then
              rd.setFont(TAlphaColorRec.Aqua)
            else
              rd.resetFont;
          end;
        'c':
          begin
            if len >= i + 7 then
            begin
              rd.setFont($FF000000 + StrToUIntDef('$' + s.SubString(i + 1, 6), TAlphaColorRec.White));
              Inc(i, 6);
            end;
          end;
        'r':
          begin
            rd.addNewLine;
            lineCnt := lineMax;
          end;
        's':
          begin
            parseGameText(rd, s, text, i);
            rd.addText(text, scr);
            len := High(s);
          end;
        'm':
          begin
            parseGameDataText(s, text, i);
            rd.addText(text, scr);
//            len := High(s);
          end;
        'P':
          begin
            rd.setFont(TAlphaColorRec.Aqua);
            parseGameSkillText(s, text, i);
            rd.addText(text, scr);
          end;
      end;
      idx := i + 1;
    end else
    if c = '$' then
    begin
      text := s.SubString(idx, i - idx);
      rd.addText(text, scr);

      Inc(i);
      case s.Chars[i] of
        'N':
          begin
            rd.addText('gameAvatar.FNameString');
          end;
        'p':
          begin
            parseQuestText(rd, s, i, scr);
          end;
      end;
      idx := i + 1;
    end else
    if c = #10 then
    begin
      text := s.SubString(idx, i - idx);
      rd.addText(text, scr);

      if (i + 1 <= len) and (s.Chars[i+1] = 'Q') then
      begin
        rd.addNewLine;
        rd.addNewLine;
      end;

      lineCnt := lineMax;
      idx := i + 1;

      if s.Chars[idx] = 'Q' then
      begin
        rd.setFont(TAlphaColorRec.Aqua);
        Inc(opt);
        scr := Format('npctalk:%d', [opt]);
        Inc(idx);
      end;

    end else
      Dec(lineCnt);

    if (lineMax > 0) and (lineCnt <= 0) then
    begin
      text := s.SubString(idx, i - idx + 1);
      rd.addText(text, scr);
      rd.addNewLine;
      idx := i + 1;
      lineCnt := lineMax;
    end;

    Inc(i);

  end;

//  redt.Font.Size := 22;
  s := s.SubString(idx, len - idx + 1);
  if (s <> '') then
  begin
//    if opt > 0 then
//      rd.addText(s, Format('npctalk:%d', [opt]))
//    else
      rd.addText(s, scr);
  end;
//  for k := 0 to ss.Count - 1 do
//    text := text + ss[k];
//  rd.addNewLine;
//  addText(rd, text, bappend, bscroll);
//  rd.paint;

  ss.Free;
end;

end.
